#include <linux/slab.h>

#include "http.h"
#include "networkfs.h"

MODULE_LICENSE("GPL");
MODULE_AUTHOR("letstatt");
MODULE_VERSION("0.01");

struct file_operations networkfs_dir_ops = {
    .owner = THIS_MODULE,
    .iterate = networkfs_iterate,
};

struct file_operations networkfs_file_operations = {
    .read = networkfs_read,
    .write = networkfs_write,
};

struct inode_operations networkfs_inode_ops = {
    .create = networkfs_create,
    .lookup = networkfs_lookup,
    .unlink = networkfs_unlink,
    .mkdir = networkfs_mkdir,
    .rmdir = networkfs_rmdir,
};

const char *get_token(struct super_block *sb) { return sb->s_fs_info; }

// Returns non-zero if failed
int save_token(struct super_block *sb, const char *src) {
  if ((sb->s_fs_info = kzalloc(36, GFP_KERNEL)) == NULL) {
    return -1;
  }
  strncpy(sb->s_fs_info, src, 36);
  return 0;
}

void free_token(struct super_block *sb) {
  if (sb->s_fs_info != NULL) {
    kfree(sb->s_fs_info);
  } else {
    printk(KERN_WARNING "networkfs free_token: tried to free null pointer");
  }
}

int networkfs_iterate(struct file *filp, struct dir_context *ctx) {
  unsigned long offset;
  int stored;
  ino_t ino;

  offset = ctx->pos;
  stored = 0;
  ino = filp->f_path.dentry->d_inode->i_ino;

  UNSIGNED_TO_STR(ino, numToStr);
  NETWORKFS_INFO("iterate ENTERING, inode: %s", numToStr);

  // allocate on the heap, because sizeof(networkfs_entries) > stack frame size
  struct networkfs_entries *entries;

  if ((entries = (struct networkfs_entries *)kmalloc(
           sizeof(struct networkfs_entries), GFP_KERNEL)) == NULL) {
    NETWORKFS_INFO("iterate failed: %s", "not enough memory");
    return -ENOMEM;
  }

  NETWORKFS_CALL(filp->f_inode->i_sb, "list", entries, 1, "inode", numToStr);
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 3

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      kfree(entries);
      return -ENOENT;
    case 3:
      kfree(entries);
      return -ENOTDIR;
    default:
      kfree(entries);
      return status;
  }

  NETWORKFS_INFO("entries_count: %lu", entries->entries_count);

  while (offset < entries->entries_count) {
    struct entry *e = &entries->entries[offset];
    NETWORKFS_INFO("entry_type: %d", e->entry_type);
    NETWORKFS_INFO("ino: %lu", e->ino);
    NETWORKFS_INFO("name: %s", e->name);

    offset++;
    stored++;
    ctx->pos++;
    dir_emit(ctx, e->name, strlen(e->name), e->ino, e->entry_type);
  }
  kfree(entries);
  return stored;
}

ssize_t networkfs_read(struct file *filp, char *buffer, size_t len,
                       loff_t *offset) {
  ino_t ino;
  int bytes_read;

  bytes_read = 0;
  ino = filp->f_path.dentry->d_inode->i_ino;

  UNSIGNED_TO_STR(ino, numToStr);
  NETWORKFS_INFO("read ENTERING, inode: %s", numToStr);

  // allocate on the heap just in case
  struct content *content;

  if ((content = (struct content *)kzalloc(sizeof(struct content),
                                           GFP_KERNEL)) == NULL) {
    NETWORKFS_INFO("read failed: %s", "not enough memory");
    return -ENOMEM;
  }

  NETWORKFS_CALL(filp->f_inode->i_sb, "read", content, 1, "inode", numToStr);
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 2

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      kfree(content);
      return -ENOENT;
    case 2:
      kfree(content);
      return -EISDIR;
    default:
      kfree(content);
      return status;
  }

  if (*offset > content->content_length) {
    kfree(content);
    return bytes_read;
  }

  // copy out
  while (len > 0 && *offset < content->content_length) {
    if (put_user(content->content[*offset], buffer++) != 0) {
      break;
    }
    len--;
    bytes_read++;
    (*offset)++;
  }
  kfree(content);
  NETWORKFS_INFO("read %d bytes", bytes_read);
  return bytes_read;
}

ssize_t networkfs_write(struct file *filp, const char *buffer, size_t len,
                        loff_t *offset) {
  ino_t ino;
  int bytes_written;

  bytes_written = 0;
  ino = filp->f_path.dentry->d_inode->i_ino;

  UNSIGNED_TO_STR(ino, numToStr);
  NETWORKFS_INFO("write ENTERING, inode: %s, len: %lu", numToStr, len);

  if (*offset != 0) {
    NETWORKFS_INFO("write, unsupported offset: %lld", *offset);
    return -EINVAL;
  }

  // allocate on the heap just in case
  char *content;

  if ((content = (char *)kzalloc(3 * len + 1, GFP_KERNEL)) == NULL) {
    NETWORKFS_INFO("write failed: %s", "not enough memory");
    return -ENOMEM;
  }

  // copy in
  while (len > 0) {
    char c;
    if (get_user(c, buffer++) != 0) {
      kfree(content);
      return -EFAULT;
    }
    char_urlencode(content + strlen(content), c);
    len--;
    bytes_written++;
    (*offset)++;
  }

  NETWORKFS_CALLE(filp->f_inode->i_sb, "write", 2, "inode", numToStr, "content",
                  content);
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 2, 6
  kfree(content);

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      return -ENOENT;
    case 2:
      return -EISDIR;
    case 6:
      return -EFBIG;
    default:
      return status;
  }

  NETWORKFS_INFO("wrote %d bytes", bytes_written);
  return bytes_written;
}

int networkfs_create(struct user_namespace *ns, struct inode *parent_inode,
                     struct dentry *child_dentry, umode_t mode, bool b) {
  struct inode *inode;
  const char *name;
  ino_t root;

  name = child_dentry->d_name.name;
  root = parent_inode->i_ino;

  UNSIGNED_TO_STR(root, numToStr);
  NETWORKFS_INFO("create ENTERING, inode: %s", numToStr);

  NETWORKFS_CALLA(parent_inode->i_sb, "create", ino_t, 3, "parent", numToStr,
                  "name", name, "type", "file");
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 3, 5, 7

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      return -ENOENT;
    case 3:
      return -ENOTDIR;
    case 5:
      return -EEXIST;
    case 7:
      return -ENOSPC;  // == directory elements count exceeded
    default:
      return status;
  }

  NETWORKFS_INFO("inode: %lu", response);

  if ((inode = networkfs_get_inode(parent_inode->i_sb, NULL,
                                   S_IFREG | S_IRWXUGO, response)) == NULL) {
    return -ENOMEM;
  }
  d_instantiate(child_dentry, inode);
  return 0;
}

struct dentry *networkfs_lookup(struct inode *parent_inode,
                                struct dentry *child_dentry,
                                unsigned int flag) {
  struct inode *inode;
  const char *name;
  ino_t root;

  name = child_dentry->d_name.name;
  root = parent_inode->i_ino;

  // TODO: searching without leak

  UNSIGNED_TO_STR(root, numToStr);
  NETWORKFS_INFO("lookup ENTERING, inode: %s", numToStr);

  NETWORKFS_CALLA(parent_inode->i_sb, "lookup", struct networkfs_entry_info, 2,
                  "parent", numToStr, "name", name);
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 3, 4

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
    case 3:
    case 4:
      d_add(child_dentry, NULL);
      return 0;
    default:
      return ERR_PTR(-status);
  }

  NETWORKFS_INFO("entry_type: %d", response.entry_type);
  NETWORKFS_INFO("ino: %lu", response.ino);

  umode_t etype = S_IRWXUGO;
  switch (response.entry_type) {
    case DT_DIR:
      etype |= S_IFDIR;
      break;
    case DT_REG:
      etype |= S_IFREG;
      break;
  }

  if ((inode = networkfs_get_inode(parent_inode->i_sb, NULL, etype,
                                   response.ino)) == NULL) {
    return ERR_PTR(ENOMEM);
  }
  d_add(child_dentry, inode);
  atomic_inc(&inode->i_count);
  return NULL;
}

int networkfs_unlink(struct inode *parent_inode, struct dentry *child_dentry) {
  const char *name;
  ino_t root;

  name = child_dentry->d_name.name;
  root = parent_inode->i_ino;

  UNSIGNED_TO_STR(root, numToStr);
  NETWORKFS_INFO("unlink ENTERING, inode: %s", numToStr);

  NETWORKFS_CALLE(parent_inode->i_sb, "unlink", 2, "parent", numToStr, "name",
                  name);
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 2, 3, 4

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      return -ENOENT;
    case 2:
      return -EISDIR;
    case 3:
      return -ENOTDIR;
    case 4:
      return -ENOENT;
    default:
      return status;
  }

  NETWORKFS_INFO("%s", "inode deleted");
  return 0;
}

int networkfs_mkdir(struct user_namespace *ns, struct inode *parent_inode,
                    struct dentry *child_dentry, umode_t mode) {
  struct inode *inode;
  const char *name;
  ino_t root;

  name = child_dentry->d_name.name;
  root = parent_inode->i_ino;

  UNSIGNED_TO_STR(root, numToStr);
  NETWORKFS_INFO("mkdir ENTERING, inode: %s", numToStr);

  NETWORKFS_CALLA(parent_inode->i_sb, "create", ino_t, 3, "parent", numToStr,
                  "name", name, "type", "directory");
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 3, 5, 7

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      return -ENOENT;
    case 3:
      return -ENOTDIR;
    case 5:
      return -EEXIST;
    case 7:
      return -ENOSPC;  // == directory elements count exceeded
    default:
      return status;
  }

  NETWORKFS_INFO("ino out: %lu", response);

  if ((inode = networkfs_get_inode(parent_inode->i_sb, NULL,
                                   S_IFDIR | S_IRWXUGO, response)) == NULL) {
    return -ENOMEM;
  }
  d_instantiate(child_dentry, inode);
  return 0;
}

int networkfs_rmdir(struct inode *parent_inode, struct dentry *child_dentry) {
  const char *name;
  ino_t root;

  name = child_dentry->d_name.name;
  root = parent_inode->i_ino;

  UNSIGNED_TO_STR(root, numToStr);
  NETWORKFS_INFO("rmdir ENTERING, inode: %s", numToStr);

  NETWORKFS_CALLE(parent_inode->i_sb, "rmdir", 2, "parent", numToStr, "name",
                  name);
  NETWORKFS_INFO("status: %lld", status);  // 0, 1, 3, 4, 8

  // error handling
  switch (status) {
    case 0:
      break;
    case 1:
      return -ENOENT;
    case 3:
      return -ENOTDIR;
    case 4:
      return -ENOENT;
    case 8:
      return -EPERM;
    default:
      return status;
  }

  NETWORKFS_INFO("%s", "directory deleted");
  return 0;
}

struct inode *networkfs_get_inode(struct super_block *sb,
                                  const struct inode *dir, umode_t mode,
                                  int i_ino) {
  struct inode *inode;

  if ((inode = new_inode(sb)) == NULL) {
    return NULL;
  }

  inode->i_ino = i_ino;
  inode->i_op = &networkfs_inode_ops;

  if (mode & S_IFDIR) {
    inode->i_fop = &networkfs_dir_ops;
  } else {
    inode->i_fop = &networkfs_file_operations;
  }

  inode_init_owner(&init_user_ns, inode, dir, mode);
  return inode;
}

int networkfs_fill_super(struct super_block *sb, void *token, int silent) {
  struct inode *inode;

  if (save_token(sb, (char *)token) != 0) {
    return -ENOMEM;
  }

  if ((inode = networkfs_get_inode(sb, NULL, S_IFDIR | S_IRWXUGO, 1000)) ==
      NULL) {
    return -ENOMEM;
  }

  if ((sb->s_root = d_make_root(inode)) == NULL) {
    return -ENOMEM;
  }

  inode->i_fop = &networkfs_dir_ops;
  return 0;
}

struct dentry *networkfs_mount(struct file_system_type *fs_type, int flags,
                               const char *token, void *data) {
  struct dentry *ret;

  if ((ret = mount_nodev(fs_type, flags, (void *)token,
                         networkfs_fill_super)) == NULL) {
    NETWORKFS_ERR("%s", "Can't mount file system");
  } else {
    NETWORKFS_INFO("%s", "Mounted successfuly");
  }
  return ret;
}

void networkfs_kill_sb(struct super_block *sb) {
  const char *token = get_token(sb);
  if (token != NULL) {
    NETWORKFS_INFO("token %s", token);
  }
  free_token(sb);
  NETWORKFS_INFO("%s", "super block is destroyed. Unmount successfully");
}

static struct file_system_type networkfs_fs_type = {
    .name = "networkfs",
    .mount = networkfs_mount,
    .kill_sb = networkfs_kill_sb,
    .owner = THIS_MODULE,
    .next = NULL};

int networkfs_init(void) {
  NETWORKFS_INFO("%s", "Hello, World!");

  if (register_filesystem(&networkfs_fs_type) != 0) {
    NETWORKFS_ERR("%s", "filesystem registering failed");
    return 1;
  }

  return 0;
}

void networkfs_exit(void) {
  NETWORKFS_INFO("%s", "Goodbye!");
  if (unregister_filesystem(&networkfs_fs_type) != 0) {
    NETWORKFS_ERR("%s", "filesystem unregistering failed");
  }
}

module_init(networkfs_init);
module_exit(networkfs_exit);
