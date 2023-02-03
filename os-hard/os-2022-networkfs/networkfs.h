#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/stat.h>

// NETWORKFS FUNCTIONS DEFINITIONS

struct inode *networkfs_get_inode(struct super_block *, const struct inode *,
                                  umode_t, int);

int networkfs_create(struct user_namespace *, struct inode *, struct dentry *,
                     umode_t, bool);

struct dentry *networkfs_lookup(struct inode *, struct dentry *, unsigned int);

int networkfs_unlink(struct inode *, struct dentry *);

int networkfs_mkdir(struct user_namespace *, struct inode *, struct dentry *,
                    umode_t);
int networkfs_rmdir(struct inode *, struct dentry *);

int networkfs_iterate(struct file *, struct dir_context *);

ssize_t networkfs_read(struct file *, char *, size_t, loff_t *);

ssize_t networkfs_write(struct file *, const char *, size_t, loff_t *);

// NETWORKFS STRUCTS

extern struct file_operations networkfs_dir_ops;
extern struct file_operations networkfs_file_operations;
extern struct inode_operations networkfs_inode_ops;

struct networkfs_entries {
  size_t entries_count;
  struct entry {
    unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
    ino_t ino;
    char name[256];
  } entries[16];
};

struct networkfs_entry_info {
  unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
  ino_t ino;
};

struct content {
  u64 content_length;
  char content[512];
};

// NETWORKFS HELPERS

#define NETWORKFS_INFO(fmt, ...) \
  printk(KERN_INFO "networkfs: " fmt "\n", __VA_ARGS__);

#define NETWORKFS_ERR(fmt, ...) printk(KERN_ERR "networkfs: " fmt, __VA_ARGS__);

#define UNSIGNED_TO_STR(n, buf)           \
  char buf[20] = {0};                     \
  do {                                    \
    unsigned long long num = n;           \
    size_t i = 0;                         \
    while (num != 0 && (++i)) {           \
      buf[i] = '0' + num % 10;            \
      num /= 10;                          \
    }                                     \
    for (size_t j = 0; j <= i / 2; j++) { \
      char tmp = buf[j];                  \
      buf[j] = buf[i - j];                \
      buf[i - j] = tmp;                   \
    }                                     \
  } while (0);

#define NETWORKFS_CALLA(sb, method, response_struct, arg_size, ...) \
  response_struct response;                                         \
  int64_t status =                                                  \
      networkfs_http_call(get_token(sb), method, (char *)&response, \
                          sizeof(response_struct), arg_size, __VA_ARGS__);

#define NETWORKFS_CALLE(sb, method, arg_size, ...)                     \
  int64_t status = networkfs_http_call(get_token(sb), method, NULL, 0, \
                                       arg_size, __VA_ARGS__);

#define NETWORKFS_CALL(sb, method, response_ptr, arg_size, ...)        \
  int64_t status =                                                     \
      networkfs_http_call(get_token(sb), method, (char *)response_ptr, \
                          sizeof(*response_ptr), arg_size, __VA_ARGS__);
