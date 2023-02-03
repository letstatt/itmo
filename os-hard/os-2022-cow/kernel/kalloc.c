// Physical memory allocator, for user processes,
// kernel stacks, page-table pages,
// and pipe buffers. Allocates whole 4096-byte pages.

#include "types.h"
#include "param.h"
#include "memlayout.h"
#include "spinlock.h"
#include "riscv.h"
#include "defs.h"

void freerange(void *pa_start, void *pa_end);

extern char end[];  // first address after kernel.
                    // defined by kernel.ld.

#define REFS_CNT ((PHYSTOP - (uint64)end) / PGSIZE)
#define REF_POS(pa) (((uint64)pa - (uint64)end) / PGSIZE)

struct run {
  struct run *next;
};

struct {
  struct spinlock lock;
  struct run *freelist;
  uint *refs;
} kmem;

void kinit() {
  initlock(&kmem.lock, "kmem");
  kmem.refs = (uint *)end;
  kmem.freelist = 0;
  freerange(end + REFS_CNT * sizeof(uint), (void *)PHYSTOP);
}

void freerange(void *pa_start, void *pa_end) {
  char *p;
  p = (char *)PGROUNDUP((uint64)pa_start);
  for (; p + PGSIZE <= (char *)pa_end; p += PGSIZE) {
    kmem.refs[REF_POS(p)] = 1;  // allow to free properly
    kfree(p);
  }
}

// Free the page of physical memory pointed at by pa,
// which normally should have been returned by a
// call to kalloc().  (The exception is when
// initializing the allocator; see kinit above.)
void kfree(void *pa) {
  struct run *r = (struct run *)pa;

  if (((uint64)pa % PGSIZE) != 0 || (char *)pa < end || (uint64)pa >= PHYSTOP)
    panic("kfree");

  acquire(&kmem.lock);

  // exclusive?
  if (--kmem.refs[REF_POS(pa)] == 0) {
    release(&kmem.lock);
    memset(pa, 1, PGSIZE);  // fill with junk

    acquire(&kmem.lock);
    r->next = kmem.freelist;
    kmem.freelist = r;
  }
  release(&kmem.lock);
}

// Allocate one 4096-byte page of physical memory.
// Returns a pointer that the kernel can use.
// Returns 0 if the memory cannot be allocated.
void *kalloc(void) {
  struct run *r;

  acquire(&kmem.lock);
  r = kmem.freelist;
  if (r) {
    kmem.freelist = r->next;
    kmem.refs[REF_POS(r)] = 1;
  }
  release(&kmem.lock);

  if (r) memset((char *)r, 5, PGSIZE);  // fill with junk
  return (void *)r;
}

// Copies memory to new page if given page is shared.
// Returns new page or `pa` if already exclusive.
// Never frees `pa`.
void *kcopy(void *pa) {
  void *mem;
  int refs;

  if (((uint64)pa % PGSIZE) != 0) panic("kcopy");

  acquire(&kmem.lock);
  if ((refs = kmem.refs[REF_POS(pa)]) == 1) {
    release(&kmem.lock);
    return pa;  // exclusive page, no copy needed

  } else if (refs == 0) {
    panic("kcopy, counter is zero");
  }
  release(&kmem.lock);

  // ref counter is never zero, because at least we hold it.
  // so copying below is safe.

  if ((mem = kalloc()) == 0) {
    return 0;
  }
  memmove(mem, pa, PGSIZE);

  acquire(&kmem.lock);
  if ((refs = kmem.refs[REF_POS(pa)]) == 1) {
    // did source page become exclusive meanwhile?
    release(&kmem.lock);
    kfree(mem);
    mem = pa;

  } else {
    // created exclusive page, so decrement `pa` counter
    kmem.refs[REF_POS(pa)] -= 1;
    release(&kmem.lock);
  }

  return mem;
}

// Increments ref counter
void kdup(void *pa) {
  if (((uint64)pa % PGSIZE) != 0) panic("kdup");

  acquire(&kmem.lock);
  kmem.refs[REF_POS(pa)] += 1;
  release(&kmem.lock);
}

// For debug purposes only
int krefcnt(void *pa) {
  uint res;

  if (((uint64)pa % PGSIZE) != 0) panic("krefcnt");

  if ((char *)pa < end || (uint64)pa >= PHYSTOP) {
    return -1;
  }

  acquire(&kmem.lock);
  res = kmem.refs[REF_POS(pa)];
  release(&kmem.lock);
  return res;
}
