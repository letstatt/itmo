#include "intrusive_list.h"

namespace intrusive {

void list_element_base::unlink() {
  prev->next = next;
  next->prev = prev;
  prev = next = nullptr;
}

void list_element_base::insert_after(list_element_base* pos) {
  pos->next->prev = this;
  next = pos->next;
  prev = pos;
  pos->next = this;
}

bool list_element_base::in_list() const noexcept {
  return prev != nullptr;
}

} // namespace intrusive