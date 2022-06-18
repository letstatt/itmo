#include "intrusive_list.h"

namespace intrusive {

void list_element_base::unlink() {
  prev->next = next;
  next->prev = prev;
  prev = next = nullptr;
}

void list_element_base::insert_before(list_element_base* pos) {
  pos->prev->next = this;
  prev = pos->prev;
  next = pos;
  pos->prev = this;
}

bool list_element_base::in_list() const noexcept {
  return prev != nullptr;
}

} // namespace intrusive