#pragma once
#include <algorithm>
#include <type_traits>
#include <utility>

namespace intrusive {
struct default_tag;

struct list_element_base {
private:
  mutable list_element_base* prev{nullptr};
  mutable list_element_base* next{nullptr};

  template <typename T, typename Tag>
  friend struct list;

  void insert_after(list_element_base* pos);
  bool in_list() const noexcept;

public:
  void unlink();

  ~list_element_base() noexcept {
    if (in_list()) {
      unlink();
    }
  }
};

template <typename Tag = default_tag>
struct list_element : list_element_base {
  list_element() noexcept = default;
  list_element(const list_element&) = delete;
};

template <typename T, typename Tag = default_tag>
struct list {

  template <typename Ref>
  struct iterator_impl;

  using iterator = iterator_impl<T>;
  using const_iterator = iterator_impl<const T>;

  static_assert(std::is_base_of_v<list_element<Tag>, T>,
                "You should derive from list_element_base");
  list_element<Tag> sentinel;

  void init() noexcept {
    sentinel.next = sentinel.prev = &sentinel;
  }

  list() noexcept {
    init();
  }
  list(list const&) = delete;
  list(list&& other) noexcept {
    init();
    *this = std::move(other);
  }
  ~list() noexcept {
    clear();
  }

  list& operator=(list const&) = delete;
  list& operator=(list&& other) noexcept {
    // want previous elements to be able to detect if they aren't in list
    // anymore, check test #move_operator_to_empty
    if (!empty()) {
      clear();
    }
    if (!other.empty()) {
      sentinel.prev = other.sentinel.prev;
      sentinel.next = other.sentinel.next;
      sentinel.prev->next = &sentinel;
      sentinel.next->prev = &sentinel;
      other.init();
    }
    return *this;
  }

  void clear() noexcept {
    iterator it = begin();
    while (it != end()) {
      it = erase(it);
    }
    init();
  }

  void push_back(T& elem) noexcept {
    insert(end(), elem);
  }

  void pop_back() noexcept {
    sentinel.prev->unlink();
  }

  T& back() noexcept {
    return static_cast<T&>(*sentinel.prev);
  }

  const T& back() const noexcept {
    return static_cast<T&>(*sentinel.prev);
  }

  void push_front(T& elem) noexcept {
    insert(begin(), elem);
  }

  void pop_front() noexcept {
    sentinel.next->unlink();
  }

  T& front() noexcept {
    return static_cast<T&>(*sentinel.next);
  }

  const T& front() const noexcept {
    return static_cast<T&>(*sentinel.next);
  }

  bool empty() const noexcept {
    return sentinel.prev == &sentinel;
  }

  iterator begin() const noexcept {
    return iterator(sentinel.next);
  }

  iterator end() const noexcept {
    return iterator(&sentinel);
  }

  iterator insert(const_iterator pos, T& elem) noexcept {
    if (&to_base(elem) == pos.value) {
      return iterator(&to_base(elem));
    }
    if (to_base(elem).in_list()) {
      to_base(elem).unlink();
    }
    to_base(elem).insert_after(pos.value);
    return iterator(&to_base(elem));
  }

  iterator insert(list_element<Tag>& pos, T& elem) noexcept {
    return insert(iterator(&pos), elem);
  }

  iterator erase(iterator elem) noexcept {
    iterator tmp = elem++;
    to_base(*tmp).unlink();
    return elem;
  }

  void splice(const_iterator pos, list& l, const_iterator first,
              const_iterator last) noexcept {
    if (first != last) {
      auto origin_head = first->prev;

      pos->prev->next = first.value; // left bound
      first->prev = pos->prev;

      last->prev->next = pos.value; // right bound
      pos->prev = last->prev;

      origin_head->next = last.value; // origin linking
      last->prev = origin_head;
    }
  }

  static list_element<Tag>& to_base(T& elem) {
    return static_cast<list_element<Tag>&>(elem);
  }

  static const list_element<Tag>& to_base(const T& elem) {
    return static_cast<const list_element<Tag>&>(elem);
  }

  template <typename Ref>
  struct iterator_impl
      : public std::iterator<std::bidirectional_iterator_tag, Ref> {
  public:
    iterator_impl() = default;
    iterator_impl(const iterator_impl<T>& other) : value(other.value) {}

  private:
    friend intrusive::list<T, Tag>;

    explicit iterator_impl(const list_element<Tag>* ptr)
        : value(const_cast<list_element_base*>(
              static_cast<const list_element_base*>(ptr))) {}

    explicit iterator_impl(const list_element_base* ptr)
        : value(const_cast<list_element_base*>(ptr)) {}

  public:
    Ref& operator*() const {
      return static_cast<Ref&>(*static_cast<list_element<Tag>*>(value));
    }

    Ref* operator->() const {
      return &static_cast<Ref&>(*static_cast<list_element<Tag>*>(value));
    }

    iterator_impl& operator--() {
      value = value->prev;
      return *this;
    }

    iterator_impl& operator++() {
      value = value->next;
      return *this;
    }

    iterator_impl operator--(int) {
      auto tmp = *this;
      operator--();
      return tmp;
    }

    iterator_impl operator++(int) {
      auto tmp = *this;
      operator++();
      return tmp;
    }

    friend bool operator==(const iterator_impl lhs,
                           const iterator_impl rhs) noexcept {
      return lhs.value == rhs.value;
    }

    friend bool operator!=(const iterator_impl lhs,
                           const iterator_impl rhs) noexcept {
      return lhs.value != rhs.value;
    }

    list_element_base* value = nullptr;
  };
};

} // namespace intrusive