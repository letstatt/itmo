#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

struct default_deleter {
  template <typename T>
  void operator()(T* ptr) {
    delete ptr;
  }
};

struct control_block_base {
  explicit control_block_base(size_t num)
      : ref_count(num), weak_ref_count(num) {}

  void inc_ref() {
    ref_count++;
    inc_weak();
  }

  void dec_ref() {
    ref_count--;
    if (0 == ref_count) {
      delete_obj();
    }
    dec_weak();
  }

  void inc_weak() {
    weak_ref_count++;
  }

  void dec_weak() {
    weak_ref_count--;
    if (0 == weak_ref_count) {
      delete this;
    }
  }

  size_t use_count() const {
    return ref_count;
  }

  virtual void* get() = 0;
  virtual void delete_obj() = 0;
  virtual ~control_block_base() = default;

private:
  size_t ref_count;
  size_t weak_ref_count;
};

template <typename T, typename D>
struct basic_control_block : control_block_base {
  explicit basic_control_block(T* ptr, D&& deleter)
      : control_block_base(1), ptr(ptr), deleter(std::forward<D>(deleter)) {}

  void* get() override {
    return ptr;
  }

  void delete_obj() override {
    deleter(ptr);
  }

  ~basic_control_block() override = default;

private:
  T* ptr;
  [[no_unique_address]] D deleter;
};

template <typename T>
struct inplace_control_block : control_block_base {

  template <typename... Args>
  explicit inplace_control_block(Args&&... args) : control_block_base(1) {
    new (&obj)(T)(std::forward<Args>(args)...);
  }

  void* get() override {
    return &reinterpret_cast<T&>(obj);
  }

  void delete_obj() override {
    reinterpret_cast<T&>(obj).~T();
  }

  ~inplace_control_block() override = default;

  typename std::aligned_storage<sizeof(T), alignof(T)>::type obj;
};

struct control_block_holder {
  control_block_holder() = default;
  explicit control_block_holder(control_block_base* cb) : cb(cb) {}

  // common
  size_t use_count() const noexcept {
    return cb ? cb->use_count() : 0;
  }

  // strong references
  void capture(const control_block_holder& other) noexcept {
    release();
    cb = other.cb;
    if (cb) {
      cb->inc_ref();
    }
  }

  void own(control_block_holder& other) noexcept {
    capture(other);
    other.release();
  }

  void release() noexcept {
    if (use_count()) {
      cb->dec_ref();
      cb = nullptr;
    }
  }

  // weak references
  void wcapture(const control_block_holder& other) noexcept {
    wrelease();
    cb = other.cb;
    if (cb) {
      cb->inc_weak();
    }
  }

  void wown(control_block_holder& other) noexcept {
    wcapture(other);
    other.wrelease();
  }

  void wrelease() noexcept {
    if (cb) {
      cb->dec_weak();
      cb = nullptr;
    }
  }

  control_block_base* cb = nullptr;
};

template <typename T>
class weak_ptr;

template <typename T>
class shared_ptr;

template <typename T>
class shared_ptr : control_block_holder {
  friend weak_ptr<T>;

  template <typename T1>
  friend class shared_ptr;

  template <typename T1, typename... Args>
  friend shared_ptr<T1> make_shared(Args&&... args);

  using make_shared_tag = struct {};

public:
  shared_ptr() noexcept = default;
  shared_ptr(std::nullptr_t) noexcept : shared_ptr() {}

  template <typename T1, typename D = default_deleter>
  explicit shared_ptr(T1* ptr, D&& deleter = D()) try
      : control_block_holder(
            new basic_control_block<T1, D>(ptr, std::forward<D>(deleter))),
        ptr(ptr) {
    static_assert(std::is_invocable_v<D, T1*>);
    static_assert(std::is_convertible<T1, T>::value);
  } catch (...) {
    deleter(ptr);
    throw;
  }

  // aliasing constructor
  template <typename T1>
  shared_ptr(const shared_ptr<T1>& alias, T* ptr) noexcept : ptr(ptr) {
    static_assert(std::is_convertible<T1, T>::value);
    control_block_holder::capture(alias);
  }

  // copying constructor
  // without specialization compiler thinks that this constructor is deleted
  shared_ptr(const shared_ptr<T>& other) noexcept : ptr(other.ptr) {
    control_block_holder::capture(other);
  }

  // copying constructor
  template <typename T1>
  shared_ptr(const shared_ptr<T1>& other) noexcept : ptr(other.ptr) {
    static_assert(std::is_convertible<T1, T>::value);
    control_block_holder::capture(other);
  }

  // move constructor
  template <typename T1>
  shared_ptr(shared_ptr<T1>&& other) noexcept : ptr(other.ptr) {
    static_assert(std::is_convertible<T1, T>::value);
    control_block_holder::own(other);
    other.ptr = nullptr;
  }

  shared_ptr& operator=(const shared_ptr& other) noexcept {
    if (this != &other) {
      control_block_holder::capture(other);
      ptr = other.ptr;
    }
    return *this;
  }

  shared_ptr& operator=(shared_ptr&& other) noexcept {
    if (this != &other) {
      control_block_holder::own(other);
      ptr = other.ptr;
      other.ptr = nullptr;
    }
    return *this;
  }

  T* get() const noexcept {
    return use_count() ? ptr : nullptr;
  }

  operator bool() const noexcept { // NOLINT
    return get() != nullptr;
  }

  T& operator*() const noexcept {
    return *get();
  }

  T* operator->() const noexcept {
    return &operator*();
  }

  std::size_t use_count() const noexcept {
    return control_block_holder::use_count();
  }

  void reset() noexcept {
    control_block_holder::release();
    ptr = nullptr;
  }

  template <typename T1, typename D = default_deleter>
  void reset(T1* new_ptr, D&& deleter = D()) {
    static_assert(std::is_invocable_v<D, T1*>);
    static_assert(std::is_convertible<T1, T>::value);
    *this = shared_ptr(new_ptr, std::forward<D>(deleter));
  }

  ~shared_ptr() {
    reset();
  }

private:
  T* ptr = nullptr;

  template <typename... Args>
  explicit shared_ptr(make_shared_tag, Args&&... args)
      : control_block_holder(
            new inplace_control_block<T>(std::forward<Args>(args)...)),
        ptr(static_cast<T*>(cb->get())) {}

  // constructing from weak_ptr
  explicit shared_ptr(const control_block_holder& cbh) noexcept {
    if (cbh.use_count()) {
      control_block_holder::capture(cbh);
      ptr = static_cast<T*>(cb->get());
    } else {
      cb = cbh.cb;
    }
  }
};

template <typename T1, typename T2>
bool operator==(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
  return a.get() == b.get();
}

template <typename T1>
bool operator==(const shared_ptr<T1>& a, std::nullptr_t) noexcept {
  return !a;
}

template <typename T1>
bool operator==(std::nullptr_t, const shared_ptr<T1>& a) noexcept {
  return !a;
}

template <typename T1, typename T2>
bool operator!=(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
  return a.get() != b.get();
}

template <typename T1>
bool operator!=(const shared_ptr<T1>& a, std::nullptr_t) noexcept {
  return a;
}

template <typename T1>
bool operator!=(std::nullptr_t, const shared_ptr<T1>& a) noexcept {
  return a;
}

template <typename T>
class weak_ptr : control_block_holder {
public:
  weak_ptr() noexcept = default;
  weak_ptr(const shared_ptr<T>& other) noexcept { // NOLINT
    control_block_holder::wcapture(other);
  }

  weak_ptr(const weak_ptr<T>& other) noexcept {
    control_block_holder::wcapture(other);
  }

  weak_ptr(weak_ptr<T>&& other) noexcept {
    control_block_holder::wown(other);
  }

  weak_ptr& operator=(const shared_ptr<T>& other) noexcept {
    control_block_holder::wcapture(other);
    return *this;
  }

  weak_ptr& operator=(const weak_ptr<T>& other) noexcept {
    if (this != &other) {
      control_block_holder::wcapture(other);
    }
    return *this;
  }

  weak_ptr& operator=(weak_ptr<T>&& other) noexcept {
    if (this != &other) {
      control_block_holder::wown(other);
    }
    return *this;
  }

  shared_ptr<T> lock() const noexcept {
    return shared_ptr<T>(static_cast<const control_block_holder&>(*this));
  }

  ~weak_ptr() {
    control_block_holder::wrelease();
  }
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&... args) {
  return shared_ptr<T>(typename shared_ptr<T>::make_shared_tag(),
                       std::forward<Args>(args)...);
}
