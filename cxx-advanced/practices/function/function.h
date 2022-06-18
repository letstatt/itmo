#pragma once

template <typename R, typename... Args>
struct function;

struct bad_function_call : std::exception {
  const char* what() const noexcept override {
    return "calling uninitialized function object";
  }
};

namespace function_details {

using storage =
    typename std::aligned_storage<sizeof(void*), alignof(void*)>::type;

template <typename T>
static constexpr bool is_small = (sizeof(T) <= sizeof(T*) &&
                                  alignof(T*) % alignof(T) == 0 &&
                                  std::is_nothrow_move_constructible_v<T> &&
                                  std::is_trivially_copyable_v<T>);

template <typename R, typename... Args>
struct v_optable {
  using F = function<R(Args...)>;

  struct blank {
    static F dup(F const&) {
      return F();
    }
    static R invoke(storage&, Args&&...) {
      throw bad_function_call();
    }
    static void destroy(storage&) {}
  };

  template <typename T>
  struct impl {
    static F dup(F const& src) {
      return F(*get(src.stg));
    }

    static R invoke(storage& s, Args&&... args) {
      return get(s)->operator()(std::forward<Args>(args)...);
    }

    static void destroy(storage& s) {
      if constexpr (is_small<T>) {
        get(s)->~T();
      } else {
        delete get(s);
      }
    }

    static T* get(storage& s) {
      if constexpr (is_small<T>) {
        return reinterpret_cast<T*>(&s);
      } else {
        return *reinterpret_cast<T**>(&s);
      }
    }

    static const T* get(storage const& s) {
      if constexpr (is_small<T>) {
        return reinterpret_cast<const T*>(&s);
      } else {
        return *reinterpret_cast<T* const*>(&s);
      }
    }
  };

  static const v_optable* get_empty_v_optable() {
    static constexpr v_optable<R, Args...> vt = {&blank::dup, &blank::invoke,
                                                 &blank::destroy};
    return &vt;
  }

  template <typename T>
  static const v_optable* get_v_optable() {
    static constexpr v_optable<R, Args...> vt = {
        &impl<T>::dup, &impl<T>::invoke, &impl<T>::destroy};
    return &vt;
  }

  F (*duplicate)(F const&);
  R (*invoker)(storage&, Args&&...);
  void (*destroyer)(storage&);
};
} // namespace function_details

template <typename R, typename... Args>
struct function<R(Args...)> {
  using v_optable = function_details::v_optable<R, Args...>;
  using storage = function_details::storage;
  friend v_optable;

  function() noexcept = default;

  function(const function& other) {
    *this = other;
  }

  function(function&& other) noexcept {
    swap(other);
  }

  template <typename T>
  function(T val) // NOLINT
      : vt(v_optable::template get_v_optable<T>()) {
    if constexpr (function_details::is_small<T>) {
      new (&stg) T(std::move(val));
    } else {
      new (&stg) T*(new T(std::move(val)));
    }
  }

  function& operator=(const function& other) {
    if (this != &other) {
      *this = other.vt->duplicate(other);
      vt = other.vt;
    }
    return *this;
  }

  function& operator=(function&& other) noexcept {
    if (this != &other) {
      swap(other);
    }
    return *this;
  }

  ~function() {
    vt->destroyer(stg);
  }

  explicit operator bool() const noexcept {
    return vt != vt_empty;
  }

  R operator()(Args&&... args) {
    return vt->invoker(stg, std::forward<Args>(args)...);
  }

  template <typename T>
  T* target() noexcept {
    return (vt == v_optable::template get_v_optable<T>())
             ? v_optable::template impl<T>::get(stg)
             : nullptr;
  }

  template <typename T>
  T const* target() const noexcept {
    return (vt == v_optable::template get_v_optable<T>())
             ? v_optable::template impl<T>::get(stg)
             : nullptr;
  }

private:
  void swap(function& other) noexcept {
    std::swap(stg, other.stg);
    std::swap(vt, other.vt);
  }

  storage stg{};
  const v_optable* vt = vt_empty;

  static inline const v_optable* const vt_empty =
      v_optable::get_empty_v_optable();
};
