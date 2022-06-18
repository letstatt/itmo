#pragma once
#include <type_traits>
#include <utility>

struct nullopt_t {};

struct in_place_t {};

const static nullopt_t nullopt;
const static in_place_t in_place;

template <bool is_copy_constructable>
struct enable_copy_ctor {
  constexpr enable_copy_ctor() = default;
  constexpr enable_copy_ctor(enable_copy_ctor const&) = delete;
  constexpr enable_copy_ctor(enable_copy_ctor&&) = default;
  constexpr enable_copy_ctor& operator= (enable_copy_ctor const&) = default;
  constexpr enable_copy_ctor& operator= (enable_copy_ctor &&) = default;
};
template <bool is_move_constructable>
struct enable_move_ctor {
  constexpr enable_move_ctor() = default;
  constexpr enable_move_ctor(enable_move_ctor const&) = default;
  constexpr enable_move_ctor(enable_move_ctor&&) = delete;
  constexpr enable_move_ctor& operator= (enable_move_ctor const&) = default;
  constexpr enable_move_ctor& operator= (enable_move_ctor &&) = default;
};
template <bool is_copy_assignable>
struct enable_copy_assignment {
  constexpr enable_copy_assignment() = default;
  constexpr enable_copy_assignment(enable_copy_assignment const&) = default;
  constexpr enable_copy_assignment(enable_copy_assignment&&) = default;
  constexpr enable_copy_assignment& operator= (enable_copy_assignment const&) = delete;
  constexpr enable_copy_assignment& operator= (enable_copy_assignment &&) = default;
};
template <bool is_move_assignable>
struct enable_move_assignment {
  constexpr enable_move_assignment() = default;
  constexpr enable_move_assignment(enable_move_assignment const&) = default;
  constexpr enable_move_assignment(enable_move_assignment&&) = default;
  constexpr enable_move_assignment& operator= (enable_move_assignment const&) = default;
  constexpr enable_move_assignment& operator= (enable_move_assignment &&) = delete;
};

template <>
struct enable_copy_ctor<true> {};
template <>
struct enable_move_ctor<true> {};
template <>
struct enable_copy_assignment<true> {};
template <>
struct enable_move_assignment<true> {};

template <typename T, bool is_trivially_destructible>
struct optional_destructor_base {
  constexpr optional_destructor_base() noexcept : dummy() {}

  constexpr optional_destructor_base(T&& val) : value(std::move(val)), has_value(true) {}

  template <typename... Args>
  explicit constexpr optional_destructor_base(in_place_t, Args&&... args) : value(std::forward<Args>(args)...), has_value(true) {}

  void reset() {
    if (has_value) {
      value.~T();
      has_value = false;
    }
  }

  ~optional_destructor_base() {
    if (has_value) {
      value.~T();
    }
  }

  union {
    T value;
    nullopt_t dummy;
  };
  bool has_value = false;
};

template <typename T>
struct optional_destructor_base<T, true> {
  constexpr optional_destructor_base() noexcept : dummy() {}
  constexpr optional_destructor_base(T&& val) : value(std::move(val)), has_value(true) {}

  template <typename... Args>
  explicit constexpr optional_destructor_base(in_place_t, Args&&... args) : value(std::forward<Args>(args)...), has_value(true) {}

  void reset() {
    has_value = false;
  }

  union {
    T value;
    nullopt_t dummy;
  };
  bool has_value = false;
};

template <typename T, bool is_trivially_copyable>
struct optional_copy_ctor_base :
    optional_destructor_base<T, std::is_trivially_destructible_v<T>> {
  using dtor_base = optional_destructor_base<T, std::is_trivially_destructible_v<T>>;

  constexpr optional_copy_ctor_base() noexcept = default;
  constexpr optional_copy_ctor_base(T&& value) : dtor_base(std::move(value)) {}

  template <typename... Args>
  explicit constexpr optional_copy_ctor_base(in_place_t, Args&&... args) : dtor_base(in_place, std::forward<Args>(args)...) {}

  optional_copy_ctor_base(optional_copy_ctor_base const& other) {
    if (other.has_value) {
      new (&(dtor_base::value)) T(other.value);
      dtor_base::has_value = true;
    }
  }

  optional_copy_ctor_base(optional_copy_ctor_base && other) {
    if (other.has_value) {
      new (&(dtor_base::value)) T(std::move(other.value));
      dtor_base::has_value = true;
      other.reset();
    }
  }

  constexpr optional_copy_ctor_base& operator=(optional_copy_ctor_base const& other) {
    if (this != &other) {
      if (dtor_base::has_value && !other.has_value) {
        dtor_base::reset();

      } else if (!dtor_base::has_value && other.has_value) {
        new (&(dtor_base::value)) T(other.value);
        dtor_base::has_value = true;

      } else if (dtor_base::has_value && other.has_value) {
        dtor_base::value = other.value;
        dtor_base::has_value = true;
      }
    }
    return *this;
  }

  constexpr optional_copy_ctor_base& operator=(optional_copy_ctor_base && other) {
    if (this != &other) {
      if (dtor_base::has_value && !other.has_value) {
        dtor_base::reset();

      } else if (!dtor_base::has_value && other.has_value) {
        new (&(dtor_base::value)) T(std::move(other.value));
        dtor_base::has_value = true;
        other.reset();

      } else if (dtor_base::has_value && other.has_value) {
        dtor_base::value = std::move(other.value);
        dtor_base::has_value = true;
        other.reset();
      }
    }
    return *this;
  }
};

template <typename T>
struct optional_copy_ctor_base<T, true> :
    optional_destructor_base<T, std::is_trivially_destructible_v<T>> {
  using dtor_base = optional_destructor_base<T, std::is_trivially_destructible_v<T>>;

  constexpr optional_copy_ctor_base() noexcept = default;
  constexpr optional_copy_ctor_base(T&& value) : dtor_base(std::move(value)) {}

  template <typename... Args>
  explicit constexpr optional_copy_ctor_base(in_place_t, Args&&... args) : dtor_base(in_place, std::forward<Args>(args)...) {}
};

template <typename T>
class optional : optional_copy_ctor_base<T, std::is_trivially_copyable_v<T>>,
                 enable_copy_ctor<std::is_copy_constructible_v<T>>,
                 enable_move_ctor<std::is_move_constructible_v<T>>,
                 enable_copy_assignment<std::is_copy_assignable_v<T>>,
                 enable_move_assignment<std::is_move_assignable_v<T>> {
public:
  using dtor_base = optional_destructor_base<T, std::is_trivially_destructible_v<T>>;
  using copy_base = optional_copy_ctor_base<T, std::is_trivially_copyable_v<T>>;

  constexpr optional() noexcept = default;
  constexpr optional(nullopt_t) noexcept {}; // NOLINT

  constexpr optional(optional const& other) = default;
  constexpr optional(optional&&) = default;

  optional& operator=(optional const&) = default;
  optional& operator=(optional&&) = default;

  constexpr optional(T value) : copy_base(std::move(value)) {} // NOLINT

  template <typename... Args>
  explicit constexpr optional(in_place_t, Args&&... args) : copy_base(in_place, std::forward<Args>(args)...) {}

  optional& operator=(nullopt_t) noexcept {
    dtor_base::reset();
    return *this;
  }

  constexpr explicit operator bool() const noexcept {
    return dtor_base::has_value;
  }

  constexpr T& operator*() noexcept {
    return dtor_base::value;
  }

  constexpr T const& operator*() const noexcept {
    return dtor_base::value;
  }

  constexpr T* operator->() noexcept {
    return &(dtor_base::value);
  }
  constexpr T const* operator->() const noexcept {
    return &(dtor_base::value);
  }

  template <typename... Args>
  void emplace(Args&&... args) {
    dtor_base::reset();
    *this = optional(in_place, std::forward<Args>(args)...);
  }

  void reset() {
    dtor_base::reset();
  }
};

template <typename T>
constexpr bool operator==(optional<T> const& a, optional<T> const& b) {
  if (bool(a) != bool(b)) {
    return false;
  } else if (!bool(a)) {
    return true;
  } else {
    return *a == *b;
  }
}

template <typename T>
constexpr bool operator!=(optional<T> const& a, optional<T> const& b) {
  return !operator==(a, b);
}

template <typename T>
constexpr bool operator<(optional<T> const& a, optional<T> const& b) {
  if (!bool(b)) {
    return false;
  } else if (!bool(a)) {
    return true;
  } else {
    return *a < *b;
  }
}

template <typename T>
constexpr bool operator<=(optional<T> const& a, optional<T> const& b) {
  if (!bool(a)) {
    return true;
  } else if (!bool(b)) {
    return false;
  } else {
    return *a <= *b;
  }
}

template <typename T>
constexpr bool operator>(optional<T> const& a, optional<T> const& b) {
  if (!bool(a)) {
    return false;
  } else if (!bool(b)) {
    return true;
  } else {
    return *a > *b;
  }
}

template <typename T>
constexpr bool operator>=(optional<T> const& a, optional<T> const& b) {
  if (!bool(b)) {
    return true;
  } else if (!bool(a)) {
    return false;
  } else {
    return *a >= *b;
  }
}