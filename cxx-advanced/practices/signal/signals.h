#pragma once
#include "intrusive_list.h"
#include <functional>

// Чтобы не было коллизий с UNIX-сигналами реализация вынесена в неймспейс, по
// той же причине изменено и название файла
namespace signals {

template <typename T>
struct signal;

template <typename... Args>
struct signal<void(Args...)> {
  using callback_t = std::function<void(Args...)>;
  struct connection : intrusive::list_element<> {
    connection() = default;

    connection(signal* sig, callback_t cb) : instance(sig), callback(std::move(cb)) {
      sig->callbacks.push_back(*this);
    }

    connection(connection&& other) {
      replace_by(other);
    }

    connection& operator=(connection&& other) {
      if (this != &other) {
        disconnect();
        replace_by(other);
      }
      return *this;
    }

    void disconnect() noexcept {
      if (!instance) {
        return;
      }
      for (auto cur = instance->tail; cur != nullptr; cur = cur->next) {
        if (&*cur->it == this) {
          cur->it++;
        }
      }
      drop();
    }

    ~connection() {
      disconnect();
    }

  private:
    signal* instance = nullptr;
    callback_t callback;

    friend struct signal;

    void replace_by(connection& other) {
      callback = std::move(other.callback);
      instance = other.instance;
      if (instance) {
        instance->callbacks.insert(other, *this);
        other.disconnect();
      }
    }

    void drop() {
      unlink();
      instance = nullptr;
      callback = {};
    }
  };

  signal() = default;

  signal(signal const&) = delete;
  signal& operator=(signal const&) = delete;

  ~signal() {
    iteration_token* p = tail;
    while (p != nullptr) {
      p->sig = nullptr;
      p = p->next;
    }

    while (!callbacks.empty()) {
      callbacks.back().drop();
    }
  }

  connection connect(callback_t slot) noexcept {
    return connection(this, std::move(slot));
  }

  struct iteration_token {
    friend struct signal;
    explicit iteration_token(const signal* sig)
        : it(sig->callbacks.begin()), next(sig->tail), sig(sig) {
      sig->tail = this;
    }

    ~iteration_token() {
      if (sig) {
        sig->tail = next;
      }
    }

  private:
    typename intrusive::list<connection>::const_iterator it;
    iteration_token* next;
    const signal* sig;
  };

  void operator()(Args... args) const {
    iteration_token token(this);

    while (token.sig && token.it != callbacks.end()) {
      auto cur = token.it;
      ++token.it;
      cur->callback(args...);
    }
  }

private:
  intrusive::list<connection> callbacks;
  mutable iteration_token* tail = nullptr;
};

} // namespace signals
