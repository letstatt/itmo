// 2 october, c++ advanced
// signals (not unix-signals)

struct {
	using slot_t = function<void ()>;
	
	void connect(slot_t slot) {
		slots.push_back(move(slot));
	}
	
	void operator()() {
		for (slot_t const& slot: slots) {
			slot();
		}
	}
	
private:
	vector<slot_t> slots;
}

// хотим disconnect!

struct {
	using slot_t = function<void ()>;
	using id_t = uint64_t;
	
	struct connection {
		connection(signal * sig, id_t id): sig(sig), id(id) {}
		
		void disconnect() {
			size_t cnt = sig->slots.erase(id);
			assert(cnt == 1);
		}
		
	private:
		id_t id;
		signal * sig;
	}
	
	connection connect(slot_t slot) {
		id_t id = next_id++;
		slots.insert(id, move(slot));
		return connection(this, id);
	}
	
	void operator()() {
		for (auto const& e: slots) {
			e.second();
		}
	}
	
private:
	unordered_map<id_t, slot_t> slots;
	id_t next_id = 0;
}

// проблема: если обработчики не noexcept, то вызовутся не все обработчики.
// проблема: если написать обработчик, который отписывается при его вызове, то мы получаем UB, потому что инвалидируем итератор.

struct {
	using slot_t = function<void ()>;
	using id_t = uint64_t;
	
	struct connection {
		connection(signal * sig, id_t id): sig(sig), id(id) {}
		
		void disconnect() {
			auto it = sig->slots.find(id);
			assert(it != sig->slots.end());
			it->second = slot_t();
		}
		
	private:
		id_t id;
		signal * sig;
	}
	
	connection connect(slot_t slot) {
		id_t id = next_id++;
		slots.insert(id, move(slot));
		return connection(this, id);
	}
	
	void operator()() {
		for (auto it = slots.begin(); it != slots.end(); ++it) {
			if (it->second) {
				it->second();
			}
		}
		for (auto it = slots.begin(); it != slots.end();) {
			if (it->second) {
				++it;
			} else {
				it = slots.erase(it);
			}
		}
	}
	
private:
	unordered_map<id_t, slot_t> slots;
	id_t next_id = 0;
}

// проблема: без вызова сигнала мапа не прочищается
// проблема: рекурсивный оператор круглых скобок (reentrancy problem)
// проблема: деструктор сигнала может вызываться внутри круглых скобок

struct {
	using slot_t = function<void ()>;
	using id_t = uint64_t;
	
	struct connection {
		connection(signal * sig, id_t id): sig(sig), id(id) {}
		
		void disconnect() {
			if (sig->inside_emit) {
				auto it = sig->slots.find(id);
				assert(it != sig->slots.end());
				it->second = slot_t();
			} else {
				size_t c = sig->slots.erase(id);
				assert(c == 1);
			}
		}
		
	private:
		id_t id;
		signal * sig;
	}
	
	connection connect(slot_t slot) {
		id_t id = next_id++;
		slots.insert(id, move(slot));
		return connection(this, id);
	}
	
	void leave_emit() const {
		is_destroyed = old_destroyed; // не компилится, но понятно
		if (--inside_emit) {
			return;
		}
		for (auto it = slots.begin(); it != slots.end();) {
			if (it->second) {
				++it;
			} else {
				it = slots.erase(it);
			}
		}
	}
	
	void operator()() const {
		++inside_emit;
		bool flag = false;
		bool* old_destroyed = is_destroyed;
		is_destroyed = &flag;
		try {
			for (auto it = slots.begin(); it != slots.end(); ++it) {
				if (it->second) {
					it->second();
					if (flag) {
						*old_destroyed = true;
						return;
					}
				}
			}
		} catch (...) {
			leave_emit();
			throw e;
		}
		leave_emit();
	}
	
	~signal {
		if (is_destroyed) {
			*is_destroyed = true;
		}
	}
	
private:
	mutable unordered_map<id_t, slot_t> slots;
	id_t next_id = 0;
	mutable size_t inside_emit = false;
	mutable bool* is_destroyed = nullptr;
}

// на самом деле не нужен эмит, не нужны айдишники и unordered_map (можно list)
// при написании кода надо думать, а какие функции обработчики, которые я вызываю, могут вызывать назад? (реентерабельность)

// Практика: std::aligned_storage<sizeof(T),alignof(T)>