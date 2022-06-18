расширения стандартной библиотеки - std::optional и std::variant

std::optional<T>

- T может быть, а может не быть
- не требует дефолтного конструктора
- безопасность

{
	std::optional<int> cached_value;

	if (!cached_value) {
		cached_value = compute_value();
	}
	return *cached_value;
}

optional<int> a = 42;
optional<int> b = nullopt;

[[clang::trivial_abi]] - тривиальный abi (чтобы быть, например, тривиально разрушимым объектом, тривиально копируемым).

optional имеет свойство наследовать trivialy_copiable и trivialy_destructible от темплейтного типа.

std::variant<A,B,C> v - хранит одну из трех альтернатив

v.index() = {0,1,2}
std::get<1>(v)
std::get_if<0>(v) - nullptr или T0

шаблон программироания visitor