# Домашнее задание 3: Procedural Macro

*This is based on an awesome [`proc-macro-workshop`](https://githubcom/dtolnay/proc-macro-workshop) 
repository.*

Ваша цель реализовать макрос, который будет генерировать код для реализации паттерна [Builder][builder pattern].
Builder позволяет создавать сложные объекты пошагово, не инициализируя все поля сразу. Это особенно полезно для
больших объектов, которые имеют много полей, особенно когда часть из полей является необязательной,
или когда набор полей будет расширяться в будущем.

[builder pattern]: https://en.wikipedia.org/wiki/Builder_pattern

Существует несколько способов реализации паттерна Builder в Rust. Более подробно с ними можно ознакомится
по [ссылке](https://doc.rust-lang.org/1.0.0/style/ownership/builders.html). Для этого задания рекомендуется
использовать API как у [`std::process::Command`] из стандартной библиотеки. В этом варианте методы-сеттеры
получают и возвращают `&mut Self`.

[`std::process::Command`]: https://doc.rust-lang.org/std/process/struct.Command.html

Пример использования макроса:

```rust
use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    current_dir: Option<String>,
}

fn main() {
    let command = Command::builder()
        .executable("cargo".to_owned())
        .arg("build".to_owned())
        .arg("--release".to_owned())
        .build()
        .unwrap();

    assert_eq!(command.executable, "cargo");
}
```

## Тестирование

Тестирование процедурных макросов не самая легкая задача. Встроенный в Rust тестовый фреймворк 
не позволяет легко протестировать возвращаемые пользователю сообщения об ошибках. Cargo не считает
ошибки компиляции (даже хорошие, человекочитаемые ошибки!) признаком успешного теста, и не умеет
сравнивать текст ошибок с эталонными. Поэтому в этом репозитории для тестирование используется
альтернативный фреймворк [trybuild].

The project skeletons in this repository use an alternative test harness called
[trybuild].

[trybuild]: https://github.com/dtolnay/trybuild

<br>

## Решение

Для этого задания предоставлены подготовленные тесты. Запустить их можно командой 
`cargo test` в корне проекта.

Изначально все тесты отключены. Включайте их последовательно, раскомментируя строчки в файле
`tests/progress.rs`. Перед тем, как отправлять решение, убедитесь, что все тесты проходят.

Часть тестов проверяют, что макрос сгенерировал код успешно. Другая часть тестов проверяет,
что макрос сгенерировал человекочитаемую ошибку компиляции. Ожидаемый вывод компилятора размещается
в файлах с расширением `.stderr`.

<br>

## Отладка

Для того, чтобы посмотреть на сгенерированный код, можно воспользоваться командой `cargo expand`.

Установите пакет `cargo-expand` командой `cargo install cargo-expand`. Напишите интересующий вас
код в файле `src/main.rs`. Затем запустите команду
`cargo expand --bin derive_builder` в корне проекта. Сгенерированный макросом код будет выведен в
консоль.

[cargo expand]: https://github.com/dtolnay/cargo-expand

Если макрос генерирует синтаксически неверный код, `cargo expand` не сможет его отобразить. В этом
случае можно заставить макрос писать в консоль сгенерированное дерево токенов.

```rust
eprintln!("TOKENS: {}", tokens);
```

Обратите внимание, что по умолчанию библиотека `syn` не снабжает свои типы реализацией `Debug`. 
Чтобы это исправить, добавьте `features = ["extra-traits"]` для зависимости `syn` в файле 
`Cargo.toml`.

<br>

### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this codebase by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
</sub>
