// Билдер для `std::process::Command` позволяет удобно передавать множество аргументов двумя разными способами:
// 1. Через метод, принимающий `Vec<T>`
// 2. Через многократные вызовы методов-сеттеров, принимающих `T`
//
// Ваш макрос должен находить атрибут `#[builder(each = "...")]` у полей. Сгенерированный код может полагаться
// на то, что в этом поле хранится `Vec<T>`. Вы должны сгенерировать как метод-сеттер, принимающий Vec<T>,
// (его имя должно быть таким же, как у поля), так и метод-сеттер, принимающий `T` (его имя указано в атрибуте
// `each`).
//
// Rust требует, чтобы вы указали все используемые вспомогательные атрибуты при объявлении макроса.

// #[proc_macro_derive(Builder, attributes(builder))]

// В случае, конфликта имен между "векторным" и "скалярным" сеттерами, генерируйте только последний ("скалярный").
//
// Ссылки:
//
//   - https://docs.rs/syn/1.0/syn/struct.Attribute.html
//   - https://docs.rs/syn/1.0/syn/enum.Meta.html

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    #[builder(each = "env")]
    env: Vec<String>,
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
    assert_eq!(command.args, vec!["build", "--release"]);
}
