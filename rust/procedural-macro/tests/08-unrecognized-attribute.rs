// Этот тест проверяет, что пользовать получить человекочитаемое сообщение об ошибке в случае
// опечатки в названии атрибута.
//
// Предпочтительный способ генерации ошибок – генерация код с макросом `compile_error!`.
//
// Ссылки:
//
//   - compile_error! макрос:
//     https://doc.rust-lang.org/std/macro.compile_error.html
//
//   - Преобразование `syn::Error` в вызов `compile_error!`:
//     https://docs.rs/syn/1.0/syn/struct.Error.html#method.to_compile_error

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    #[builder(eac = "arg")]
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}

fn main() {}
