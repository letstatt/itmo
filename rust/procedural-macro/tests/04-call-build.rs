// Сгенерируйте финализирующий метод `build` для создания пользовательской структуры.
//
// Этот метод должен проверять, что каждое из полей было явным образом установлено.
// Он должен вернуть ошибку, если любое из полей отсутствует. Точный тип ошибки не важен,
// можете использовать `String`, `Box<dyn Error>` или `anyhow::Error` из крейта [`anyhow`](https://lib.rs/crates/anyhow).
//
//     impl CommandBuilder {
//         pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
//             ...
//         }
//     }

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: String,
}

fn main() {
    let mut builder = Command::builder();
    builder.executable("cargo".to_owned());
    builder.args(vec!["build".to_owned(), "--release".to_owned()]);
    builder.env(vec![]);
    builder.current_dir("..".to_owned());

    let command = builder.build().unwrap();
    assert_eq!(command.executable, "cargo");
}
