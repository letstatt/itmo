// Сгенерируйте структуру для хранения состояния билдера, а также функцию-конструктор для этой структуры.
//
// Начните с генерации следующего кода (убедитесь, что используете правильное имя структуры билдера,
// которое совпадает с именем структуры пользователя).
//
//     impl Command {
//         pub fn builder() {}
//     }
//
// Уже на этом этапе тест пройдет, потому что `()` уже является достаточным типом для хранения
// состояния нашего билдера.
//
// Прежде чем продолжить, сгенерируйте следующий код:
//
//     pub struct CommandBuilder {
//         executable: Option<String>,
//         args: Option<Vec<String>>,
//         env: Option<Vec<String>>,
//         current_dir: Option<String>,
//     }
//
// и функцию `builder`:
//
//     impl Command {
//         pub fn builder() -> CommandBuilder {
//             CommandBuilder {
//                 executable: None,
//                 args: None,
//                 env: None,
//                 current_dir: None,
//             }
//         }
//     }
//
//
// Ссылки:
//
//   - Крейт `quote` для генерации кода:
//     https://github.com/dtolnay/quote
//
//   - Склеивание пользовательского названия + "Builder" для генерации имени структуры билдера:
//     https://docs.rs/syn/1.0/syn/struct.Ident.html

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: String,
}

fn main() {
    let builder = Command::builder();

    let _ = builder;
}
