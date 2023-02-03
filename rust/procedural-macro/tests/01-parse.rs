// Этот тест проверяет, что макрос с заданным именем сущестует. Возвращаемое дерево токенов не проверяется,
// поэтому достаточно вернуть пустой `TokenStream`.
//
// Распарсите аргумент макроса как `syn::DeriveInput`. Изучите документацию этой структуры, чтобы узнать,
// какие поля она содержит. Они пригодятся вам на следующих шагах.
//
//
// Ссылки:
//
//   - Крейт `syn` для парсинга:
//     https://github.com/dtolnay/syn
//
//   - Документация `DeriveInput`, который представляет входной аргумент макроса:
//     https://docs.rs/syn/1.0/syn/struct.DeriveInput.html
//
//   - Пример макроса, используещего `syn`:
//     https://github.com/dtolnay/syn/tree/master/examples/heapsize

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: String,
}

fn main() {}
