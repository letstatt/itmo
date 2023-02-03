// Работает ли ваш макрос, если какие-то типы стандартной библиотеки переопределены в пользовательском коде?
//
// Все макросы дложны учитывать это ограничение и использовать полностью квалифицированные импорты в
// сгенерированном коде (как `std::result::Result` вместо `Result`).

use derive_builder::Builder;

type Option = ();
type Some = ();
type None = ();
type Result = ();
type Box = ();

#[derive(Builder)]
pub struct Command {
    executable: String,
}

fn main() {}
