// Некоторые поля пользовательской структуры могут быть необязательными. Обычно они используют
// тип `Option<T>`.
//
// Ваш макрос должен находить такие поля и не требовать обязательного вызова метода-сеттера для них.
// В данном тесте `current_dir` является необязательным полем.
//
// Учитывайте, что Rust компилятор выполняет разрешение имен уже после разворачивания макросов.
// Это означает, что на момент исполнения макроса у вас нет информации о типах, а есть только о токенах.
// Например, `Option<T>`, `std::option::Option<T>`, `<Vec<Option<T>> as IntoIterator>::Item` – это разные
// представления одного и того же типа. Верно и обратное: идентичные наборы токенов могут означать
// разные тиаы в зависимости от места использования. Как следствие, написать универсальный макрос, который
// эффективно обрабатывает все возможные случаи, невозможно.
//
// Для этого задания достаточно просто определять поля, тип которых записан только как `Option<...>` и
// игнорировать все другие альтернативные представления этого типа.
//
// Синтаксическое дерево для представления типов в Rust достаточно сложное, потому что язык имеет
// множество вариантов синтаксиса для описания типов. Вам нужно находить следующее синтаксическое поддерево:
//
//     Type::Path(
//         TypePath {
//             qself: None,
//             path: Path {
//                 segments: [
//                     PathSegment {
//                         ident: "Option",
//                         arguments: PathArguments::AngleBracketed(
//                             AngleBracketedGenericArguments {
//                                 args: [
//                                     GenericArgument::Type(
//                                         ...
//                                     ),
//                                 ],
//                             },
//                         ),
//                     },
//                 ],
//             },
//         },
//     )

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}

fn main() {
    let command = Command::builder()
        .executable("cargo".to_owned())
        .args(vec!["build".to_owned(), "--release".to_owned()])
        .env(vec![])
        .build()
        .unwrap();
    assert!(command.current_dir.is_none());

    let command = Command::builder()
        .executable("cargo".to_owned())
        .args(vec!["build".to_owned(), "--release".to_owned()])
        .env(vec![])
        .current_dir("..".to_owned())
        .build()
        .unwrap();
    assert!(command.current_dir.is_some());
}
