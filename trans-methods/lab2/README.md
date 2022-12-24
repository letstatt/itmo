## Вариант 9
### Описание заголовка функции в Kotlin.

1. **Начнем в примеров**

```
fun foo()
fun bar(str: String)
fun printSum(a: Int, b: Int)
fun double(x: Int, ): Int
```

2. **Построим КС-грамматику**

Для того, чтобы имя функции/переменной не выражалось последовательностью односивольных токенов, а являлось одним большим токеном, прибегнем к регулярным выражениям.

```
[head] = [a-zA-Z_]
[tail] = [a-zA-Z0-9_]
$name$ = [head][tail]*
```

С этой же целью, будем иметь в виду, что ключевое слово `fun` является одним токеном.

Теперь запишем саму грамматику с экстеншном в виде полученной регулярки и многосимвольных токенов:

```
# specials
F => fun
N => $name$

# main
S -> F N(L)E
L -> $eps$
L -> A
L -> A,L
A -> N:N
E -> $eps$
E -> :N
```

Заметим, что пробел важно сохранять только между ключевым словом `fun` и названием функции, поэтому для простоты от остальных пробелов мы избавимся на уровне токенизации.

В грамматике выше легко заметно правое ветвление по нетерминалу `L`, от которого мы избавимся по известному правилу:

```
# specials
F => fun
N => $name$

# main
S -> F N(L)E
L -> $eps$
L -> AB
B -> ,L
B -> $eps$
A -> N:N
E -> $eps$
E -> :N
```

3. **Лексический анализатор**

В соответствии с полученной грамматикой построим токенайзер. Он будет довольно прост:

```
# pseudocode
s = input()
i = 0

enum Token {
    FUNC_DECL,
    NAME(str),
    LPAREN,
    RPAREN,
    COLON,
    COMMA,
    SPACE,
    $
}

tokens = []

while i < s.len():
    # trim spaces
    if s[i] == ' ':
        while i < s.len() and s[i] == ' ':
            i++

    # keywords and identifiers
    elif is_alpha(s[i]) or s[i] == '_':
        t = s[i++]
        while i < s.len() and \
            (is_alphanum(s[i]) or s[i] == '_'):
            t += s[i++]
        
        if !empty(tokens) and \
            type(tokens[-1]) == FUNC_DECL | NAME:
            tokens.push(SPACE)
        
        tokens.push(
            match t:
                'fun': FUNC_DECL,
                _: NAME(t))

    # special symbols
    else:
        t = s[i++]
        tokens.push(
            match t:
                '(': LPAREN,
                ')': RPAREN,
                ':': COLON,
                ',': COMMA,
                _: error("unexpected token: {t}"))

tokens.push(END)
```

Обратите внимание, что слово `fun` теперь является зарезервированным и в качестве идентификатора использовать его нельзя. Теперь оно является исключительно началом декларации функции.

При необходимости алгоритм можно сделать онлайновым.

4. **Получение таблицы FIRST/FOLLOW**

Продублируем грамматику для удобства.

```
F => fun
N => $name$

S -> F N(L)E
L -> $eps$
L -> AB
B -> ,L
B -> $eps$
A -> N:N
E -> $eps$
E -> :N
```

Построение FIRST тривиально. Для построения FOLLOW потребуется чуть больше времени, но это тоже можно сделать без алгоритмов.

```
   |    FIRST    |    FOLLOW    |
-----------------------------------
N  | [head]      | (, ), :, $
F  | f           | ` `
L  | eps, [head] | )
B  | eps, `,`    | )
A  | [head]      | ), `,`
E  | eps, `:`    | $
```

Используя теорему и полученную таблицу, несложно доказать, что наша грамматика входит в множество `LL(1)`.

5. **Написание синтаксического анализатора**

См. исходный код

6. **Визуализация дерева разбора**

Запуск программы с дополнительным вторым параметром - выходным файлом, куда будет записано представление синтаксического дерева на языке GraphViz.

7. **Тесты**

См. tests/tests.py

8. **Модификация**

Разрешаем дженерики с одним параметром. Примеры:

```
fun foo(m: Array<Int>)
fun bar(): Array<Array<Int>>
```

Для поддержки дженериков сначала нужно поправить грамматику:

```
F => fun
N => $name$

S -> F N(L)E
L -> $eps$
L -> AB
B -> ,L
B -> $eps$
A -> N:T
E -> $eps$
E -> :T
T -> NG
G -> $eps$
G -> <T>
```

Поддержим изменения и в таблице FIRST/FOLLOW.

```
   |    FIRST    |    FOLLOW    |
-----------------------------------
N  | [head]      | (, ), <, :, $
F  | f           | ` `
L  | eps, [head] | )
B  | eps, `,`    | )
A  | [head]      | ), `,`
E  | eps, `:`    | $
T  | [head]      | ), >, `,`, $
G  | eps, <      | ), >, `,`, $
```

Внесем правки в парсер.