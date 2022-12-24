## Вариант 8

## Выберите подмножество теха и напишите его конвертор в HTML.

### При необходимости используйте MathML.

Пример:

`$a_i = b_i + x^2$`

Вывод:

`<i>a</i><sub><i>i</i></sub> = <i>b</i><sub><i>i</i></sub> +
<i>x</i><sup>2</sup>`

## План

1. Найдем документацию по ANTLR и MathML
2. Выберем подмножество элементов из MathML
3. Выберем подмножество теха, которые переводятся в это подмножество
4. Напишем грамматику для генерации парсера
5. Заиспользуем сгенерированный парсер для перевода теха в язык MathML

## Маппинг MathML-TeX

1. `<math>...</math>` = `$...$`
2. `<mn>...</mn>` = `5`, `10` (числа)
3. `<mo>...</mo>` = `+`, `*`, `=`, `(` (операторы, скобки)
4. `<mi>...</mi>` = `a`, `b` (переменные, текст)
5. `<mrow>...</mrow>` = `{...}` (группировка)
6. `<msqrt>base</msqrt>` = `\sqrt{base}`
7. `<mroot>base index</mroot>` = `\sqrt[index]{base}`
8. `<mfrac>numerator denominator</mfrac>` = `\frac{numerator}{denominator}`
9. `<msub>base subscript</msub>` = `base_{subscript}`
10. `<msup>base superscript</msup>` = `base^{superscript}`
11. `<msubsup>base subscript superscript</msubsup>` = `base_{subscript}^{superscript}`

## Генерация парсера

`antlr4 .\tex2html.g4 -o .\src\com\letstatt\antlr`

## Аргументы командной строки

`java ... [-i input_file] [-o output_file]`

Пример:

`java ... -i input.tex -o output.html`