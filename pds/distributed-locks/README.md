# Распределённое взаимное исключение

В этом задании вы реализуете несколько алгоритмов для
взаимного исключения в распределенной системе.

## Задача

В файле [`MutexProcess.java`](src/solution/MutexProcess.java) находится описание интерфейса, 
который вам предстоит реализовать. Свой код вы должны писать на языке Java или Kotlin.

Реализацию на Java пишите в файлах:
* [`ProcessCentralized.java`](src/solution/ProcessCentralized.java) &mdash; реализация 
централизованного алгоритма. 
* [`ProcessLamport.java`](src/solution/ProcessLamport.java) &mdash; реализация
 алгоритма Лампорта.
* [`ProcessRickartAgrawala.java`](src/solution/ProcessRickartAgrawala.java) &mdash; реализация
 алгоритма Рикарта и Агравала.
* [`ProcessToken.java`](src/solution/ProcessToken.java) &mdash; реализация
  алгоритма на основе токена.
* [`ProcessPhilosophers.java`](src/solution/ProcessPhilosophers.java) &mdash; реализация
  алгоритма обедающих философов.

Для решения на Kotlin откройте Java файл из списка выше и нажмите Ctrl+Alt+Shift+K (Cmd+Alt+Shift+K на MacOS) в 
IntelliJ IDEA для конвертации соответствующего фала из `XXX.java` в `XXX.kt`. 
На вопрос "Some code in the rest of your project may require corrections after performing this conversion. 
Do you want to find such code and correct it too?" отвечайте "No".
Пишите код в соответствующем kt файле, который получится после конвертации. 

## Окружение процесса

Тестовая система будет запускать ваш код в нескольких процессах, каждому из которых выдан уникальный
идентификатор начинающийся с единицы. Через ссылку на объект [`Environment`](src/internal/Environment.java) 
каждый процесс может узнать конфигурацию системы и общаться с другими процессами:

* `env.getProcessId()` &mdash; возвращается идентификатор процесса (нумерация с единицы).
* `env.getNumberOfProcesses()` &mdash; возвращает общее число процессов.
* `env.send(destinationPid, message)` &mdash; посылает сообщение `message` процессу с номером `destinationPid` 
(нумерация с единицы).
* `env.lock()` &mdash; должен быть вызван процессом при входе в критическую секцию.
* `env.unlock()` &mdash; должен быть вызван процессом при выходе из критической секции.

Конструктор каждого класса, который вам необходимо реализовать, принимает единственный параметр: 
ссылку на объект `Environment`. Сохраните этот объект в поле класса для того, чтобы с его помощью 
взаимодействовать с остальными процессами распределённой системы.

## Интерфейс процесса

Методы вашего класса будут вызываться в следующих случаях:

* `onMessage(sourcePid, message)` &mdash; вызывается при получении сообщения от другого процесса с номером `sourcePid` 
(нумерация с единицы), которое было отправлено этим процессом через `env.send(...)`.
* `onLockRequest()` &mdash; вызывается в случае запроса на вход в критическую секцию.
  Процесс должен инициировать алгоритм
  входа в критическую секцию и, после входа в неё, вызвать метод `env.lock()`.

  Вызов `env.lock()` необязательно должен быть сделан в теле метода `onLockRequest()`: можно вызвать 
  `env.lock()` в ходе обработки очередного сообщения, но только после того как процессу был сделан запрос 
  на вход в критическую секцию с помощью метода `onLockRequest()`.

  Метод `onLockRequest()` не будет повторно вызываться до выхода этого процесса из критической секции.
* `onUnlockRequest()` &mdash; вызывается в случае запроса на выход из критической секции.
  Процесс должен немедленно вызвать метод `env.unlock()` и инициировать алгоритм выхода из критической секции.
  Этот метод будет вызываться только если ранее был осуществлен вход
  в критическую секцию (был вызван `env.lock()`).

## Замечания

* В алгоритме на основе токена гарантируется, что число процессов в системе составляет
  не менее двух;
* В алгоритме Лампорта гарантируется, что сообщения между каждой парой процессов доставляются
  в порядке FIFO;
* В централизованном алгоритме предполагается, что процесс с идентификатором `1` является координатором;
* В алгоритме на основе токена предполагается, что изначально токеном владеет процесс с идентификатором `1`.

## Работа с сообщениями

Каждое отправленное сообщение должно быть сериализуемо. Описание класса сообщений может выглядеть, например, так 
на Java:

```java
record MyMessage(int key, String value) implements java.io.Serializable {}
```

или на Kotlin:

```kotlin
data class MyMessage(val key: Int, val value: String) : java.io.Serializable
```

Размер каждого отправленного сообщения не должен превышать ста байт.

Заметьте, что метод `onMessage(int senderPid, Object message)`в качестве аргумента принимает не описанный
вами тип сообщения, а `java.lang.Object`. На Java используйте приведения типов вида:  

```java 
MyMessage typedMessage = (MyMessage) message;
```

и, если нужно, оператор `instanceof`

```java
if (message instanceof MyMessage) {
    // ...
}
```

для приведения сообщения к нужному вам типу.

На Kotlin:

```kotlin
val typedMessage = message as MyMessage
// or 
if (message is MyMessage) { 
    // ...
}
```

## Тестирование

Тестирования реализации происходит путем запуска тестов:
* [`ProcessCentralizedTest.java`](test/ProcessCentralizedTest.java)
* [`ProcessLamportTest.java`](test/ProcessLamportTest.java)
* [`ProcessRickartAgrawalaTest.java`](test/ProcessRickartAgrawalaTest.java)
* [`ProcessTokenTest.java`](test/ProcessTokenTest.java)
* [`ProcessPhilosophersTest.java`](test/ProcessPhilosophersTest.java)

Из командной строки: `./gradlew test`

Тест проверяет корректность алгоритма (гарантию взаимного исключения), его прогресса (отсутствие взаимных блокировок),
и замеряет эффективность алгоритма, считая количество передаваемых сообщений.
