# safe-env-parser

Программа читает `.env`-файл, вычисляет значения переменных (включая подстановки вида `$VAR`) и выводит результат в JSON. Исполнение команд через `$(...)` и бэктики запрещены


## Ошибки

| Сообщеие | Причина | Пример |
|-----------|---------|--------|
| `Parse error: error: $(...) is forbidden` | попытка исполнения команды | `examples/dangerous.env` |
| `Parse error: error: backtick is forbidden` | бэктик-подстановка | `examples/backtick.env`, `examples/backtick_sq.env` |
| `Parse error: error: unclosed double quote` | незакрытая двойная кавычка | `examples/unclosed_quote.env` |
| `Parse error: unknown esc: \q` | неизвестная escape-последоательность | `examples/bad_escape.env` |
| `Parse error: bad name: ...` | имя переменной начинается с цифры | `examples/bad_name.env` |
| `Error: unknown variable: $X` | переменная X не объявлена | `examples/unknown.env` |
| `Error: cycle detected: A -> B -> C` | циклическая зависимость | `examples/cycle.env` |
| `Error: duplicate variable: X` | имя X определено дважды | `examples/duplicate.env` |
