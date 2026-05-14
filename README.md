# safe-env-parser

Парсер `.env`-файлов на Haskell

# НОРМ?

- `isNameChar` — символ допустим в имени переменной
- `parseName` — вырезает имя из начала строки

- `stripExport` — убирает `export` если есть
- `parseSingleQuoted` — парсит `'...'`, запрещает `$(`, чтобы не хакали в энвах
- `parseRawChunks` — парсит значение, стопается на `#`

- `trimRightChunks` — обрезает пробелы справа
- `parseDoubleChunks` — парсит `"..."` с `$VAR` внутри - есть такие подстаговки в ""
- `parseValue` — выбирает парсер по первому символу
- `parseAssignment` — парсит строку `NAME=value`
- `parseLine` — пропускает пустые строки и комментарии
- `parseLines` — парсит списк строк рекурсивно
- `parseEnvFile` —  точка входа

## Тестирование

```
cabal repl
:m +Parser
:r

:r
parseAssignment "NAME=hello"
```