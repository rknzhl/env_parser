# safe-env-parser

#ПРОТЕСТИТЕ ПЖЖ
Парсер `.env`-файлов на Haskell



надо дописааааать
парсинг

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
## Что умеет

- `KEY=value`, `KEY="value"`, `KEY='value'`
- `export KEY=value`
- `$VAR`, `${VAR}` — подстановка в двойных кавычках и без
- `\n \t \r \\ \" \$` — escape в двойных кавычках
- Одинарные кавычки — буквально, без подстановки
- Комментарии: `# строка` и `KEY=val # хвост`

## Что запрещено

| Конструкция | Почему |
|---|---|
| `$(...)` | shell command substitution |
| `` `...` `` | backtick command substitution |
