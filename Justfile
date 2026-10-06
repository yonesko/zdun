# Показывать список доступных команд при запуске просто `just`
default:
    @just --list

# Сборка проекта
build:
    stack build --fast

# Запуск тестов (с передачей аргументов, например: just test -m "MyTest")
test *args:
    stack test --fast {{args}}

# Запуск исполняемого файла (с аргументами: just run --flag)
run *args:
    stack run -- {{args}}

# REPL (GHCi) с загруженным проектом
repl:
    stack ghci

# Проверка кода линтером (требует hlint)
lint:
    stack exec -- hlint app/ src/ test/

# Форматирование всего проекта (fourmolu или ormolu)
format:
    fourmolu --mode inplace $(git ls-files '*.hs')

# Проверка форматирования (для CI)
format-check:
    fourmolu --mode check $(git ls-files '*.hs')

# Очистка артефактов сборки
clean:
    stack clean