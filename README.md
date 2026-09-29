# Zdun

```text
          _______
         /       \
        /  (o) (o) \

       |     V     |   <- (Сижу, жду твои порты...)
       |   \___/   |
       /           \
      /  /|     |\  \

     |  | |_____| |  |
      \  \_______/  /
       \___________/
```

> *«Сижу, жду твои порты...»*

**Zdun** — легковесная CLI-утилита для ожидания готовности зависимостей (readiness probes) перед запуском основной команды. Заменяет медленные и громоздкие bash-скрипты на базе `nc`, `curl` и бесконечных циклов `while`.

Все проверки выполняются **параллельно**, с единым таймаутом и информативным выводом ошибок.

---

## Возможности

* **TCP проверки (`--tcp`)**: замена `nc -z`. Поддерживает форматы `host:port` и `tcp://host:port`.
* **HTTP проверки (`--http`)**: 
  * Проверка кода ответа `200 OK` (заголовок без скачивания тела).
  * Проверка кода `200 OK` + совпадение тела ответа по регулярному выражению (`RegEx`). Скачивает только первые 64 КБ потоком, поддерживает UTF-8/кириллицу.
* **Параллельное выполнение**: все сервисы опрашиваются одновременно, а не по очереди.
* **Таймаут (`-t`)**: поддержка форматов `30s`, `1m`, `5m10s`.
* **Бесшовная передача управления**: после готовности всех сервисов запускает команду через `execvp` без оверхеда на родительский процесс.

---

## Использование

```bash
zdun [ОПЦИИ] -- КОМАНДА [АРГУМЕНТЫ...]
```

### Ключи:
* `-t, --timeout DURATION` — максимальное время ожидания всех сервисов (например: `10s`, `1m`, `2m30s`). По умолчанию `0` (ждать бесконечно).
* `--tcp TARGET` — проверка TCP-порта. Формат: `host:port` или `tcp://host:port` (можно указывать несколько раз).
* `--http TARGET` — проверка HTTP-эндпоинта. Формат: `URL` (для проверки 200 OK) или `regex@URL` (для проверки тела по регулярке). Можно указывать несколько раз.
* `--version` — показать версию утилиты.
* `-h, --help` — показать справку.

---

## Инструкция по миграции

### Таблица соответствия команд

| Старый способ (Bash / CLI) | Новый способ в Zdun | Описание |
|---|---|---|
| `nc -z tsdb 5432` | `--tcp tsdb:5432` | Проверка доступности TCP-порта |
| `while ! nc -z zookeeper 2181; do sleep 1; done` | `--tcp zookeeper:2181` | Ожидание открытия порта |
| `curl --silent --fail http://mock:8000/__admin/health` | `--http http://mock:8000/__admin/health` | Проверка HTTP 200 OK (без скачивания тела) |
| `curl -s http://kafka:9644/v1/status/ready \| grep -q ready` | `--http "ready@http://kafka:9644/v1/status/ready"` | Проверка HTTP 200 OK + совпадение тела по RegEx |

---

### Примеры миграции

#### 1. Миграция из Docker Compose

##### До миграции:
```yaml
services:
  billing-service:
    image: my-app:latest
    depends_on:
      - tsdb
      - zookeeper
      - kafka
    # Последовательное ожидание в bash: долго и требует netcat/curl внутри контейнера
    command: >
      sh -c "
        while ! nc -z tsdb 5432; do sleep 1; done &&
        while ! nc -z zookeeper 2181; do sleep 1; done &&
        while ! (curl -s http://kafka:9644/v1/status/ready | grep -q ready); do sleep 1; done &&
        ./start-billing.sh
      "
```

##### После миграции на Zdun:
```yaml
services:
  billing-service:
    image: my-app:latest
    depends_on:
      - tsdb
      - zookeeper
      - kafka
    # Параллельная проверка всех зависимостей с общим таймаутом 60 секунд
    command: >
      zdun
        --tcp tsdb:5432
        --tcp zookeeper:2181
        --http "ready@http://kafka:9644/v1/status/ready"
        -t 60s
        -- ./start-billing.sh
```

---

#### 2. Миграция из GitLab CI (`.gitlab-ci.yml`)

##### До миграции:
```yaml
test-billing:
  stage: test
  services:
    - name: tsdb:latest
      alias: tsdb
    - name: redpanda/redpanda:latest
      alias: kafka
    - name: wiremock/wiremock:latest
      alias: mock-server
    - name: temporalio/auto-setup:latest
      alias: temporal
  before_script:
    - apk add --no-cache netcat-openbsd curl
    - while ! nc -z tsdb 5432; do sleep 1; done
    - while ! (curl -s http://kafka:9644/v1/status/ready | grep -q ready); do sleep 1; done
    - while ! curl --silent --fail http://mock-server:8000/__admin/health; do sleep 1; done
    - while ! curl --silent --fail http://temporal:7243/api/v1/namespaces/default; do sleep 1; done
  script:
    - ./gradlew test
```

##### После миграции на Zdun:
```yaml
test-billing:
  stage: test
  services:
    - name: tsdb:latest
      alias: tsdb
    - name: redpanda/redpanda:latest
      alias: kafka
    - name: wiremock/wiremock:latest
      alias: mock-server
    - name: temporalio/auto-setup:latest
      alias: temporal
  script:
    # Все проверки выполняются одной строкой параллельно
    - zdun
        --tcp tsdb:5432
        --http "ready@http://kafka:9644/v1/status/ready"
        --http http://mock-server:8000/__admin/health
        --http http://temporal:7243/api/v1/namespaces/default
        -t 45s
        -- ./gradlew test
```

---

## Преимущества перехода

1. **Скорость**: Проверки выполняются **асинхронно и параллельно**. Если 5 сервисов поднимаются 10 секунд, `zdun` дождется всех за 10 секунд, а не за 50 секунд последовательных циклов `while`.
2. **Чистые образы**: В итоговые Docker-контейнеры больше не нужно устанавливать `netcat`, `curl`, `grep` или `bash`. Достаточно скопировать один бинарник `zdun`.
3. **Безопасность по таймауту**: Скрипты `while ! nc` без явного таймаута могут подвесить CI-пайплайн на часы при падении контейнера. `zdun` гарантированно завершит работу с ошибкой по истечении `-t`.
4. **Понятная диагностика**: При таймауте `zdun` явно выводит список сервисов, которые не ответили:
   ```text
   [zdun] Some checks failed: tsdb:5432 kafka:9644/v1/status/ready
   ```

---

## Сборка и установка

```bash
# Сборка проекта
stack build

# Установка бинарника в ~/.local/bin
stack install
```
