#!/bin/bash

set -e

echo "=== Начало установки окружения OCaml ==="

if ! command -v opam &> /dev/null
then
    echo "Менеджер пакетов opam не найден. Устанавливаем..."
fi

if [ ! -d "~/.opam" ]; then
    echo "Инициализация opam..."
    opam init --auto-setup --disable-sandboxing -y
fi

echo "Обновление репозиториев opam..."
opam update

echo "Проверка и установка компилятора..."
opam switch create 5.1.0 --no-install || true
eval $(opam env)

echo "Установка библиотеки Lwt для сетевой работы..."
opam install lwt lwt_ppx -y

echo "=== Установка успешно завершена! ==="
echo "Пожалуйста, перезапустите терминал или выполните команду: eval \$(opam env)"