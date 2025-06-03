#!/usr/bin/env bash

set -eu

command="${1}"
file="massif.out"

valgrind --tool=massif --time-unit=B --massif-out-file="${file}" -- "${command}"
massif-visualizer "${file}"
