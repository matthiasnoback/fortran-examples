#!/usr/bin/env bash

source /opt/intel/oneapi/setvars.sh --force || true

set -eu

build_dir=./cmake-build-debug-system

cmake --build "${build_dir}" -v
cmake --install "${build_dir}"
