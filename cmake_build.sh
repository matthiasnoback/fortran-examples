#!/usr/bin/env bash

source /opt/intel/oneapi/setvars.sh --force

set -eu

cmake -B ./cmake-build-debug-system

export PATH="${PATH}:./install/bin/"
