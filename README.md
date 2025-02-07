# Getting started

## Install

Requirement: Python

```bash
python3 -m venv venv
source venv/bin/activate
pip install -U pip
pip install -U -r requirements.txt
```

Requirement: Intel OneAPI Fortran Essentials

```bash
source /opt/intel/oneapi/setvars.sh
```

## Next time

```bash
source venv/bin/activate
source /opt/intel/oneapi/setvars.sh
```

## Linter

```bash
fortitude check --fix -- src test
```
