# Getting started

## Install

### Intel Fortran compiler

You can install Intel OneAPI Fortran Essentials [Download here](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html) or Visual Studio (which you probably already have installed).

### Python 3.13

[Download here](https://www.python.org/downloads/)

After installing, create a Python virtual environment in the project directory, and install the required dependencies locally:

```bash
python -m venv venv
.\venv\Scripts\python.exe -m pip install --upgrade pip
.\venv\Scripts\python.exe -m pip install -U -r requirements.txt
```

### Visual Studio Code

During the workshop we'll work in the Visual Studio Code IDE. Open the `Extensions` panel on the left (or press `Ctrl + Shift + X`). Then install the following extensions:

- Cmake Tools
- Modern Fortran
- Python

To select the `ifx` compiler instead of `ifort`, go to File - `Settings` - `Extensions` - `Cmake Tools` and find the setting `Cmake: Configure Args`. Add an item:

```
-T fortran=ifx
```

Now go to the CMake panel on the left and in the `Project outline` panel select from the menu (`...`) `Clean Reconfigure All Projects` and the `Clean Rebuild All Projects`.

## Linter

```bash
.\venv\Scripts\fortitude.exe check --fix -- src test
```
