# Getting started

## Install

### Cmake

Install CMake [Download here](https://cmake.org/download/)

### Intel Fortran compiler

You can install Intel OneAPI Fortran Essentials [Download here](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html) or Visual Studio (which you probably already have installed).

### Python 3.13

[Download here](https://www.python.org/downloads/)

### Visual Studio Code

During the workshop we'll work in the Visual Studio Code IDE [Download here](https://code.visualstudio.com/download). 

After installing, run it, and open the folder where you've cloned this project to.

Open the `Extensions` panel on the left (or press `Ctrl + Shift + X`). Then install the following extensions:

- Cmake Tools
- Modern Fortran
- Python
- Trigger Task on Save

CMake Tools may ask for a "CMake configure preset". Pick "ifx". In case you picked the wrong one or ever need to manually select it: press `Ctrl + Shift + P` to find a command, and look for the command: "CMake: Select configure preset".

The Modern Fortran extension requires the `fortls` language server, which comes installed with this project as a Python executable. Go to `File` - `Preferences` - `Settings` - `Extensions` - `Modern Fortran` - `Language server`. Make sure to select the `User` tab, not the `Workspace` tab. Then for the setting `Fortran > Fortls: Path` provide the full path to `fortls.exe`, e.g.

```
C:\Users\[location of the repository]\venv\Scripts\fortls.exe
```

(Unfortunately it doesn't work with a relative path)

The same goes for `fprettify` which is installed as a Python dependency of this project. Go to `File` - `Preferences` - `Settings` - `Extensions` - `Modern Fortran` - `Language server`. Make sure to select the `User` tab, not the `Workspace` tab. Then for the setting `Fortran > Formatter: Path` provide the full path to `fprettify.exe`, e.g.


```
C:\Users\[location of the repository]\venv\Scripts\fprettify.exe
```

## Building, running, debugging

From the CMake panel (you'll find an icon on the left of the screen), you can select any of the project's targets that you want to build, run, or debug. The menu (`...`) shows specific actions like Reconfiguring, Rebuilding, Cleaning, etc.

## Fortitude linter

If you've installed the "Trigger Task on Save" extension, Fortitude will check the file immediately after saving it. Any issues will be reported in the file itself, but also in the `Problems` panel.

To check all files at once, run inside a terminal:

```bash
.\venv\Scripts\fortitude.exe check --fix -- src test
```
