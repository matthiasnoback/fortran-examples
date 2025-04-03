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

- Cmake Tools (provides a UI for interacting with CMake targets)
- Modern Fortran (understands Fortran code and gives code completion based on input from the Fortran Language Server, etc., runs the compiler as a linter)
- Trigger Task on Save (allows auto-formatting with fprettify and running the Fortitude linter on-save)
- EditorConfig for VS Code (automatically deals with whitespacing)
- CMake IntelliSense (formatting, auto-complete in CMake configuration)
- CTest Lab (integrates CTests with the Test panel so you can run tests in your IDE)
- Python (helps with writing Python code for the output visualization scripts)

CMake Tools may ask for a "CMake configure preset". Pick "ifx". In case you picked the wrong one or ever need to manually select it: press `Ctrl + Shift + P` to find a command, and look for the command: "CMake: Select configure preset".

The Modern Fortran extension requires the `fortls` language server, which comes installed with this project as a Python executable. Go to `File` - `Preferences` - `Settings` - `Extensions` - `Modern Fortran` - `Language server`. Make sure to select the `User` tab, not the `Workspace` tab. Then for the setting `Fortran > Fortls: Path` provide the full path to `fortls.exe`, e.g.

```
C:\Users\[location of the repository]\venv\Scripts\fortls.exe
```

(Unfortunately it doesn't work with a relative path)

`fprettify` will be executed on save, and to ensure that the Modern Fortran doesn't try to reformat the code too, we disable formatting under `File` - `Preferences` - `Settings` - `Extensions` - `Modern Fortran` - `Formatting`.

## Building and running

From the CMake panel (you'll find an icon on the left of the screen), you can select any of the project's targets that you want to build, run, or debug. The menu (`...`) shows specific actions like Reconfiguring, Rebuilding, Cleaning, etc.

### Debugging

Unfortunately, debugging doesn't work out-of-the-box with CMake tools. To Debug, make sure to select a Launch target in the CMake panel on the left. Then choose Run -> Start Debugging.

The debugger doesn't show the correct contents of variables. To fix this (yes, this is incredibly silly, I just read it [somewhere](https://gist.github.com/albertziegenhagel/6431811950864bd0009b6a1fa78e7f2b)), take the following steps.

- Open this folder, inside your oneAPI installation directory: C:\Program Files (x86)\Intel\oneAPI\debugger\latest\share\ide_support\visual_studio\debugger\vs2022
- Copy the file FEE_VSIX_v17.vsix to your home directory, e.g. C:\Users\noback
- Rename the file to FEE_VSIX_v17.zip, then right-click and `Extract all...`
- Open the extracted folder and create a file in this folder called `.vsdbg-config.json`
- Copy the following text into this file:

```json
{
  "$schema": "https://aka.ms/vs/vsdbg-config-schema",
  "languages": [
    {
      "languageId": "{8e546c6f-4819-4dde-83dd-f998d52e6f33}",
      "vendorId": "{2a333b19-f91e-477b-8032-22de549d925a}",
      "name": "Fortran",
      "codeViewCompilerIds": [ { "code": 2 } ]
    }
  ]
}
```

- Create a `.cppvsdbg\extensions` in your home directory (e.g. `C:\Users\noback\.cppvsdbg\extensions`).
- Create a file called `FEE.link` in this new directory. The contents should be the full path to the unzipped directory, e.g. `C:\Users\noback\FEE_VSIX_v17`.
- Next time you do a Debug launch, the Debug console panel should show:

```
------------------------------------------------------------------------------
You may only use the C/C++ Extension for Visual Studio Code with Visual Studio
Code, Visual Studio or Visual Studio for Mac software to help you develop and
test your applications.
------------------------------------------------------------------------------
Loading extensions from 'C:\Users\noback\FEE_VSIX_v17'.
[...]
```

## Fortitude linter

If you've installed the "Trigger Task on Save" extension, Fortitude will check the file immediately after saving it. Any issues will be reported in the file itself, but also in the `Problems` panel.

To check all files at once, run inside a terminal:

```bash
.\venv\Scripts\fortitude.exe check --fix -- src test
```

## Plotting

If your program outputs x/y points on each line, you can pass the output to `plot_points.py` and generate a nice graph for it, e.g. after building the `example_plot_function_to_be_integrated` target, run:

```bash
.\out\build\ifx\src\examples\Debug\example_plot_function_to_be_integrated.exe | python .\plot_points.py
```
