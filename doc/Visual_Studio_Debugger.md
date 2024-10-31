# How to Use Visual Studio Debugger with OpenRadioss 

## Prerequisites

* This page assumes that the build environment with Git, Visual Studio, Intel OneAPI, and the Intel OneAPI plugin for Visual Studio was properly installed.
* The procedure was tested on Visual Studio 2019 and Visual Studio 2022.

Check the [HOWTO.md](../HOWTO.md) section for the installation.

## Choose the Debug Configuration in the configuration Menus

![image](./Debug_build_configuration.png) 

## Build the Configuration

![image](./build_all.png)

The executables to use and symbol .pdb files are in the build directory

For the SMP directories:

* Starter 

      [OpenRadioss Root]\vs_build_win64\Starter_x64-Debug\starter

* Engine 

      [OpenRadioss Root]\vs_build_win64\Starter_x64-Debug\engine

## Add the directory to the pdb files in the Debugging section from the Options Menus

This needs to be done only once.
Options can be found in [Tools][Options]

![image](./tools_options.png)

Search Debugging in the list & choose Symbols:

The Pdb symbol files can be found when building the Debugging versions on:
* [OpenRadiossRoot]\vs_build_win64\Starter_x64-Debug\starter
* [OpenRadiossRoot]\vs_build_win64\Engine_x64-Debug\engine

You can add the .pdb files by clicking on the '+' icon.


![image](./Debug_symbol_setting.png)

You can now add your settings to create the Debug session. 
Set the menus to execute the model you need to debug.

## To access the settings to debug, select first the default setting.
Select the default executable in the "Startup Item Menu"
* starter_win64_db.exe for Starter.
* engine_win64_db.exe for Engine.

![image](./Startup_item.png)

You can now add your settings to create the Debug session. 
Set the menus to execute the model you need to debug.

## Select the "Debug and launch Settings" to configure the Debug run


![image](./debug_and_launch_settings_menu.png)

It is a Json file which can be modified. It contains the launch parameters and sets entries in the "Launch Item" menu.

![image](./launch_json.png)

Add an entry with:
* Input Deck directory: "currentDir" 
* execution arguments: "args"
* A title: "name"

This will permit to launch the OpenRadioss deck with the debugger.

Here are templates for Starter & Engine: 

    {
      "args": [
        "-i",
        "DECK_0000.rad",
        "-np",
        "1"
      ],
      "currentDir": "[Path To Input Deck/Launch Directory]",
      "name": "starter_win64_db.exe (starter\\starter_win64_db.exe launch NEON1M11)",
      "project": "CMakeLists.txt",
      "projectTarget": "starter_win64_db.exe (starter\\starter_win64_db.exe)",
      "type": "default"
    },
    {
      "args": [
        "-i",
        "DECK_0001.rad"
      ],
      "currentDir": "[Path To Input Deck/Launch Directory]",
      "name": "engine_win64_db.exe ( launch NEON1M11)",
      "project": "CMakeLists.txt",
      "projectTarget": "engine_win64_db.exe (engine\\engine_win64_db.exe)",
      "type": "default"
    },

## To Debug, Breakpoints are mandatory.

Breakpoints can be set in the source code on the left column.
On the source where the breakpoint should be placed, click on the left column on the line where the breakpoint should be set. A red dot appears.
The breakpoint is set.

![image](./breakpoints.png)

## Now, the debug session can be started: Select the newly created entry in the Startup Item Menu

![image](./select_engine_debug.png)

![image](./launch.png)

## The Job stopped at the Breakpoints.
The menus on top permit to continue.
Highlight with the mouse on variables to highlight values etc...

![image](./stoped_at_breadkpoint.png)
