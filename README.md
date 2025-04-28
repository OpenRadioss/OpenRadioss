# OpenRadioss

## What is OpenRadioss?

**Altair® Radioss®** is an industry-proven analysis solution that helps users evaluate and optimize product performance for highly nonlinear problems under dynamic loadings. For more than 30 years, organizations have used Altair Radioss to streamline and optimize the digital design process, replace costly physical tests with quick and efficient simulation, and speed up design optimization iterations.

**OpenRadioss** is the publicly available open-source code base that a worldwide community of researchers, software developers, and industry leaders are enhancing every day. OpenRadioss is changing the game by empowering users to make rapid contributions that tackle the latest challenges brought on by rapidly evolving technologies like battery development, lightweight materials and composites, human body models and biomaterials, autonomous driving and flight, as well as the desire to give passengers the safest environment possible via virtual testing.

With OpenRadioss, scientists and technologists can focus their research on a stable code base under professional maintenance that benefits from the large library of existing finite element capabilities and the continuous integration and continuous development tools provided to contributors.

For more information on the OpenRadioss project, please visit [www.openradioss.org](https://www.openradioss.org)

If you have any questions about OpenRadioss, please feel free to contact <webmaster@openradioss.org>.

## How to Use OpenRadioss

* [Quick Start guide](doc/Getting_started.md)
* [How to Build OpenRadioss](HOWTO.md)
* [How to Run OpenRadioss](INSTALL.md)
* [OpenRadioss Stable Releases](RELEASES.md)

## Community and Ways to Participate

`git` and `git-lfs` are needed to clone the OpenRadioss repository.

* [How to contribute](CONTRIBUTING.md)
* [How to access the stable version of the code](Stable_code.md)
* [Code of conduct](CODE_OF_CONDUCT.md)

Community Manager
Marian Bulla
<communitymanager@openradioss.org>  

## OpenRadioss GUI

Launch OpenRadioss using the [openradioss_gui](doc/openradioss_gui.md) tool

## Input Deck Support

* .rad file native Radioss format, read in Starter
* .k, .key LS-Dyna format. Native support in Starter.
* .inp : Abaqus and other solver. Converter with [.inp format to Radioss (.rad) format converter](https://github.com/OpenRadioss/Tools/tree/main/input_converters/inp2rad)

## Post Processing tools

Tools are available to convert Radioss formats to VTK, CSV or d3plot

* [Animation files to VTK](https://github.com/OpenRadioss/Tools/tree/main/output_converters/anim_to_vtk)
* [Time History file](https://github.com/OpenRadioss/Tools/tree/main/output_converters/th_to_csv)
* Animation to d3plot converter can be found on [Vortex-CAE GitHub repository](https://github.com/Vortex-CAE/Vortex-Radioss)

## Resources

Online Help Documentation:

* [Radioss online help](https://help.altair.com/hwsolvers/rad/index.htm)

Help Documentation in pdf form:

* [reference guide](https://2022.help.altair.com/2022/simulation/pdfs/radopen/AltairRadioss_2022_ReferenceGuide.pdf)  
* [user guide](https://2022.help.altair.com/2022/simulation/pdfs/radopen/AltairRadioss_2022_UserGuide.pdf)  
* [theory manual](https://2022.help.altair.com/2022/simulation/pdfs/radopen/AltairRadioss_2022_TheoryManual.pdf)  

[![Current status](https://github.com/OpenRadioss/OpenRadioss/actions/workflows/prmerge_ci_main.yml/badge.svg)](https://github.com/OpenRadioss/OpenRadioss/actions/workflows/prmerge_ci_main.yml)

Help for contributors:

* [OpenRadioss documentation](https://openradioss.atlassian.net/wiki/spaces/OPENRADIOSS/pages/1016047/OpenRadioss+Documentation)
