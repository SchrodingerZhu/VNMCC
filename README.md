# Very Naive MIPS CPU using Clash

![logo](document/image/logo.png)

In this project, we implement a very naive CPU with a 5-stage pipline that supports a tiny subset of MIPS instructions. This is of course not a functioning CPU in the real world; its main design purpose is to provide a clear logic and an overview of how a piplined CPU works.
We also hope that this project can behave as another tutorial for the Clash language and shows its power and beauty in hardware design. With the help of Clash, we can largely reduce the complexity to implement our toy CPU.

With the goals beared in mind, we also maintain [a book](document/VNMCC.pdf) that provides basic knowledge of Verilog HDL, Clash, and the CPU with pipeline: how to implement it and how to handle the hazards. There is another important reason making us feel it necessary to provide this book, that is currently some universities require students to write a CPU as their term projects, however, it seems common that most students are not provided with adequate materials; they do even have the experience to write a sequential logic circuit before. There will be far more problems than they may expect. Hence, by providing a hand-by-hand tutorial in this book, we hope that students learning about microprocessors can have more ease and joy.

## Acknowledgements

The book and the project cannot be finished without the help from some excellent people. We must give our acknowledgments to them.

Thanks **Prof. Ye-Ching Chung** <ychung@cuhk.edu.cn> for his lectures and guidance on microprocessor architectures and instruction sets.

Thanks **dramforever** <dramforever@live.com> for the template repository and several important corrections of our technical details, mainly on the installation part and the descriptions of several Clash functions.

Thanks **Mickey Ma** <mickeyma@cuhk.edu.cn> for her kind and patient advice on the details of the CPU design.

Thanks **Outvi V** <i@outv.im> for providing some materials of GtkWave and good suggestions on hazards handling.

Thanks **ice1000** <ice1000kotlin@foxmail.com> for the suggestions on Haskell code-style and logic structures.

## License 

The Book for Very Naive MIPS using Clash by Schrodinger ZHU is licensed under CC BY 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by/4.0. (including all the LaTex contents and related media files created in this repository).

![CC-BY](https://search.creativecommons.org/static/img/cc-by_icon.svg)

Other components in this repository are under MIT license.
