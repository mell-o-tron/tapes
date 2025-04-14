## Tapes

A tool for manipulating and drawing tape diagrams.

![An example of TikZ output.](./imgs/sample.png)

### Current features

- Simple language for specifying terms of a sesquistrict rig signature
- Typechecker for these terms
- Conversion of terms to tape diagrams
- Generation of TikZ code for rendering tape diagrams

### Usage

The executable takes as command line argument the path to a text file containing the term. For quick tests, edit the file in `./formula` and execute `make run`.