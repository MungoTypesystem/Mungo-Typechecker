# Mungo-Typechecker
This repository contains the implementation of a type checker for a version of the Mungo language. The Mungo language was originally developed by the [ABCD group](http://www.dcs.gla.ac.uk/research/mungo/index.html) at University of Glasgow. 

This is an implementation for a alternative version of Mungo, presented in [this paper](https://arxiv.org/abs/2002.12793) by Mario Bravetti, Adrian Francalanza, Iaroslav Golovanov, Hans Hüttel, Mathias Steen Jakobsen, Mikkel Klinke Kettunen and António Ravara.

Examples can be found in the `ExamplePrograms` directory or presented on the [example page](https://mungotypesystem.github.io/Mungo-Typechecker/).

## Requirements
- Haskell (`ghc`)
- `stack`


## Setup
1. Clone the repository 
```bash
git clone https://github.com/MungoTypesystem/Mungo-Typechecker
```
2. Setup project
```bash
cd Mungo-Typechecker
stack install
```
3. Build
```bash
stack build
```

## Usage

```bash
stack run <input file>
```

e.g. 

```bash
stack run ExamplePrograms/FileExample3.mg
```

If the output is `Right ()` then the program was typechecked correctly, otherwise the error will be indicated in a `Left String` expression.
