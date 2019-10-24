# Mungo-Typechecker

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
