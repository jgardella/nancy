# Nancy

Nancy is a simple history-aware programming language.

As reduction proceeds, a trail describing reduction is maintained. This trail can be inspected
during the program to produce a value.

## Build

`stack setup` to install GHC.

`stack install` to build executable.

## Running Nancy Programs

Example programs are provided in the `examples` folder. You can interpret them with the command:

```
nancy examples/example.nc
```

To only parse or typecheck the program, pass the `--mode` (`-m`) option:

```
nancy --mode Parse examples/example.nc
nancy --mode Typecheck examples/example.nc
nancy --mode Interpret examples/example.nc
```

To disable pretty printing and print using Haskell's `Show` typeclass, pass the `--show` (`-s`) flag:

```
nancy --show examples/example.nc
```

To print trails in prose mode, pass the `--prose` flag:

```
nancy --prose examples/example.nc
```

If a filename is not passed, Nancy will provide a REPL:

```
nancy
```
