
# Cl-Maxsat - Common Interface to MAX-SAT Solvers from Common Lisp

## Usage

The API extends the `SOLVE` generic function in [CL-SAT](https://github.com/guicho271828/cl-sat).
The format for denoting the logic formula is equivalent.
The extension allows `SOLVE` function to take an additional `:soft-clauses` keyword argument,
which is a list of form `((<weight> <logical form>)...)`.

Example:

```
(solve '(and (or a b) (or (not e) d) (or (not d) c))
       :maxsat-competition
       :soft-clauses
       '((1 (and d c))
         (2 (and b (not e))))
       :year 2017 :track :complete :name :maxhs)
```

## Available solvers

Similar to CL-SAT, this library supports various competition winners from the previous MaxSAT competitions.
When `solver-designator` argument is `:maxsat-competition`, `solve` function accepts `:year` `:track` `:name` arguments
and try to download and build the specified solver if it was not done before.

The zip-compressed source codes distributed at the competition sites do not follow the consistent build procedure
unlike SAT competitions. Some fails to compile on the newer GCC (e.g. those in Ubuntu 18.04).
However, we patch up the build scripts so that they build successfully.

Note that a couple of solvers require MILP solvers and the patches are hard-coding that we will use IBM CPLEX.
CPLEX binary should be visible in the PATH when using those solvers.
However, not all solvers rely on CPLEX, and also some solvers allow switching between CPLEX and other solvers (e.g. Gurobi, Xpress).
If there are such needs we may support them.

+ [MaxSAT Evaluation 2017](https://maxsat-evaluations.github.io/2017/)
  + track `:complete`
    + name `:LMHS`  --- requires CPLEX
    + name `:MaxHS` --- requires CPLEX
    + name `:Open-WBO`
    + name `:maxino`
    + name `:QMaxSAT`
    + name `:QMaxSATuc`
  + track `:incomplete`
    + name `:LMHS-inc`  --- requires CPLEX
    + name `:MaxHS-inc` --- requires CPLEX
    + name `:Open-WBO-LSU`
+ [MaxSAT Evaluation 2018](https://maxsat-evaluations.github.io/2018/)
  + not supported yet

## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.4.10 on X86-64 Linux 4.15.0-43-generic (author's environment)

Also, it depends on the following libraries:

+ cl-sat :
    
+ trivia by *Masataro Asai* :
    NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility

## Installation

## Author, License, Copyright

Licensed under LGPL v3.

Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
