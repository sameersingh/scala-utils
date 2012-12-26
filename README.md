scala-utils
===========

Multiple small Scala utils for timing, evaluation, etc.

# Installation

```
    <dependency>
      <groupId>org.sameersingh.utils</groupId>
      <artifactId>${util.name}</artifactId>
      <version>${utils.version}</version>
    </dependency>
```

where `util.name` is one of `cmdopts`, `misc`, `coref`, `termutils`, or `timing`, while `utils.version` is `0.1.1-SNAPSHOT`.

cmdopts: Command-line Options Parsing
------------------------------------

coref: Represent and evaluatate Coreference (and clustering)
------------------------------------------------------------

### EntityMap[T]

termutils: Pretty printing on the terminal
------------------------------------------

timing: Time spent in different methods
---------------------------------------

### Snapshotter

Allows maintenance of timing with simple features such as pause/resume and snapshots (think "laps" on stopwatches).

misc: Other misc. utils
-----------------------

Currently contains the following:

### PairMap[T]

Efficient representation of a double dimensional "array", i.e. map from (i,j)->T

### Alphabet

Map of strings to ints, and reverse

### Counter

Simple counter that can be disabled to that updates have no effect
