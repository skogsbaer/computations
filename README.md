# A framework for coarse-grained self-adjusting computations in Haskell

Ensuring that software applications
presents their users the most recent version of data is not trivial.
Self-adjusting computations are a technique for automatically and efficiently recomputing
output data whenever some input changes.

This repository contains the code of a framework for coarse-grained self-adjusting computations in Haskell. The framework has been extracted from
a commercial software product developed by medilyse GmbH, Freiburg, Germany.

The repository also contains two demo applications using the framework.

More documentation is to come.

## Building

Requirements: you need the build tool `stack` (https://docs.haskellstack.org/en/stable/).
I use version 2.9.3, I guess a slightly older version might work was well.

Build the software by executing

```
$ stack build
```

Run the tests by executing

```
$ stack test
$ stack run -- test
```

## Demo: Directory sync

This demo synchronizes the content of one directory to another. The
sync works recursively and continuously.

[Source code](app/Control/IncComps/Demos/DirSync)

To start the sync from directory `A` to directory `B`, execute the following command.
Note that `A` must exist, `B` is created automatically.

*Attention: everything in B that does not exist in A will be deleted!*

```
$ stack run -- --log-level info sync --source A --target B
```

## Demo: Hospital

This demo shows how a processing pipeline for hospital data might look like.
The demo is very basic. It supports admission and discharge of patients, as well
textual notes being added to a patient. A simulation creates a stream of events
driving the demo.

[Source code](app/Control/IncComps/Demos/Hospital)

Start the simulation:

```
$ stack run -- --log-level info hospital-simulation --root run-data
```

Start the pipeline:

```
$ stack run -- --log-level info hospital-pipeline --config hospital-config/ --root run-data
```

Now output files should appear in `run-data/output`.

You can also change data by hand. The sqlite database for patients is in
`run-data/pats.sqlite`, the database for patient notes in
`run-data/pat_notes.sqlite`.

Start the webserver:

```
stack run -- --log-level info hospital-server --out run-data/output
```

A frontend is currently under development.
