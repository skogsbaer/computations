# A framework for coarse-grained incremental computations in Haskell

More documentation to come ...

## Building

Requirements: you need the build tool `stack` (https://docs.haskellstack.org/en/stable/).
I use version 2.9.3, I guess a slightly older version might work was well.

```
stack build
```

## Tests

```
stack test
stack run -- test
```

## Demo: Directory sync

This demo synchronizes the content of one directory to another. The
sync work recursively and continuously.

[Source code](app/Control/IncComps/Demos/DirSync)

Start the sync from directory A to directory B. Directory A must exist,
B is created automatically.

*Attention: everything in B that does not exist in A will be deleted!*

```
stack run -- --log-level info sync --source A --target B
```

## Demo: Hospital

This demo show how a processing pipeline for hospital data might look like.
The demo is very basic. It supports admission and discharge of patients, as well
textual notes being added to a patient. A simulation creates a stream of events
driving the demo.

[Source code](app/Control/IncComps/Demos/Hospital)

Start the simulation:

```
stack run -- --log-level debug hospital-pipeline --config hospital-config/ --root run-data
```

Start the pipeline:

```
stack run -- --log-level info hospital-simulation --root run-data
```

Now output files should appear in `run-data/output`.

A frontend is currently under development.
