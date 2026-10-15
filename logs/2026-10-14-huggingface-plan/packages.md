# Work packages

Derived from [packages.yaml](packages.yaml). Execution state is in [STATUS.md](STATUS.md).

```mermaid
flowchart LR
  p1[Library and offline tests] --> p2[Distribution and documentation]
  p2 --> p3[Credential and input review]
```

Implementation is solo. A focused review agent checks the security boundaries
as required by the planning skill, with findings and resolutions in [security.md](security.md).
