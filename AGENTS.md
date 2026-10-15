# huggingface

Haskell2010 library for Hugging Face Hub and hosted inference. Match the
packaging and code conventions in emilypi/base64 and emilypi/base64-lens.

- All library modules are exposed by Cabal, including `Internal` modules.
  Reserve `Internal` for transport implementation details; keep domain logic
  with its public types and shared helpers in `HuggingFace.Utils`.
- Use explicit exports and Haddock module headers with Module, Description,
  Copyright, License, Maintainer, Stability, and Portability fields.
- Keep dependency bounds, `tested-with`, and the CI matrix aligned.
- Read `/testing-patterns` before writing tests and `/tech-ref-git` for Git work.
  There is no Haskell style skill in the current skill catalog.
- Keep tests offline with a local HTTP server. Never require credentials or
  invoke billable inference in tests. Never log bearer tokens.
- Validate with `scripts/validate.sh`; inspect the source tarball as well.
- Do not install pre-commit hooks. Run validation explicitly or in CI.
- Put session plans and evidence in `logs/`; do not include them in the package.
- Keep examples compilable. Document unsupported API surfaces honestly.
