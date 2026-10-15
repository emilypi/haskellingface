# Hugging Face bindings — 2026-10-14

Build a new local repository in `~/haskell` using the established emilypi
Haskell package conventions. The user confirmed Hub API plus hosted inference.

## Discovery and decisions

See the [decision registry](registry.md) for requirements, constraints, and
resolved scope questions.

## Sources

- `../base64/base64.cabal`, `.github/workflows/haskell-ci.yml`, `LICENSE`,
  `CONTRIBUTING.md`; `../base64-lens/base64-lens.cabal`, `test/Main.hs`.
- https://huggingface.co/docs/hub/api
- https://huggingface.co/.well-known/openapi.json
- https://github.com/huggingface/huggingface_hub/blob/main/src/huggingface_hub/hf_api.py
- https://github.com/huggingface/huggingface_hub/blob/main/src/huggingface_hub/inference/_providers/hf_inference.py
- https://huggingface.co/docs/inference-providers/en/tasks/chat-completion
- https://huggingface.co/docs/inference-providers/en/tasks/feature-extraction
- https://huggingface.co/docs/inference-providers/en/tasks/text-generation

## Deliverables

- [Architecture and data flow](architecture.yaml)
- [Contracts](contracts.yaml)
- [Work packages](packages.yaml)
- [Verification status](STATUS.md)
- [Completion summary](SUMMARY.md)
- [Focused security review](security.md)
