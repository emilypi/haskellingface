# Decision registry

- D-1: Package/repository name `huggingface`, initial version 0.1.0.0.
- D-2: Follow local base64/base64-lens: Cabal Simple, BSD-3-Clause, bounded
  dependencies, Haskell2010, explicit module exports, Tasty, source-tarball CI,
  README, changelog, contributor guide, Setup.hs, and a Nix expression.
- D-3: Hub reads: paginated models/datasets/Spaces, revision metadata,
  identity, and streamed file downloads. Hosted inference: non-streaming
  chat, embeddings, classification, text generation, and a JSON escape hatch.
- D-4: Explicit `Either HuggingFaceError` for client failures. Validated
  identifiers, encoded path segments, opaque tokens and continuation handles.
- D-5: No automatic POST retries. Never forward credentials across origins;
  reject HTTPS downgrades and cross-origin pagination. Downloads use callbacks
  so the library does not choose or overwrite filesystem paths.
- D-6: Offline integration tests through a loopback server; no billable calls.
- C-1: Locally installed GHC 9.14.1 and Cabal 3.18.1.0. CI covers the same five
  compiler releases as the recent base64 packages. Other versions are CI targets,
  not claimed as locally verified.
- OQ-1 resolved: packaging read directly from local emilypi repositories.
- OQ-2 resolved: API paths checked against Hugging Face OpenAPI and the official
  Python client on 2026-10-14 (see sources below).
- OQ-3 accepted scope: no upload/commit APIs, local model execution, snapshot
  cache, automatic retries, or SSE in the initial release. No remote publication
  was requested. GitLab lookup is inapplicable to this new GitHub-style project.
