# Execution status

- p5: verified — replace handwritten text/header parsing with Parsec
  grammars over Text and ByteString. Retain opaque domain types, redacted
  failures, numeric smart constructors, and manual validation without hooks.
  Added `parsec >=3.1.16 && <3.2`; locally resolved 3.1.18.0. All 92 offline
  tests pass with `-Werror` on GHC 9.14.1, including full consumption, name
  boundaries, policy/provider prefixes, non-BMP paths, origin ports, quoted
  Link parameters, and empty HTTP list members. `scripts/validate.sh` passes,
  all public modules retain 100% Haddock coverage, and HLint reports no hints.
  The new internal parser module is present in the source archive; the
  independently unpacked package builds and passes all 92 tests. Public types
  and constructors are unchanged. No hooks were installed.

- p4: verified — user requested distinct domain types, smart constructors,
  Text rather than String, and parsing into types at input boundaries. Parsed
  endpoints retain their request representation; routed models retain their
  repository and routing choice. Returned identifiers use the same types as
  inputs. Free-form content and extension JSON remain open. No unsafe IsString,
  Read, or exported record selectors for opaque newtype representations.
  All 78 offline tests pass on GHC 9.14.1 with `-Werror`, including JSON input
  boundaries, typed response reuse, endpoint port overflow, and HF/TGI wire
  compatibility. `scripts/validate.sh` passes; every public module has 100%
  Haddock coverage. An independently unpacked source archive builds and passes
  the same 78 tests. All three README snippets compile with `-Wall -Werror`;
  eleven negative compilation checks reject representation bypasses and
  invalid API input types. `hlint src examples` reports no hints. Four focused
  review findings were fixed and covered; see `security.md`.

- User override: all pre-commit hooks removed, local `core.hooksPath` unset,
  and no active hook exists in `.git/hooks`. `AGENTS.md` forbids installing
  pre-commit hooks. Manual validation and CI remain available.

The p1–p3 entries below record the initial implementation's verification.

- p1: verified — all 66 offline tests pass on GHC 9.14.1, including the
  dropped-response/no-POST-replay regression. Library, tests, and compiled
  example build with `-Werror`.
- p2: verified — `scripts/validate.sh` completes; Cabal check is clean;
  Haddock reports 100% documentation coverage in every public module;
  source tarball independently builds and passes all 66 tests. All three
  README Haskell snippets typecheck with `-Wall -Werror`. CI YAML parses.
- p3: verified — focused security review completed and all confirmed
  findings resolved; see `security.md`.

## Additional verification

- `hlint src examples`: no hints.
- Mutation check in a disposable extracted archive: restoring transport retries
  makes exactly the no-replay test fail (two POSTs rather than one). The modified
  source was restored; the repository source was never changed by this check.
- Live smoke test: the compiled example performed an unauthenticated public
  model search and returned ten Hub model IDs. No paid inference was invoked.
- GHC 9.6.6 dependency solving succeeds. Only GHC 9.14.1 was compiled locally;
  the committed five-version CI matrix has not run remotely.
- Nix is not installed, so `default.nix` was not evaluated locally.
- GitHub and Hackage publication were outside the requested local setup.
