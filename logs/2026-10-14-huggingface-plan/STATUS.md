# Execution status

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
