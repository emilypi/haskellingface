# Contributing

Bug reports, documentation improvements, and pull requests are welcome. Include
a small reproducer and your GHC/Cabal versions in a report. Remove tokens,
private repository data, and sensitive error bodies before sharing logs.

Run `scripts/validate.sh` before submitting a change. Tests use a local HTTP
server and must not require tokens, public network access, or paid inference.
Add focused regression tests for changed request shapes, error handling, and
credential boundaries. Match the explicit exports and Haddock module headers.

Keep dependency bounds, the CI matrix, and `tested-with` aligned. Include new
fixture files in `extra-source-files`, update the changelog for public API
changes, and verify the unpacked source distribution. Do not claim a compiler
has been tested locally unless it has actually been run.
