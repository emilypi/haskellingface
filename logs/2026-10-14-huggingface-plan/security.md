# Focused client security review — 2026-10-14

Static review of `Client`, `Types`, `Hub`, `Inference`, `Internal.HTTP`, and
`Internal.Types`, against this plan's contracts and the local API-security and
OWASP baseline references. No source changes, Cabal commands, live credentials,
or billable requests were used. The remediation review below checks the current
source; the main implementation task reports 66 passing offline tests. This
review independently inspected the relevant regression definitions but did not
rerun the suite.

All three confirmed findings are resolved. Revision component validation is
also corrected. No new security issue was identified in these changes.

## Resolved findings

1. **P1 — Resolved: the default transport could replay inference POST requests.**
   The initial `src/HuggingFace/Client.hs` created the manager with unmodified
   `tlsManagerSettings`, while `Inference.hs` promised no automatic retry.
   The resolved `http-client-0.7.19` source, `Network/HTTP/Client/Core.hs`
   (`requestAction`), retries a request after a retryable exception on a reused
   connection, without checking the method. The guarded operation includes both
   sending the request and reading response headers. Its default manager retries
   `IOException`, `NoResponseDataReceived`, and `IncompleteHeaders`;
   `http-client-tls-0.3.6.4` also retries TLS EOF. A POST accepted by the server
   whose response is lost can therefore be submitted again, potentially charging
   the caller twice. `Client.hs:41` now sets
   `managerRetryableException = const False` on the owned manager.
   `Client.hs:44–48` and the README explicitly require callers to disable
   transport retries on a custom manager. The
   dependency sources were read from the local Cabal source archives; the
   resolved versions were checked in `dist-newstyle/cache/plan.json`. The
   owned-manager change is verified by inspection. Subsequently, the main task
   added and ran the dropped-response regression described below.

2. **P2 — Resolved: relative pagination after a redirect used the wrong base URI.**
   The initial `Internal/HTTP.hs:68–74` passed the original request to `continuation`, while
   `withBody` can follow same-origin redirects before returning headers. If
   `/api/models` redirects to `/v2/models` and the response contains
   `Link: <?cursor=next>; rel="next"`, the client requests `/api/models?cursor=next`
   instead of `/v2/models?cursor=next`. `Internal/HTTP.hs:63` now preserves
   `getOriginalRequest response`, and lines 72–74 resolve links against that
   final request. Each manual redirect creates a separate response, so this
   accessor supplies the correct final URI. The regression at `test/Main.hs:188`
   covers a redirected request and query-only continuation. The same-origin guard remains intact;
   this is a pagination correctness defect, not demonstrated credential leakage.

3. **P3 — Resolved: callback exception documentation overstated propagation.**
   The initial `Types.hs:126` said callback exceptions propagate, but `Internal/HTTP.hs:89–110`
   catches `HttpException` around both transport and callback execution. An
   `HttpException` raised by a callback's own HTTP work becomes a generic
   `TransportError`. `Types.hs:125–127` and the README now document propagation
   for asynchronous and non-HTTP callback exceptions. This matches the catch
   scope. The regressions at `test/Main.hs:257` cover non-HTTP callback exceptions
   and asynchronous cancellation.

## Hardening observations

- **Resolved:** `Types.hs:54` now validates every slash-separated revision
  component; `test/Main.hs:105` rejects `../main`, `refs/../main`, `/main`, and
  `main/`. Valid branches such as `refs/pr/42` remain supported and are encoded
  as one URI segment. No traversal exploit had been established against Hugging
  Face or a particular proxy in the initial review.
- `Internal/HTTP.hs:60–63` buffers successful JSON responses without a byte cap.
  Error bodies are correctly capped at 64 KiB. Consider a configurable decoded
  response limit for applications requiring bounded memory. The README now
  states this limitation; `downloadFile` explicitly buffers, while
  `withDownload` offers the streaming alternative.
- Cross-origin download destinations are trusted after stripping credentials.
  They are not restricted to public addresses or a CDN allowlist. This is a
  deliberate service/redirect trust boundary, rather than an unrestricted
  caller-supplied URL API; deployments needing strict egress control must enforce
  that separately. A custom manager also controls proxies, TLS, and hooks.

## Controls inspected

- Token and continuation constructors are hidden; their `Show` instances are
  redacted. `ClientConfig` inherits token redaction. Error `Show` omits response
  body/headers and transport requests, including server-echoed secrets.
- Token validation blocks control characters and header injection. Repository
  identifiers and file paths are validated; path segments and query values are
  encoded separately. The library chooses no local filesystem destinations.
- Default endpoints use HTTPS. Configured plaintext origins are limited to
  explicit loopback names. Redirects reject userinfo, unsupported schemes,
  HTTPS downgrades, and cross-origin API destinations. Download redirects drop
  authorization/cookies when the origin changes; stripped credentials are not
  restored later in a redirect chain. The redirect count is bounded.
- Pagination handles remain opaque and are checked against the consuming
  client's Hub origin before adding that client's bearer token.
- Only GET redirects are followed. Chat extension fields cannot override model,
  messages, sampling validation, or the enforced `stream = false` field.
- `withResponse` scopes response lifetime, including exceptions and redirects.
  No model code executes locally, no shell is invoked, and JSON decoding does
  not execute server-supplied content.

## Transport retry regression

The implementation now includes `lost response on a reused connection does not
replay a billed POST` in `test/Main.hs`. It warms up the owned manager with a
Hub GET, then uses WAI's raw response to close the connection after consuming
the complete POST, before returning headers. It asserts `TransportError` and
exactly one received POST. This test passed in the main task's 66-test run.
The more explicit raw-socket fixture described below is an alternative design.
The main task subsequently enabled retries in a disposable unpacked archive:
exactly this regression failed, observing two POSTs instead of one. The fixture
therefore detects the original risk. The temporary source was restored.

Use `newClient` with a raw loopback HTTP server, ensuring the test exercises the
owned manager rather than the test suite's separately configured custom manager.

1. Accept one connection and serve a warm-up Hub GET with a complete HTTP/1.1
   response, `Content-Length`, and keep-alive behavior. Let the client fully
   consume its body so the connection returns to the pool.
2. Read the next complete inference POST, including its `Content-Length` body,
   on that same socket. Record the request before closing the socket without
   sending response bytes. Closing only after receipt removes the stale-socket
   timing race and triggers the exact reused-connection/no-response case.
3. Keep the listening socket available in a server worker. If a retry opens a
   new connection, read and count that POST and return valid chat JSON. With the
   old policy this yields a successful replay; the corrected client instead
   returns `Left (TransportError ...)` and the POST count remains one.
4. Assert the failed result and one POST after the call returns. Transport retry
   is synchronous, so no sleep is needed to prove absence of replay. Bracket all
   sockets/workers and use a bounded outer timeout to surface fixture failures.

## Untested and out of scope

The main task reports 66 offline tests passing after remediation, including
the dropped-response regression and a live unauthenticated Hub search. This
review did not independently run TLS/proxy/DNS integration tests,
measure resource exhaustion, fuzz URI/link parsing, scan dependency advisories,
or exercise real Hub/provider endpoints. Server-side authentication,
authorization, rate limiting, and billing behavior are outside this client
review. No claim of complete security coverage is made.
