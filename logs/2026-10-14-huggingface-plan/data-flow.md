# Data flow

Derived from [data-flow.yaml](data-flow.yaml).

All requests encode validated identifiers and query values before entering the
shared transport. Non-success status codes become explicit errors with bounded
bodies. Successful JSON is decoded into public records; file bodies instead
remain scoped to the download callback. The client has no background requests,
credential discovery, disk cache, or filesystem writes.

Pagination resolves Link URLs against the final response request. Continuations
must retain the configured Hub origin. Inference POSTs are never redirected,
and the owned manager disables automatic connection retries.
