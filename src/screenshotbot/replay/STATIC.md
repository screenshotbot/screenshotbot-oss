# Static Website Screenshots: Data Flow

This document traces what happens when a user runs
`screenshotbot static-website --directory <dir>` — from the SDK crawling a
local directory of HTML/CSS, all the way to a normal Screenshotbot run
showing up in the dashboard.

The client entry point is `src/screenshotbot/sdk/static.lisp`. The server
side lives mostly in this directory (`src/screenshotbot/replay/`) plus the
API handlers in `src/screenshotbot/pro/blobs.lisp`.

Note on naming: many packages here have `screenshotbot/pro/replay/...`
nicknames, but the code lives in `src/screenshotbot/replay/` (the packages
are nicknamed in their `defpackage` forms, e.g. `replay-acceptor.lisp`).

## High-level picture

```
 CLIENT (SDK, user's CI machine)
 ────────────────────────────────
 static-website command
   └─> record-static-website (sdk/static.lisp)
         1. serve <dir> on a local hunchentoot server (random port)
         2. "replay" each HTML file through the crawler (replay/core.lisp),
            producing a SNAPSHOT: rewritten HTML + all referenced assets,
            each stored as a content-addressed file <sha256>.<type>
         3. upload assets:   POST /api/blob/check, PUT /api/blob/upload
         4. schedule render: POST /api/replay/schedule
              body = cl-store-serialized SNAPSHOT-REQUEST
                     (snapshot + channel, branch/commit info, browser configs)

 SERVER (screenshotbot.io)
 ─────────────────────────
 /api/replay/schedule (pro/blobs.lisp)
   └─> deserialize snapshot-request
       point snapshot's tmpdir at the company blob store
       push-snapshot onto the global RENDER-ACCEPTOR
       send-remote-run (replay/remote.lisp)
         └─> background thread + persistent REMOTE-RUN object (status, logs)
               └─> schedule-replay-job (replay/integration.lisp)
                     renders the uploaded snapshot directly
                     for each browser config:
                       selenium/webdriver navigates to the hosted snapshot
                       full-page screenshots taken via the replay proxy
                     └─> process-results: reuses SDK code *in-process* with a
                         transient API key to PUT /api/run
                           └─> normal run/promotion/report pipeline
```

## Phase 1: Client-side capture (SDK)

`record-static-website` (`sdk/static.lisp`) does not just upload the
directory as-is. It reuses the *replay crawler* — the same engine used for
sitemap-based web replays — to normalize each page:

1. It starts a throwaway local hunchentoot acceptor on a random port with
   `:document-root` pointing at the user's directory, so the crawler can
   fetch the pages over plain HTTP.
2. It finds the HTML files to snapshot — either by walking the directory
   (`find-all-index.htmls`) or from an explicit `--html-list` file.
3. For each HTML file it calls `replay:load-url-into`
   (`replay/core.lisp`) against `http://localhost:<port>/<path>`. If
   `--assets-root` was passed, that URL is used as the `actual-url`, i.e.
   the base URL for resolving relative asset references against the real
   internet instead of the local server.

### What `load-url-into` produces

The crawler builds a `snapshot` object (`replay/core.lisp`) backed by a
tmpdir:

- The HTML is parsed with plump. `process-node` walks the DOM and, for
  every `img`/`source`/`link`/`script`/inline `style`/`style` attribute,
  fetches the referenced asset (`push-asset`) and rewrites the reference to
  point at the asset's future hosted path. It also normalizes things that
  break screenshots: strips `srcset`, forces `loading=eager`, removes
  `autoplay` and `autofocus`, replaces iframes with a placeholder, adds a
  `screenshotbot` class to `body`, and injects `/css/replay.css`.
- CSS (external stylesheets, `<style>` tags, and `style=` attributes) is
  run through `rewrite-css-urls`, which recursively fetches and rewrites
  `url(...)` references.
- Every fetched asset is written to the tmpdir as `<sha256-of-content>.<type>`
  (`write-asset`) and recorded as an `asset` object holding:
  - `file` — the hosted path `/snapshot/<snapshot-uuid>/assets/<hash>.<type>`
  - `url` — the original URL it was fetched from
  - `status` and the (sanitized) response headers, replayed verbatim later
- Finally the rewritten HTML itself is serialized and stored as an asset
  too (the *root asset*), and its URL is pushed onto the snapshot's
  `root-urls`. `root-assets` recovers the ordered list of entry-point
  assets from `root-urls`.

Fetches go through an on-disk LRU HTTP cache (`~/.cache/screenshotbot/replay/`
on the client, guarded by a file lock), so repeated runs don't re-download
unchanged external assets.

(Heads up: the SDK's `md5-file` in `static.lisp` is actually SHA-256 —
the name is historical. Blob identity is SHA-256 everywhere.)

## Phase 2: Asset upload

`upload-snapshot-assets` uploads the tmpdir contents, content-addressed:

- `POST /api/blob/check` with a JSON list of `{file, hash, type}` — the
  server answers which hashes already exist for this company.
- For each missing blob, `PUT /api/blob/upload?hash=<sha256>&type=<ext>`
  with the raw bytes as the request body.

Server side (`pro/blobs.lisp`): blobs land in the company's blob store,
`<object-store>/company-blob-dir/<company-oid>/<hash>.<type>`. The upload
handler re-hashes the file and asserts it matches the claimed hash. Because
files are named by content hash, uploads are idempotent and deduplicated
per company.

## Phase 3: Scheduling

`schedule-snapshot` (`sdk/static.lisp`) wraps the snapshot in a
`replay:snapshot-request` — snapshot + channel name, repo URL, main
branch, branch hash, commit, merge-base, pull-request URL, browser
configs (default: desktop Chrome 1280x800 + an emulated Nexus), and the
current SDK flag values. The request is serialized with `cl-store` and
POSTed to `/api/replay/schedule`. (The snapshot's `tmpdir` slot is nilled
out first; it's client-local and not serializable.)

The server handler (`pro/blobs.lisp`, `schedule-snapshot`):

1. Deserializes the request (cl-store, or JSON if the content-type says so).
2. **Re-points the snapshot's `tmpdir` at the company blob store.** This is
   the key trick: the asset paths inside the snapshot are just
   `<hash>.<type>` names, and since Phase 2 put those exact files in the
   blob store, the snapshot is now fully materialized server-side without
   ever shipping the files inside the request.
3. Creates an `integration:run` (`replay/integration.lisp`) carrying the
   company, user, channel, browser configs, the original request, and a
   `urls` alist mapping each root asset's original URL path (used as the
   screenshot title) to the asset's original recorded URL — the key
   `run-replay-on-urls` uses to find each root asset in the snapshot.
4. Hands the run to `remote:send-remote-run`.
5. Responds with `{id, logs}` where `logs` is a
   `https://screenshotbot.io/replay/logs/<oid>` URL — the SDK prints this.

### send-remote-run (`replay/remote.lisp`)

Creates a persistent `remote-run` object (bknr.datastore, indexed by
company) with a `log-file` blob, then runs the job on a background thread:
status transitions `:queued → :running → :success` (or `:user-aborted` /
`:cancelled`), with a per-company hash lock so one company's jobs run
serially. All `write-replay-log` output during the job goes to the log
file, which the `/replay/logs/<oid>` page tails live over a websocket
(`local-run-log-resource`). The thread ultimately calls
`schedule-replay-job` on the run.

## Phase 4: Rendering (taking the screenshots)

`schedule-replay-job` (`replay/integration.lisp`) picks its snapshot via
`uploaded-snapshot`: if the run carries an original `snapshot-request`
(the static-website flow), the uploaded snapshot is rendered directly.
Only runs without one (the sitemap/web-replay flow) go through
`crawl-urls-into-snapshot`, which runs the Phase 1 crawl machinery
server-side over the run's URLs to build a snapshot from the live site.

(Historical note: static runs used to be re-crawled too — the uploaded
snapshot was hosted publicly on `screenshotbot-replay.tdrhq.com` and
`schedule-replay-job` crawled that hosted copy into a second snapshot,
so every page was fetched and rewritten twice. Rendering the uploaded
snapshot directly eliminated the second snapshot; the tradeoff is that
the HTML rewriting is now whatever the user's SDK version produced,
rather than being re-normalized by the server's current crawler.)

The snapshot is then handed to `replay-job-from-snapshot`. For each
browser config:

1. `with-selenium-server` obtains a selenium server (in prod, a static
   host — the replay machine — fronted by a Squid proxy;
   `replay/services.lisp`).
2. `with-hosted-snapshot` (`replay/replay-acceptor.lisp`) pushes the
   snapshot onto the shared `render-acceptor` (port 5002) and computes a
   `hosted-url` that the selenium machines can reach (using the local
   address as seen from a socket to the selenium host).
3. `run-in-parallel` splits the URL list into batches across worker
   threads (concurrency comes from `replay-concurrency`, per company
   plan). Each batch gets a fresh webdriver session, proxied through
   Squid.
4. For each page, the browser is pointed at
   `http://<host>:5002/company/<encrypted-company-oid>/assets/<root-asset-name>`.
   The render acceptor looks the asset up in the snapshots pushed for that
   company and serves it — replaying the recorded status code and response
   headers, and streaming the file body from the snapshot's tmpdir (for a
   static run, the company blob store). Asset references in the rewritten
   HTML are bare filenames (`<hash>.<type>`), so they resolve relative to
   the page — `/company/<eoid>/assets/<hash>.<type>` here — and the
   acceptor serves those the same way (it also handles the
   `/snapshot/<uuid>/assets/...` form recorded in each asset's `file`
   slot). Missing assets get a cacheable 404.
5. After a short settle sleep, `process-full-page-screenshot` asks the
   *replay proxy* (`replay/proxy.lisp` — an HTTP service co-located with
   selenium, not the Squid proxy) to take a full-page screenshot. The
   proxy returns an `oid` handle plus the image's md5; the image bytes are
   only downloaded from the proxy if needed (next phase).

## Phase 5: Turning screenshots into a run

Screenshots accumulate in an `all-screenshots` collector
(`replay/run-builder.lisp`). `record-screenshot` first checks
`find-image` by md5 — if the company already has that exact image, the
bytes are never even downloaded from the replay proxy; otherwise it fetches
them and creates an `image` model object directly (`make-image`,
`:verified-p t`).

`process-results` (`replay/integration.lisp`) then closes the loop by
*reusing the SDK's client code in-process on the server*:

- It mints a transient API key for the run's user/company and builds an
  SDK `api-context` pointing back at the server's own host.
- `upload-image-directory` on `all-screenshots` doesn't upload anything —
  it just emits `dto:screenshot` records referencing the already-created
  image OIDs.
- It rebinds the SDK flags from the `sdkFlags` captured in the original
  snapshot-request (plus pull-request / main-branch / repo-url), builds a
  `run-context`, and calls `sdk:make-run`, which PUTs `/api/run`.

From there it's a completely ordinary API run: channel lookup, promotion,
comparisons, reports, build statuses, notifications — identical to a run
uploaded by the normal `screenshotbot ci record` flow. If the request had
no commit info, the run is marked as a periodic (trunk) job.

## Cast of characters (quick reference)

| Component | File | Role |
|---|---|---|
| `static-website` command | `sdk/static.lisp` | Client: crawl local dir, upload, schedule |
| Replay crawler / `snapshot` | `replay/core.lisp` | Fetch + rewrite HTML/CSS into content-addressed assets |
| Blob API + schedule API | `pro/blobs.lisp` | `/api/blob/check`, `/api/blob/upload`, `/api/replay/schedule` |
| Company blob store | `pro/blobs.lisp` | `<store>/company-blob-dir/<oid>/<hash>.<type>` |
| `render-acceptor` | `replay/replay-acceptor.lisp` | Serves hosted snapshots to browsers (port 5002, also public replay hostname) |
| `remote-run` | `replay/remote.lisp` | Persistent job record, background thread, live log streaming |
| `run` + job orchestration | `replay/integration.lisp` | Selenium orchestration, batching, results |
| `all-screenshots` | `replay/run-builder.lisp` | md5-dedup'd image collection → `dto:screenshot` list |
| Selenium/Squid services | `replay/services.lisp` | Locates the selenium host + browser-side proxy |
| Replay proxy | `replay/proxy.lisp` | Screenshot capture service next to selenium (returns oid + md5) |

## Gotchas worth knowing

- `md5-file` in `sdk/static.lisp` and the `:md5` key in
  `record-screenshot` are misnomers in the blob path — blob hashes are
  SHA-256. (The replay-proxy screenshot hash *is* a real md5.)
- The snapshot travels as a `cl-store` binary blob, so the SDK and server
  must agree on the class definitions in `replay/core.lisp`
  (`snapshot`, `asset`, `http-header`, `snapshot-request`). These classes
  also carry `json-mop` metadata for a JSON encoding of the same request.
- The rendered HTML for a static run is exactly what the user's SDK
  produced — the server never re-processes it. A fix to the crawler's
  rewriting rules only affects static runs once users upgrade their SDK
  (sitemap runs pick it up immediately, since their snapshot is built
  server-side).
- The `*replay-acceptor*` registered for `screenshotbot-replay.tdrhq.com`
  in `pro/blobs.lisp` no longer has snapshots pushed to it — it exists so
  the hostname still resolves to something (a cached 404). It used to
  serve the re-crawl described in the historical note above.
- The crawler and asset-serving code paths are shared with the
  sitemap-based "web replay" feature; the flows differ only in where the
  snapshot comes from (uploaded by the SDK vs. crawled server-side) and
  that static runs arrive with git metadata.
