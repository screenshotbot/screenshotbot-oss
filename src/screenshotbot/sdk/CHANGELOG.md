# Changelog

## 2.17.5 - 2026-02-01

### Changed

- Bring back multi_ack again


## 2.17.4 - 2026-01-31

### Changed

- For the commit-graph protocol, prefer the use of repo-link instead
  of the remote-url from the git repo.

## 2.17.3 - 2026-01-30

### Changed

- Don't use both extraheaders and auth

## 2.17.2 - 2026-01-29

### Changed

- Removed ignore-errors around credential-fill for better debugging
- Remove --local, so we can also see global extraheaders
- Add a warning for when both extraheaders and a credential is available

## 2.17.1 - 2026-01-29

### Added

- Added support for credential-helper when interacting with Git via
  HTTP

## 2.17.0 - 2026-01-29

### Changed

- Lispworks upgraded to 8.1.2, likely we pulled in Quicklisp changes
  too. But no intentional changes otherwise.

## 2.16.24 - 2025-11-18

### Changed

- Fixed an issue with the upload-pack protocol hanging when an ERR is
  sent during read-shallow-lines. (T2130)


## 2.16.23 - 2025-11-10

### Changed

- Added more Sentry extras

## 2.16.22 - 2025-11-10

### Changed

- Added a bunch of Sentry extras to track run-time when a crash happens

## 2.16.21 - 2025-11-10

### Changed

- Reverted multi-ack capability changes from 2.16.20
- Fixed logging for stack overflow errors to improve error reporting (T2113)
- Fixed stack overflow issue in git-pack protocol by avoiding reliance on tail call optimization (T2115)

### Note

- Version 2.16.20 should not be used due to issues with the multi-ack implementation

## 2.16.20 - 2025-11-07

### Changed

- Added retry logic for upload-pack operations to improve reliability (T2111)
- Enabled multi-ack capability in git-pack protocol by default

**WARNING: This version has known issues and should not be used. Please upgrade to 2.16.21 or later.**

## 2.16.19 - 2025-11-06

### Changed

- Fixed git protocol ACK/NAK handling when sending multiple "have" lines
  without multi_ack capability. The server sends ACK responses interleaved
  with the request stream, so we now read until finding the PACK signature
  (T2108)

## 2.16.18 - 2025-10-31

### Changed

- Automatically detect API hostname from API secret when --api-hostname is not provided

## 2.16.17 - 2025-10-28

### Changed
 
- Fix an issue with duplicate hashes in the `haves` during the
  git-pack protocol. This leads to undocumented behavior on
  upload-pack servers (T1907)

## 2.16.16 - 2025-10-28

No expected changes, only refactoring

## 2.16.15 - 2025-10-24

### Changed

- Advertise "shallow" feature in the git-pack protocol (potential fix
  for T2093)

## 2.16.14 - 2025-10-24

### Changed

- Added timestamps to the server-cli-logs


## 2.16.13 - 2025-10-23

### Added

- Added support for using Authorization header from .git/config when
  making HTTP requests. (T2082)


## 2.16.12 - 2025-10-22

### Changed

- We were sending the wrong data format to /api/commit-graph in the
  new flow (T2092)


## 2.16.11 - 2025-10-22

### Changed

- Fixed a typo that prevented `allow-tip-sha1-in-want` to actually be
  enabled, probably causing T2091

## 2.16.10 - 2025-10-22

### Changed

- Use the new commit-graph flow even for what we were calling
  "locally-rebased". In reality, it wasn't being locally rebased, it
  was Bitrise using the `refs/pull/*merge` commit, which should be
  part of the remote repository.

## 2.16.9 - 2025-10-21

### Added

- Added X-Cli-session-id

### Changed

- Fixed server-cli-logs

## 2.16.8 - 2025-10-21

### Changed

- Fixed `dev install` command (T2086)


## 2.16.7 - 2025-10-15

### Changed

- Added the ability to store logs, to make it easier to debug CI-side issues


## 2.16.6 - 2025-10-14

### Changed

- Nothing significant should've changed, accidental no-op

## 2.16.5 - 2025-10-14

### Changed

- Fixed crash when using default API hostname (T2077, T2076, T2075, T2074)

## 2.16.4 - 2025-10-13

### Added

- Server side flag to control the new commit-graph workflow that was
  last disabled in 2.15.4. The plan is to slowly roll out.
  
### Changed

- /api/version is now authenticated
- Updgraded to Lispworks 8.1.1

## 2.16.3 - 2025-09-12

### Added

- Added `--pixel-tolerance` flag to help with flakiness to some
  degree.

## 2.16.2 - 2025-08-29

### Changed

- Fixed issue with re-using SSL connections (blames to 2.16.0, from
  changes in dependencies.)

## 2.16.1 - 2025-08-28

### Changed

- Fixed `ci upload-commit-graph` subcommand. This was crashing.

## 2.16.0 - 2025-08-28 

### Added

- Support for Xcode cloud Environment variables
- Support for parsing images from .xcresults

### Changed

- We're not using OpenSSL/LibreSSL anymore for hash functions. This
  might be a minor performance regression (by a few seconds at
  most). But it's more reliable.

## 2.15.17 - 2025-06-02

### Changed

- Fixed the innocuous warning about `--pull-request argument` being
  invalid, which typically only happened on Bitrise.

## 2.15.16 - 2025-05-30

### Changed

- Correctly parse partial --override-commit-hash arguments. (Context:
  Some older customers tend to pass this arguments, but for most cases
  you don't need to pass this and we can detect it from the
  environment. For customers who were using this argument, some were
  passing an incomplete SHA prefix. Around version 2.15.14, we started
  trying to complete this prefix, but the code was buggy.)

## 2.15.15 - 2025-05-22

### Changed

- Fixed bug in the old commit-graph flow where shallow commits weren't
  sending parent commits, thus messing up the commit graph.

## 2.15.14 - 2025-05-21

### Changed

- Disabled the new commit-graph flow again. We've been running an
  older version (2.15.1), but we're pushing this new version so that
  customers can update to this instead of running buggy versions > 2.15.1 and < 2.15.14.

## 2.15.12 - 2025-05-11

### Changed

- Fixed incomplete commit prefixes passed to
  `--override-commit-hash`. 

## 2.15.11 - 2025-05-11

### Changed

- Fix couple of edge cases with the new commit graph flow

## 2.15.8 - 2025-05-11

### Changed

- Fixed netrc parsing in certain case
- Fix Git HTTP URL parsing when authentication is part of the Git URL
- Add a retry if the Git HTTP request fails


## 2.15.7 - 2025-05-10

### Changed

- Bug fix in the HTTP upload-pack protocol

## 2.15.6 - 2025-05-10

### Changed

- Enable using upload-pack via HTTP protocol too

## 2.15.5 - 2025-05-10

### Changed

- Added some additional logging, no behavioral changes

## 2.15.4 - 2025-05-09

### Changed

- Fixed a bug in URL encoding of parameter of new commit graph API

## 2.15.3 - 2025-05-09

### Added

- Re-enabled the new commit graph api


## 2.15.1 - 2025-05-08

### Removed

- Disabled the new commit graph api


## 2.15.0 - 2025-05-08

### Added

- It turns out we weren't really deploying the older versions because
  of a bug in 2.12.1. So this includes all the changes from 2.12.1
  onward to 2.14.1. Sorry about this.

## 2.14.1 - 2025-05-08

### Changed

- Fixed bug in 2.14.0 that made the commit-graph always be uploaded
  twice, even when the first one succeeded.

## 2.14.0 - 2025-05-08

This release was not deployed because of a bug

### Added

- Use the `git-upload-pack` protocol to support shallow clones

## 2.13.1 - 2025-04-17

### Changed

- Avoid crash when `git merge-base` fails, warn instead. This is
  technically not being used by the server anymore.


## 2.13.0 - 2025-04-07

### Added

- `dev verify-against-ci` sub-command, currently in beta testing. At
  the moment, we require you to know the exact commit you want to
  compare against, in a future update we'll try to automate that
  process based on feedback.

## 2.12.1 - 2025-03-29

### Changed

- Upgraded Lispworks to 8.1 

## 2.12.0 - 2025-03-06

### Added

- Started this CHANGELOG
- Added subcommand `batch reports` to list all reports under a batch
- Added subcommand `batch accept-all` to accept all reports under a batch




