# Changelog

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




