# Changelog

## 2.15.11 - 2025-05-11

- Fix couple of edge cases with the new commit graph flow

## 2.15.8 - 2025-05-11

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




