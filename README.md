# Screenshotbot: Screenshot Testing Service

Screenshotbot is a Screenshot Testing service. Screenshotbot will
connect your existing Android, iOS or Selenium tests to track how
screenshots change over time, notifying you on Pull Requests, Jira
etc. We provide several integrations to common Code Review and Task
Management platforms, and have more in the pipeline.

Screenshotbot provides an Open Source alternative to various other
tools already on the market (for example, Applitools, Percy,
Happo). It powers our own commercial platform
[screenshotbot.io](https://screenshotbot.io).

Unlike the other tools mentioned, this open source library does not
provide browser test runner. There are better open source tools to run
Selenium or Cypress tests and generate screenshots. Screenshotbot just
expects you to use these tools to generate a directory of screenshots,
and we'll handle the rest.

## Installation

Screenshotbot is written in Common Lisp. Common Lisp has several
commercial and open source implementations. We officially support
SBCL, Clozure CL (CCL) and LispWorks, but the core features should work on
any compliant Common Lisp.

Certain features, such as the Slack integration, use third-party Java
libraries. Java is not supported in SBCL, so if you need these
integrations you should use either CCL or LispWorks.

LispWorks is a commercial platform, and can get expensive. Contact us
if you need pre-built binaries on LispWorks, but it'll come with
additional licensing restrictions. SBCL is more performant that CCL
especially with multi-threading, but we think CCL should satisfy your
needs if you do need Java support.

## Using the CLI tool

## Setting up Plugins

## Upgrading Screenshotbot
