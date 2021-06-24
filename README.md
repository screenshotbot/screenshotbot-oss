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
needs if you do need Java
support. [screenshotbot.io](https://screenshotbot.io) is powered by
LispWorks.

### OS

Currently we primarily support Linux.

In theory we should be able to work on Mac (easy) and Windows
(harder). If you do go this route, you're on your own. But please send
us pull requests :)

### Dependencies

Screenshotbot is built as a monolith service. It does not depend on
any external service. It does not use an external database. There are
a few command line tools (`imagemagic` and `html2text`) that we use,
and we'll automatically pull in an Common Lisp dependencies with
Quicklisp.

### Launch

Once you've picked your implementation, you can use the implementation
to load launch.lisp. For example, with SBCL that looks like:

```
$ sbcl --script launch.lisp
```

This should start up Screenshotbot on port 4091. You can access it as
http://localhost:4091 from the browser. All the data will be stored in
`~/.config/screenshotbot/object-store`. If you need to reset the
state, you can just delete that directory and start over. But you must
treat this directory as your database. We recommend keeping this on a
device with high redundancy, such as RAID or Amazon EBS.

### Configuration

Screenshotbot has integrations with various external tools,
e.g. GitHub, Jira, Gitlab etc. Most of these platforms require some
kind of API key to access their APIs, and must be configured with
Screenshotbot before you can use them.

For simplicity and maintainability, we don't have complex GUIs to
modify these _site-admin_ configurations. Instead each of these
integrations are exposed as plugins that must be configured with basic
Common Lisp code. The configuration can be hot-reloaded.

Screenshotbot looks for a file called `config.lisp` in both the
git-root, and in `~/.config/screenshotbot/`. If found, it loads this
file as the configuration.

See [Updating
config.lisp](https://github.com/screenshotbot/screenshotbot-oss/wiki/Updating-config.lisp)
for a more thorough discussion.

### Upgrading


## Using the CLI tool

## Setting up Plugins

## Upgrading Screenshotbot
