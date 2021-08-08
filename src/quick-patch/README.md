
# Quick-Patch

This is a really simply library to override sytems in Quicklisp, or to
add libraries that are not available in Quicklisp.

The goal of this project is to make it easy for people to contribute
to existing projects. Currently with Quicklisp, if somebody chooses to
contribute, they'd have to wait a month before they can get the
updated version, or they'd have to use git submodules to manage their
patches. Submodules are super hard to maintain, especially across
multiple developers or CI machines.

Quick-patch does one thing, and does it really simply: it checks out a
repository at a commit that you specify, and adds it to
`asdf:*central-registry*`. That's it. On subsequent runs if you set it
up correctly it won't hit the network.

# Usage

Currently Quick-patch isn't designed to be interactive. In theory
it'll work, as long as you're adding new repos, but don't rely on it
for removing patches interactively.

Usually your project has a top-level script that sets things up. Or
maybe you're just using an init file. In either case, before you start
loading other quicklisp projects, you want to do:

```
(ql:quickload :quick-patch)
```

Currently we don't have any dependencies, so you can override just
about any system in quicklisp.

Now you can set up an override. For example, I recently sent a pull
request to `cl-plus-ssl`. At this point, I needed to use my own
patched version, so I added this to my loading script:

```
(quick-patch:register-external "https://github.com/tdrhq/cl-plus-ssl"
                            "4c614fc3f28017f5c5f4c72a8ce413dd042bfb09")
```

Notice I used the full git commit hash. A partial hash or a tag/branch
name will work, but it will cause quick-patch to hit the network and
do a fetch on every startup, and it can get really annoying. With the
full SHA hash, we can quickly check on subsequent runs that the repo
we checked-out is on the correct commit. So this is how I recommend
you use it.

Finally, we need to tell quick-patch to do all the work required to
fetch stuff.

```
(quick-patch:prepare-externals "build/quick-patch/")
```

`prepare-externals` takes one argument which is going to be your cache
directory. If this is under a git repository, make sure your cache
directory is in your `.gitignore`.

Happy hacking!

# License

Mozilla Public License, v2.

(No specific reason for the license, it's mostly arbitrarily chosen,
but this is what it is at the moment.)

# Authors

Arnold Noronha <arnold@tdrhq.com>. While you're here, may I recommend
[Screenshotbot](https://github.com/screenshotbot/screenshotbot-oss)?
