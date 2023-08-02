
# file-lock: Simple flock based file locking for CL

There's not much to this. Currently only supported on POSIX systems
(so no Windows, though I'll be happy get a PR for that.)

Only supported on SBCL and Lispworks.

On any other CL and OS combination, we revert to a noop lock (i.e. we
won't throw an error, so beware).

This uses a polling mechanism. (i.e.,  it doesn't block on
`flock`. Instead, it keeps polling `flock` in a non-blocking
manner. Otherwise the CL thread would be uninterruptable which isn't a
great developer experience.)

The most common use case is pretty straightforward:

```lisp
(let ((file-lock (make-file-lock :file "...")))
    (unwind-protect
        (do-stuff)
      (release-file-lock file-lock)))
```

## Author

Arnold Noronha <arnold@screenshotbot.io>
