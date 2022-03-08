# AUTO-RESTART

Auto-restart makes it easy to create restarts for the most common
situation of just retrying a function. To summarize, this is how
you'll use it:

```lisp
(with-auto-restart ()
  (defun foo(arg1 &key other-options)
    (... do stuff ...)))
```

If something fails in-side foo, you'll be presented with a restart to
RETRY-FOO. Once you've pushed this to production you might that the
function FOO is flaky, possibly because it hits the network or some
other unreliable resource. In that case, you can automate calling the
retry like so:

```lisp
(with-auto-restart (:retries 2 :sleep 1)
  (defun foo(arg1 &key other-options)
    (... do stuff ...)))
```

This is really it, but you might have some questions as to why the API
is as it is. Let's build up the reasoning for this.

## Motivation

So say you write a complex, slow running function FOO:

```lisp
(defun foo (...args ...)
  (... do stuff ...))
```

It's part of a slow job, say a crawler, so if the function FOO fails,
you don't want to restart the job from scratch. Instead, you probably
just want to restart the FOO function. You might consider doing
something like this:

```lisp
(defun foo (...args...)
  (labels ((actual-foo ()
             (restart-case
                (...do stuff...)
               (retry-foo ()
                (actual-foo)))))
     (actual-foo)
```

Now, when an error happens you'll get a restart to RETRY-FOO. But
there's a catch! If you make changes to `(...do stuff...)`, those
changes won't show up even if you RETRY-FOO.

So we'll do something like this instead:
```
(defun foo (...args...)
  (restart-case
     (...do stuff...)
    (retry-foo ()
      (foo ...args...))))
```

This version works great. Applying `...args...` on the last line needs
to be done carefully. You need to account for `&optional`, `&key`
etc. Also, any changes to the args needs to be correctly kept in sync
on the last line. So it's error prone.

`with-auto-restart` just does this for you automatically.

## Retries

In the process of developing, you'll end up testing that the restart
works correctly. (Restarts need to be verified too! It's just code
after all! A restart might fail, if for example some global state has
changed beforethe  condition was signaled).

But you might find that the `FOO` function keeps failing for
legitimate reasons. Perhaps it's hitting the network, or some other
resource that's not always available. Using `:retries` makes it
trivial to update the existing function to automatically call the
retry for you multiple times.

## Future work

One thing I would like to do, but I don't know if this is possible, is
to be able to enter the debugger, but if no action was chosen with a
set amount of time, then automatically pick a restart.


## Authors

Built by Arnold Noronha <arnold@tdrhq.com>

## License

Licensed under the Mozilla Public License, v2.
