
# easy-macros: An easy way to write 90% of your macros

Easy-macros help you write macros of this form:

```
  (with-<something> (...args...)
     ...body...)
```

Under the hood, this automates the call-with pattern.

## Examples

Let's rewrite some well known examples to show what we mean.

### ignore-errors

```
(def-easy-macro custom-ignore-errors (&fn fn)
  (handler-case
     (funcall fn)
    (error () nil)))
```

This `custom-ignore-errors` has a slightly different API though:

```
(custom-ignore-errors ()
  ...body...)
```

All easy-macros takes a second list for arguments. This is true even
if it takes no arguments and only a body.

Notice a few things:
* We don't use backticks anywhere
* Instead of a body, we get a lambda function. This function is provided by the `&fn` argument.
* If you redefine custom-ignore-errors, all callers of the macro will
  point to the new code, unlike with regular macros. (With some caveats! See below.)

### with-open-file

```
(def-easy-macro with-custom-open-file (&binding stream file &rest args &fn fn)
  (let ((stream (apply #'open file args)))
    (unwind-protect
       (funcall fn stream)
      (close stream))))
```

This can be used almost exactly like with-open-file.

Notice a few things:
* We don't use backticks anywhere
* This function takes one argument. easy-macro knows this based on the
  `&binding` argument, unlike the previous example.

### uiop:with-temporary-file

```
(def-easy-macro my-with-custom-temporary-file (&key &binding stream &binding pathname prefix suffix &fn)
   ;; ... you get the idea
    (funcall fn my-stream my-pathname))
```

I didn't build out the example completely, but I wanted to show you
how you could write more complex arguments in the macro.

All the arguments named with `&binding` are not part of argument-list,
they will be sequentially bound to the `&fn` body function. The rest
of expressions form the lambda-list for the argument-list.
