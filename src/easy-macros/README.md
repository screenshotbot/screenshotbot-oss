
# easy-macros: An easy way to write 90% of your macros

[![tdrhq](https://circleci.com/gh/tdrhq/easy-macros.svg?style=shield)](https://app.circleci.com/pipelines/github/tdrhq/easy-macros?branch=main)

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

### maplist

Common Lisp comes with `dolist`, but not a `maplist`. Let's implement
a quick `maplist` macro using `loop`:

```
(def-easy-macro maplist (&binding x list &fn fn)
  (loop for value in list collect (funcall fn value))
```

Before `def-easy-macro` this would've been too much work to define for
something simple. With `def-easy-macro` it's just as easy to work with
as any regular function, so you tend to macrofy even tiny abstractions
like this.


## Caveats with redefinitions

Most redefinitions will automatically be applied to all callers. If
you change the lambda-list (either `&binding` or otherwise), the new
definition may not be compatible.

## TODO

This library is NOT very polished.

However, even with its limited polish it's been ridiculously useful in
my work, so I thought I should put it out there and accept feedback
and pull requests. There are few things that I'd personally like to see:

* Less brittle lambda-list parsing: currently it's really hacky
* A way to implement macros of the form:
```
(def-stuff my-stuff (...)
  ,@body)
```
* In a similar vein as above: sometimes in macros you want to pass the
  quoted symbol name instead of the evaluated expression. In theory I
  can build that...
* But I want to limit what this library does. I want to make it easy
  for somebody new to CL to write macros *most* of the time. Just
  because I can doesn't mean I should.

## Author

Arnold Noronha <arnold@screenshotbot.io>

## License

Apache License, Version 2.0
