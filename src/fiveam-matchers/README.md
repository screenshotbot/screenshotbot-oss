
# fiveam-matchers

fiveam-matchers is an extensible, composable matchers library for fiveam.

## Examples

It's best to explain this with examples:

```lisp
(test stuff
  (let ((res (+ 1 2)))
   (assert-that res (equal-to 3))))
```

If that fails, you'll get telling you what you what the expected and
actual results are. So far so good.

What about this:

```lisp

(test stuff
  (let ((res (list (+ 1 2) (+ 2 2))))
    (assert-that res
            (has-item 3)))))
```

That makes sense too, it's basically just saying 3 is the result
list. This is can be done with regular fiveam macros though. Let's try
something harder.

```lisp
(test stuff
   (let ((res (list "car" "foobar")))
     (assert-that res
        (has-item (starts-with "foo")))))
```

This is testing that there's one item that starts with `"foo"`. This
demonstrates the composibility of matchers. `starts-with` is a
matcher, and `has-item` is a matcher. If the test fails, you'll get a
message that looks something like this: `none of the elements matched:
a string that starts with "foo"`.

## Custom matchers

Our current set of matchers is pretty basic, we usually build matchers
as and when we need it. If you need a new matcher, it's pretty easy to
define it (and yes it composes with existing matchers)

The matcher API is inspired by Java's Hamcrest. We think it works
nicely, and it gives us a nice template of nomenclature to copy from.

Let's look at how the `starts-with` is implemented.

First we define a matcher class. The name here isn't important since
it won't be exposed to the end-user of your matcher:

```
(defclass starts-with-matcher (matcher)
  ((prefix :initarg :prefix
           :reader prefix)))
```

Define a method that creates the matcher:

```
(defmethod starts-with ((prefix string))
  (make-instance 'starts-with-matcher
                  :prefix prefix))
```

(A common pattern is defining the method with an argument that's
already a matcher, for insteance the `has-item` needs this)

Let's create a method to describe the matcher:

```
(defmethod describe-self ((matcher starts-with-matcher))
  `("a string that starts with `" ,(prefix matcher) "`"))
```

For convenience, you don't need to explicitly format the message. You
can return a list of objects that are all appended to each other. This
method is not called. This function is used to render failure messages
better.

Finally, we need to describe what the mismatch is:

```
(defmethod describe-mismatch ((matcher starts-with-matcher) actual)
  `("expected `" ,actual "` to start with " ,(prefix matcher) ))
```


## Contributing

Please feel free to send us Pull Requests for your matchers! You can
send us pull requests for new matchers, or for the core library.

## Author

Arnold Noronha <arnold@screenshotbot.io>

## License

Apache License, Version 2.0
