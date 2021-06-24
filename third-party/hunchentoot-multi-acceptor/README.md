# hunchentoot-multi-acceptor
### Arnold Noronha <arnold@jipr.io>

Typically each hunchentoot acceptor listens on a single port, and
typically serves a single domain. A typical configuration is to have a
frontend webserver like Nginx to route requests to the appropriate
port.

This is hard to manage when the number of domains keep increasing
(which is typical for small companies trying to iterate on different
products). hunchentoot-multi-acceptor simplifies the configurations of
such systems.

## Installation

As an example, I personally run tdrhq.com (my personal website) and
jipr.io (a project I'm working on.) These have two different
hunchentoot acceptors:

```lisp
(defvar *tdrhq-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                         :port nil
                                         :name 'tdrhq))

(defvar *jipr-acceptor*
  (make-instance 'hunchentoot:easy-acceptor
                 :port nil
                 :name 'jipr))
```

These definitions could be in different packages and systems, it
doesn't matter. Also the `:port` argument is useful when developing
locally and want to test only one of the websites, but it won't be
used by multi-acceptor.

Now, we have a multi-acceptor setup as follows:

```lisp
(defparameter *multi-acceptor* (make-instance 'hunchentoot-multi-acceptor:multi-acceptor :port 4001 :name 'multi-acceptor))
(hunchentoot-multi-acceptor:add-acceptor *multi-acceptor* "tdrhq.com" *tdrhq-acceptor*)
(hunchentoot-multi-acceptor:add-acceptor *multi-acceptor* "www.jipr.io" *jipr-acceptor*)

(hunchentoot:start *multi-acceptor*)
```

At this point, you can configure your Nginx frontend for both websites
to point to `localhost:4001`. Make sure the "Host" header is
appropriately set. (`proxy_set_header Host $host:$server_port`). After
that hunchentoot-multi-acceptor will take care of routing to the
appropriate acceptor. Only one port will be opened.

## License

Apache License, Version 2.0
