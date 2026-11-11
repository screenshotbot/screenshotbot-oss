# CL-SENTRY-CLIENT

Sentry client for Common Lisp

WIP

## Basic usage

```lisp
(sentry-client:initialize-sentry-client <sentry-dsn>)
(sentry-client:with-sentry-error-handler () (error "test"))
```

Or in your own error handler: 

```lisp
(handler-case (my-code-with-error)
   (error (e)
      (sentry-client:capture-exception e)
      ...)))
```

### Hunchentoot handler

```lisp
(defmethod hunchentoot:maybe-invoke-debugger :after (condition)
    (when hunchentoot:*catch-errors-p*
        ;; There's an error in trivial-backtrace:map-backtrace in SBCL 
        ;; if we don't set sb-debug:*stack-top-hint* to NIL
        (let ((sb-debug:*stack-top-hint* nil)) 
           (sentry-client:capture-exception condition))))
```

Just add `sentry-client.hunchentoot` as ASDF dependency to include the Hunchentoot handler.

## Async client version

The async version of the client uses simple-tasks library for sending HTTP requests in the background

```lisp
(ql:quickload :sentry-client.async)

(sentry-client:initialize-sentry-client <dsn> :client-class 'sentry-client:async-sentry-client)
(sentry-client:test-sentry-client)
```

## Screenshots

![alt text](https://github.com/mmontone/cl-sentry-client/raw/master/doc/screenshot1.png "Screenshot 1")
![alt text](https://github.com/mmontone/cl-sentry-client/raw/master/doc/screenshot2.png "Screenshot 2")

## License

MIT
