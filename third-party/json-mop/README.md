# JSON-MOP

[![Quicklisp dist](http://quickdocs.org/badge/json-mop.svg)](http://quickdocs.org/json-mop/)

[![CI](https://github.com/gschjetne/json-mop/actions/workflows/CI.yml/badge.svg)](https://github.com/gschjetne/json-mop/actions/workflows/CI.yml)

## Introduction

JSON-MOP is a small library aiming to cut down time spent moving data
between CLOS and JSON objects. It depends on
[YASON](https://github.com/hanshuebner/yason) and it should be
possible to use it alongside straight calls to functions from YASON.

## Quick Start

To use JSON-MOP, define your classes with the class option
`(:metaclass json-serializable-class)`. For slots that you want to appear in
the JSON representation of your class, add the slot option `:json-key`
with the string to use as the attribute name. The option `:json-type`
defaults to `:any`, but you can control how each slot value is
transformed to and from JSON with one of the following:

### JSON type specifiers

Type          | Remarks
--------------|--------------------------------------------
`:any`        | Guesses the way to encode and decode the value 
`:string`     | Enforces a string value
`:number`     | Enforces a number value
`:hash-table` | Enforces a hash table value
`:vector`     | Enforces a vector value
`:list`       | Enforces a list value
`:bool`       | Maps `T` and `NIL` with `true` and `false`
`<symbol>`    | Uses a `(:metaclass json-serializable-class)` class definition to direct the transformation of the value

### Homogeneous sequences

In addition, the type specifier may be a list of two elements, first
element is one of `:list`, `:vector`; the second is any JSON type
specifier that is to be applied to the elements of the list.

### NIL and null semantics

JSON `null` is treated as an unbound slot in CLOS. Unbound slots are
ignored when encoding objects, unless `*encode-unbound-slots*` is
bound to `T`, in which case they are represented as JSON `null`.

Slots bound to `NIL` with JSON types other `:bool` will signal an
error, but this may change in the future.

### Encoding and decoding JSON

Turning an object into JSON is done with the `yason:encode` generic
function. Turning it back into an object is slightly more involved,
using `json-to-clos` on a stream, string or hash table; a class name;
and optional initargs for the class. Values decoded from the JSON will
override values specified in the initargs.

### Example

First, define your classes:

```lisp
(defclass book ()
  ((title :initarg :title
          :json-type :string
          :json-key "title")
   (published-year :initarg :year
         :json-type :number
         :json-key "year_published")
   (fiction :initarg :fiction
            :json-type :bool
            :json-key "is_fiction"))
  (:metaclass json-serializable-class))

(defclass author ()
  ((name :initarg :name
         :json-type :string
         :json-key "name")
   (birth-year :initarg :year
               :json-type :number
               :json-key "year_birth")
   (bibliography :initarg :bibliography
                 :json-type (:list book)
                 :json-key "bibliography"))
  (:metaclass json-serializable-class))
```

Let's try creating an instance:

```lisp
(defparameter *author*
  (make-instance 'author
                 :name "Mark Twain"
                 :year 1835
                 :bibliography
                 (list
                  (make-instance 'book
                                 :title "The Gilded Age: A Tale of Today"
                                 :year 1873
                                 :fiction t)
                  (make-instance 'book
                                 :title "Life on the Mississippi"
                                 :year 1883
                                 :fiction nil)
                  (make-instance 'book
                                 :title "Adventures of Huckleberry Finn"
                                 :year 1884
                                 :fiction t))))
```

To turn it into JSON, `encode` it:

```lisp
(encode *author*)
```

This will print the following:

```javascript
{"name":"Mark Twain","year_birth":1835,"bibliography":[{"title":"The Gilded Age: A Tale of Today","year_published":1873,"is_fiction":true},{"title":"Life on the Mississippi","year_published":1883,"is_fiction":false},{"title":"Adventures of Huckleberry Finn","year_published":1884,"is_fiction":true}]}
```

The same can be turned back into a CLOS object with `(json-to-clos input 'author)`

## Licence

Copyright (c) 2015 Grim Schjetne

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
