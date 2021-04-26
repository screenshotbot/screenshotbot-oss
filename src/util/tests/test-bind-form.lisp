(defpackage :util.test-bind-form
  (:use :cl
        :fiveam)
  (:export))
(in-package :util.test-bind-form)

(markup:enable-reader)

(Def-suite* :util.test-bind-form)


(test simple-rename
  (is
   (equal
    (markup:write-html <input value= "car" name= "foo{bar}" >)
    (markup:write-html
     (let ((data (alexandria:plist-hash-table '("bar" "car") :test 'equal)))
       <util:bind-form data=data name= "foo">
       <input name= "foo{bar}" >
       </util:bind-form>)))))

(test simple-rewrite-existing-arg
  (is
   (equal
    (markup:write-html <input  name= "foo{bar}" value= "car">)
    (markup:write-html
     (let ((data (alexandria:plist-hash-table '("bar" "car") :test 'equal)))
       <util:bind-form data=data name= "foo">
       <input name= "foo{bar}" value= "duh" >
       </util:bind-form>)))))


(test nested-rename
  (let ((data (alexandria:plist-hash-table '(:bar "car"))))
   (is
    (markup:write-html <h3><input value= "car" name= "foo{bar}"  ></h3>)
    (markup:write-html
     <util:bind-form data=data name= "foo">
     <h3><input name= "foo{bar}" ></h3>
     </util:bind-form>))))
