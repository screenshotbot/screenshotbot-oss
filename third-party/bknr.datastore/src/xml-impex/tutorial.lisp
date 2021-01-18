;;; XML Import Export for Common Lisp

;;;# Introduction
;;;
;;; We often have to communicate data with the external world, for
;;; example sharing data with external programs or getting information
;;; from a third-party. The XML format has been pretty successful as a
;;; common base format for digital information. However, parsing and
;;; handling and serializing XML data is not very practical, though
;;; not very difficult, and most of the time just plain
;;; boring. Imagine you have a XML file containing information about
;;; books in a library. Most of the time, you will parse the XML data,
;;; and instantiate CLOS objects in order to use standard LISP
;;; functions on the data. The parsing and serializing code is most of
;;; the time trivial. The XML import/export module in BKNR enables you
;;; to annotate standard CLOS class definitions in order to make
;;; parsing and serializing XML data automatic.
;;;
;;; The XML import/export modules works by using DTDs to represent the
;;; format of the XML file. DTDs are quite simple and pretty
;;; widespread. A DTD specifies which elements are valid in a XML
;;; file, which attributes these elements have, and how the elements
;;; are nested. When defining a CLOS class, you can specify a DTD, and
;;; annotate the slot definitions with information about which slot is
;;; mapped to which attribute or element in the DTD.
;;;
;;; This tutorial will show you how to parse and serialize CLOS
;;; classes to XML, and how to use features like specifying relations
;;; on IDs.

;;;# Obtaining and loading BKNR XML import/export
;;;
;;; You can obtain the current CVS sources of BKNR by following the
;;; instructions at `http://bknr.net/'. Add the `src' directory of
;;; BKNR to your `asdf:*central-registry*', and load the indices
;;; module by evaluating the following form:

(asdf:oos 'asdf:load-op :bknr.impex)

;;; Then switch to the `bknr.impex' package to try out the tutorial.

(in-package :bknr.impex)

;;;# A simple example
;;;
;;; We start with our book example. We have to manipulate informations
;;; about the books present in a library, and the library provides us
;;; with a XML file containing all the books present in the library. A
;;; sample XML file has the following format:

<books>
  <book id="1" isbn="234114">
    <author>J.R.R Tolkien</author>
    <title>The Lord of the Rings</title>
  </book>
  <book id="2" isbn="235431">
    <author>William Gibson</author>
    <title>Neuromancer</title>
  </book>
</books>

;;; The DTD for the sample above is pretty simple too:

<!ELEMENT books (book)*>
<!ELEMENT book (author,title)>
<!ELEMENT author (#PCDATA)>
<!ELEMENT title (#PCDATA)>

<!ATTLIST book id   ID    #REQUIRED
               isbn CDATA #REQUIRED>

;;; We now specify our CLOS class book without XML annotations. The
;;; common approach to XML parsing would now implement either a SAX
;;; parser filling a `BOOK' class from a `book' XML element, or a DOM
;;; parser walking down the DOM tree to instantiate `BOOK' objects at
;;; `book' leaves.

(defclass book ()
  ((author :initarg :author :reader book-author)
   (id :initarg :id :reader book-id :type integer)
   (isbn :initarg :isbn :reader book-isbn)
   (title :initarg :title :reader book-title)))

(defmethod print-object ((book book) stream)
  (print-unreadable-object (book stream :type t :identity t)
    (format stream "~S" (book-title book))))

;;; We can however extend the `BOOK' class definitions by giving it
;;; the metaclass `XML-CLASS', by specifying the DTD to be used by the
;;; class, and by specifying the XML element corresponding to the
;;; class. We also annotate the slot definitions.

;;; If we have evaluated the previous book definition we must reset it now
;;; before proceeding:
(setf (find-class 'book) nil)

(defvar *tutorial-dtd* "../xml-impex/tutorial.dtd")

(defclass book ()
  ((author :initarg :author :reader book-author
           :element "author")
   (id :initarg :id :reader book-id :type integer
       :attribute "id" :parser #'parse-integer)
   (isbn :initarg :isbn :reader book-isbn
         :attribute "isbn")
   (title :initarg :title :reader book-title
          :element "title"))
  (:metaclass xml-class)
  (:dtd-name *tutorial-dtd*)
  (:element "book"))

;;; We can now read the XML file containing the book definitions. As
;;; we haven't specified a CLOS class for the root element `books', we
;;; get back a list containing all the children nodes of `books' under
;;; the keyword `:BOOK'.

(parse-xml-file "../xml-impex/tutorial.xml" (list (find-class 'book)))
; => (:BOOK
;     (#<BOOK ("The Lord of the Rings") {4922BCD5}>
;      #<BOOK ("Neuromancer") {4922E0B5}>))
(setf *books*
      (getf (parse-xml-file "../xml-impex/tutorial.xml"
                            (list (find-class 'book)))
            :BOOK))
; => (#<BOOK ("The Lord of the Rings") {4922BCD5}>
;     #<BOOK ("Neuromancer") {4922E0B5}>)

;;; In the same way, we can serialize our books back to XML using the
;;; function `WRITE-TO-XML'. As we still do not have a class for the
;;; root element (and we never will), we can specify the name of the
;;; root element as an argument.

(write-to-xml *books* :name "books")
; => <books>
;     <book id="1" isbn="234114">
;        <author>J.R.R Tolkien</author>
;        <title>The Lord of the Rings</title>
;     </book>
;     <book id="2" isbn="235431">
;        <author>William Gibson</author>
;        <title>Neuromancer</title>
;     </book>
;    </books>

;;; As you have noticed, we use an in ID in the book class. It would
;;; be nice to have an index for all the books. This is very simple,
;;; as a class with metaclass `XML-CLASS' also support BKNR
;;; indices. Thus, you can write:

(defclass book ()
  ((author :initarg :author :reader book-author
           :element "author"
           :index-type hash-index :index-initargs (:test #'equal)
           :index-reader books-with-author
           :index-keys all-authors)
   (id :initarg :id :reader book-id :type integer
       :attribute "id" :parser #'parse-integer
       :index-type unique-index :index-reader book-with-id
       :index-values all-books)
   (isbn :initarg :isbn :reader book-isbn
         :attribute "isbn"
         :index-type unique-index :index-initargs (:test #'equal)
         :index-reader book-with-isbn)
   (title :initarg :title :reader book-title
          :element "title"))
  (:metaclass xml-class)
  (:dtd-name *tutorial-dtd*)
  (:element "book"))

;;; We can now import our XML file and the indices will automatically
;;; get filled. The indices change nothing to the serialization.

(parse-xml-file "../xml-impex/tutorial.xml" (list (find-class 'book)))
; => (:BOOK
;     (#<BOOK "The Lord of the Rings" {49224D25}>
;      #<BOOK "Neuromancer" {492272CD}>))
(all-authors)
; => ("J.R.R Tolkien" "William Gibson")
(all-books)
; => (#<BOOK "The Lord of the Rings" {49224D25}>
;     #<BOOK "Neuromancer" {492272CD}>)
(books-with-author (first (all-authors)))
; => (#<BOOK "The Lord of the Rings" {49224D25}>)
; T
(book-with-id 1)
; => #<BOOK "The Lord of the Rings" {49224D25}>
;  T
(write-to-xml *books* :name "books")
; => <books>
;       <book id="1" isbn="234114">
;          <author>J.R.R Tolkien</author>
;          <title>The Lord of the Rings</title>
;       </book>
;       <book id="2" isbn="235431">
;          <author>William Gibson</author>
;          <title>Neuromancer</title>
;       </book>
;    </books>

;;; Note that you have to clear the indices if you reload the same XML
;;; file multiple times.

(clear-class-indices (find-class 'book))
; => NIL

;;;# Specifying a mapping from CLOS to XML
;;;
;;; In the following text, "XML class" refers to a CLOS class with
;;; metaclass `XML-CLASS', and "XML object" refers to an instance of
;;; an "XML class".
;;;
;;;## Class options
;;;
;;; Specifying `XML-CLASS' as a metaclass for a CLOS class has two
;;; effects. First, you can use slot indices and class indices for
;;; your class, as `XML-CLASS' inherits from `INDEXED-CLASS' (see the
;;; BKNR Indices tutorial for more information). Second, you can
;;; specify a DTD to use with your class, and an optional element name
;;; to which the class will be mapped. Please note that when
;;; you have multiple XML classes, it is best to specify the same DTD
;;; object, as `EQL' is used to reduce the list of DTDs when exporting
;;; XML objects.
;;;
;;; A CLOS class is mapped to an element of the DTD. In our example
;;; above, the class `BOOK' was mapped to the DTD element `book'. This
;;; is done by specifying the `:ELEMENT' class option. The XML
;;; import/export code then searchs the element definition in the DTD,
;;; and stores it in the class object. It then maps the slots of the
;;; class to the attributes and child element of the element
;;; definition in the DTD.
;;;
;;;### Specifying an empty class element
;;;
;;; The `:ELEMENT' class option is optional. The optional element
;;; specification is there to be able to derive objects which have the
;;; same attributes without having to rewrite everything. For example,
;;; suppose we have the following DTD:

<!ELEMENT test EMPTY>
<!ELEMENT test2 EMPTY>
<!ELEMENT test3 EMPTY>

<!ATTLIST test id ID #REQUIRED>
<!ATTLIST test2 id ID #REQUIRED>
<!ATTLIST test3 id ID #REQUIRED>

;;; We can then write the following class definitions:

(defvar *test-dtd* "../xml-impex/tutorial2.dtd")

(defclass test-object ()
  ((id :initarg :id :attribute "id"
       :parser #'parse-integer
       :index-type unique-index :index-reader object-with-id
       :index-values all-objects))
  (:metaclass xml-class)
  (:dtd-name *test-dtd*)
  (:element nil))

(defmethod print-object ((object test-object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (slot-value object 'id))))

(defclass test (test-object)
  ((id :index-type unique-index
       :index-reader test-with-id
       :index-values all-tests))
  (:metaclass xml-class)
  (:dtd-name *test-dtd*)
  (:element "test"))

(defclass test2 (test-object)
  ((id :index-type unique-index
       :index-reader test2-with-id
       :index-values all-test2s))
  (:metaclass xml-class)
  (:dtd-name *test-dtd*)
  (:element "test2"))

(defclass test3 (test-object)
  ((id :index-type unique-index
       :index-reader test3-with-id
       :index-values all-test3s))
  (:metaclass xml-class)
  (:dtd-name *test-dtd*)
  (:element "test3"))

;;; When we parse a sample file, we get the following results:

(parse-xml-file "../xml-impex/tutorial2.xml"
                (mapcar #'find-class '(test test2 test3)))
; => (:TEST3 (#<TEST3 3>) :TEST2 (#<TEST2 2>)
;     :TEST (#<TEST 1>))
(all-objects)
; => (#<TEST 1> #<TEST2 2> #<TEST3 3>)
(all-tests)
; => (#<TEST 1>)
(object-with-id 1)
; => #<TEST 1>
;    T
(test2-with-id 2)
; => #<TEST2 2>
;    T

;;;## Slot options
;;;
;;; The slots of a class can be mapped to different structures of the
;;; DTD. A slot can be:
;;;
;;;### The :attribute slot option
;;;
;;; an "attribute" - the value of the slot is stored as an attribute
;;; of the element corresponding to the class. For example, in the
;;; book example, both the slot `ID' and the slot `ISBN' are stored as
;;; attribute of the element "book". The import/export code verifies
;;; that the attributes are specified in the DTD.
;;;
;;;### The :element slot option
;;;
;;; an "element" - the value of the slot is stored as the CDATA body
;;; of a child element. For example, in the book example, both the
;;; slot `TITLE' and the slot `AUTOR' are stored as child elements of
;;; the element "book". The element has to be specified as a single or
;;; optional child of the class element. When the element is specified
;;; as multiple children (either "*" or "+" in the DTD), then the slot
;;; value is assumed to be a list of children. For example, if we have
;;; the DTD:

<!ELEMENT adult (child)*>
<!ELEMENT child EMPTY>
<!ATTLIST adult name CDATA #REQUIRED>
<!ATTLIST child name CDATA #REQUIRED>

;;; we can write the following class definition:

(defvar *adult-dtd* "../xml-impex/tutorial3.dtd")

(defclass adult ()
  ((name :initarg :name :attribute "name"
         :reader adult-name)
   (children :initarg :children :element "child"
             :reader adult-children :containment :*))

  (:metaclass xml-class)
  (:dtd-name *adult-dtd*)
  (:element "adult"))

(defmethod print-object ((adult adult) stream)
  (print-unreadable-object (adult stream :type t)
    (princ (adult-name adult) stream)))

(defclass child ()
  ((name :initarg :name :attribute "name"
         :reader child-name))
  (:metaclass xml-class)
  (:dtd-name *adult-dtd*)
  (:element "child"))

(defmethod print-object ((child child) stream)
  (print-unreadable-object (child stream :type t)
    (princ (child-name child) stream)))

;;; We can then parse the following XML file:

<family>
  <adult name="Clara">
    <child name="Robert"/>
    <child name="Anton"/>
  </adult>
  <adult name="Joseph">
    <child name="Ludwig"/>
    <child name="Benediktine"/>
  </adult>
</family>

(setf *adults* 
      (getf (parse-xml-file "../xml-impex/tutorial3.xml"
                            (mapcar #'find-class '(adult child)))
            :adult))
; => (#<ADULT Clara> #<ADULT Joseph>)
(adult-children (first *adults*))

; => (#<CHILD Robert> #<CHILD Anton>)
(write-to-xml *adults* :name "family")
; => <family>
;       <adult name="Joseph">
;          <child name="Benediktine"/>
;          <child name="Ludwig"/>
;       </adult>
;       <adult name="Clara">
;          <child name="Anton"/>
;          <child name="Robert"/>
;       </adult>
;    </family>

;;;### The :parent slot option
;;;
;;; Assume that we want to know who the parent of a child is. We can
;;; do this by creating a slot with the `:PARENT' slot option. This
;;; will automatically get filled by the object representing the
;;; parent element in the XML file.

(defclass child ()
  ((name :initarg :name :attribute "name"
         :reader child-name)
   (parent :initarg :parent :parent t
           :reader child-parent))
  (:metaclass xml-class)
  (:dtd-name *adult-dtd*)
  (:element "child"))

(setf *adults* 
      (getf (parse-xml-file "../xml-impex/tutorial3.xml"
                            (mapcar #'find-class '(adult child)))
            :adult))
; => (#<ADULT Joseph> #<ADULT Clara>)
(adult-children (first *adults*))
; => (#<CHILD Benediktine> #<CHILD Ludwig>)
(child-parent (first (adult-children (first *adults*))))
; => #<ADULT Joseph>

;;;### The :body slot option
;;;
;;; Sometimes, an XML element has a important string as body, and we
;;; would like to store this string in a slot. This can be done by
;;; setting the `:BODY' slot option. Note that only one PCDATA body is
;;; permitted when using this option. For example, we could use this
;;; code to read in the DTD:

<!ELEMENT book-resume (#PCDATA)>
<!ATTLIST book-resume id ID #REQUIRED
                      book-id CDATA #REQUIRED
                      reviewer CDATA #REQUIRED>

(defvar *resume-dtd* "../xml-impex/tutorial4.dtd")

(defclass book-resume ()
  ((id :initarg :id :attribute "id"
       :parser #'parse-integer
       :reader book-resume-id)
   (book-id :initarg :book-id :attribute "book-id"
            :parser #'parse-integer
            :reader book-resume-book-id)
   (reviewer :initarg :reviewer :attribute "reviewer"
             :reader book-resume-reviewer)
   (review :initarg :review :body t
           :reader book-resume-review))
  (:metaclass xml-class)
  (:dtd-name *resume-dtd*)
  (:element "book-resume"))

;;; Parsing the following file gives the results:

<resumes>
  <book-resume id="1" book-id="45"
               reviewer="Henry von der Grande">
Bla bla bla. Resume highlight blablabla foobar blorg.
  </book-resume>
  <book-resume id="2" book-id="1337"
               reviewer="L0rd 3v1l">
Bl4 bl4 bl4. R3s\/m3 h1ghl1ght bl4bl4bl4 f00b4r bl0rg.
  </book-resume>
</resumes>

(setf *resumes*
      (getf (parse-xml-file "../xml-impex/tutorial4.xml"
                            (list (find-class 'book-resume)))
            :book-resume))
; => (#<BOOK-RESUME {4947F22D}> #<BOOK-RESUME {4947DB95}>)
(book-resume-review (first *resumes*))
; => "
; Bl4 bl4 bl4. R3s\\/m3 h1ghl1ght bl4bl4bl4 f00b4r bl0rg.
; "
(write-to-xml *resumes* :name "resumes")
; => <resumes>
;       <book-resume book-id="1337" id="2"
;                    reviewer="L0rd 3v1l">
;    Bl4 bl4 bl4. R3s\/m3 h1ghl1ght bl4bl4bl4 f00b4r bl0rg.
;       </book-resume>
;       <book-resume book-id="45" id="1"
;                    reviewer="Henry von der Grande">
;    Bla bla bla. Resume highlight blablabla foobar blorg.
;      </book-resume>
;    </resumes>

;;;## Using IDs in XML import/export
;;;
;;; Often, you want to reference objects by ID in the XML file. For
;;; example, we could specify both authors and books on the toplevel,
;;; and reference the author by ID inside the document definition. The
;;; DTD would look like this:

<!ELEMENT book (title)>
<!ATTLIST book id ID #REQUIRED
               autor CDATA #REQUIRED>
<!ELEMENT title #PCDATA>
<!ELEMENT author (name)>
<!ATTLIST author id ID #REQUIRED>
<!ELEMENT name #PCDATA>

;;; We can write the following class definitions:

(defparameter *book2-dtd* "../xml-impex/tutorial5.dtd")

(defclass author ()
  ((id :initarg :id :reader author-id
       :attribute "id" :parser #'parse-integer
       :index-type unique-index :index-reader author-with-id
       :index-values all-authors)
   (name :initarg :name :reader author-name
         :element "name"))
  (:metaclass xml-class)
  (:dtd-name *book2-dtd*)
  (:element "author"))

(defmethod print-object ((author author) stream)
  (print-unreadable-object (author stream :type t)
    (princ (author-name author) stream)))

(defclass book ()
  ((id :initarg :id :reader book-id
       :attribute "id" :parser #'parse-integer
       :index-type unique-index :index-reader book-with-id
       :index-values all-books)
   (author :initarg :author :reader book-author
          :attribute "author" :parser #'parse-integer
          :id-to-object #'author-with-id
          :object-to-id #'author-id)
   (title :initarg :title :reader book-title
          :element "title"))
  (:metaclass xml-class)
  (:dtd-name *book2-dtd*)
  (:element "book"))
       
;;; We can then read the following XML file:

<library>
  <author id="1">
    <name>J.R.R Tolkien</name>
  </author>
  <author id="2">
     <name>William Gibson</name>
  </author>
  <book id="3" author="1">
    <title>Lord of the Rings</title>
  </book>
  <book id="4" author="2">
    <title>Neuromancer</title>
  </book>
</library>

(map nil #'clear-class-indices
     (mapcar #'find-class '(book author)))
; => NIL
(parse-xml-file "../xml-impex/tutorial5.xml"
                (mapcar #'find-class '(book author)))
; => (:BOOK
;     (#<BOOK "Neuromancer" {494AEFC5}>
;      #<BOOK "Lord of the Rings" {494AD48D}>)
;     :AUTHOR (#<AUTHOR William Gibson>
;              #<AUTHOR J.R.R Tolkien>))
(all-authors)
; => (#<AUTHOR J.R.R Tolkien> #<AUTHOR William Gibson>)
(all-books)
; => (#<BOOK "Lord of the Rings" {494AD48D}>
;     #<BOOK "Neuromancer" {494AEFC5}>)
(book-author (first (all-books)))
; => #<AUTHOR J.R.R Tolkien>
(write-to-xml (append (all-books) (all-authors))
              :name "library")
; => <library>
;       <book author="1" id="3">
;          <title>Lord of the Rings</title>
;       </book>
;       <book author="2" id="4">
;          <title>Neuromancer</title>
;       </book>
;       <author id="1">
;          <name>J.R.R Tolkien</name>
;       </author>
;       <author id="2">
;          <name>William Gibson</name>
;       </author>
;    </library>

