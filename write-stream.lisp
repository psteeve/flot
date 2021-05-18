(defpackage :write-stream
  (:use
   :common-lisp
   :positionable-stream)
  (:export
   :write-stream
   :cr
   :cr-tab
   :-space
   :tab))

(in-package :write-stream)

(defclass write-stream (positionable-stream)
  ()
  (:documentation "Is a subclass of positionable-stream representing accessors for writing elements into a collection."))

(defgeneric cr (write-stream)
  (:documentation "Store the return character as the next element in the collection."))

(defgeneric cr-tab (write-stream &optional n)
  (:documentation "Store the return character and a single tab character as the next two elements in the collection.
Follow the return character by n number of tab characters if n is provided."))

(defgeneric -space (write-stream)
  (:documentation "Store the space character as the next element accessible in the collection."))

(defgeneric tab (write-stream)
  (:documentation "Store the tab character as the next element accessible in the collection."))
