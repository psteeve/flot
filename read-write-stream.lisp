(defpackage :read-write-stream
  (:use :common-lisp
   :write-stream)
  (:export :read-write-stream))

(in-package :read-write-stream)

(defclass read-write-stream (write-stream)
  ()
  (:documentation "Represents accessor that can both read and write elements into its collection. It supports all the methods of both read-stream and write-stream."))
