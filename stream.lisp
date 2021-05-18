(defpackage :flot.stream
  (:use :common-lisp)
  (:export
   :flot
   :at-end-p
   :next
   :next-put
   :next-put-all
   :do-each
   :contents
   :next-match-for))

(in-package :flot.stream)

(defclass flot ()
  ((contents :initarg :on
             :initform (error "Must provide a value to stream on.")
             :reader contents
             :documentation "Answer all the objects in the collection.")))

(defgeneric at-end-p (flot)
  (:documentation "Answer if cannot access any more objects"))

(defgeneric next (flot)
  (:documentation "Answer the next object accessible in the collection."))

(defgeneric next-put (flot value &optional n)
  (:documentation "Store the argument, \"value\", as the next element accessible in the collection.
Or as the next \"n\" number of elements accessible in the collection."))

(defgeneric next-put-all (flot values)
  (:documentation "Store the elements in the argument, \"values\", as the next elements accessible in the collection."))

(defgeneric next-match-for (flot value)
  (:documentation "Access the next object and answer whether it is equal to the argument, \"value\""))

(defgeneric do-each (flot func)
  (:documentation "Evaluate the argument, \"func\", for each of the remaining elements that can be accessed in the collection."))
