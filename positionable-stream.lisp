(defpackage :positionable-stream
  (:use
   :common-lisp
   :flot.stream)
  (:export
   :positionable-stream
   :peek
   :peek-for
   :up-to
   :reverse-contents
   :reset
   :set-to-end
   :skip
   :skip-to
   :contents
   :is-empty-p
   :get-position))

(in-package :positionable-stream)

(defclass positionable-stream (flot)
  ((position :initform -1
             :reader get-position)
   (is-empty-p :reader is-empty-p))
  (:documentation "A positionable-stream is a subclass of flot. 
It provide additional methods appropriate to streams that can reposition their position references, but, 
it is an abstract class because it does not provide an implementation of the inherited methods next and next-put. 
The implementation of theses generics is left to the subclasses of positionable-stream: read-stream, write-stream and read-write-stream."))

(defgeneric peek (positionable-stream)
  (:documentation "Answer the next element in the collection (as in the method next), but do not change the position reference. Answer nil if reference position is at the end."))

(defgeneric peek-for (positionable-stream value)
  (:documentation "Determine the response of the method peek. If it is the same as the argument, \"value\", then increment the position reference and answer true. Otherwise answer false and do not change the position reference."))

(defgeneric up-to (positionable-stream value)
  (:documentation "Answer a collection of elements starting with the next element accessed in the collection, and up to, not inclusive of, the next element that is equal to \"value\". If \"value\" is not in the collection, answer the entire rest of the collection."))

(defgeneric reverse-contents (positionable-stream)
  (:documentation "Answer a copy  of the receiver's contents in reverse order."))

(defgeneric reset (positionable-stream)              
  (:documentation "Set the reference position to the beginning of the collection."))

(defgeneric set-to-end (positionable-stream)
  (:documentation "Set the reference position to the end of the collection."))

(defgeneric skip (positionable-stream n)
  (:documentation "Set the reference position to be the current position plus the argument, \"n\", possibly adjusting the result so as the remain within the bounds of the collection."))

(defgeneric skip-to (positionable-stream value)
  (:documentation "Set the reference position to be past the next occurence of the argument, \"value\", in the collection. Answer whether such an occurrence existed."))

