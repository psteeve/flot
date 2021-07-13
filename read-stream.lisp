(defpackage :read-stream
  (:use
   :common-lisp
   :positionable-stream)
  (:export
   :read-stream-on
   :next-match-for
   :at-end-p
   :next
   :reset
   :get-position
   :peek
   :peek-for
   :set-to-end
   :up-to
   :up-to-end
   :read-stream
   :limit-stream))

(in-package :read-stream)

(defclass read-stream (positionable-stream)
  ((limit-stream :initarg :limit-stream
                 :reader limit-stream))
  (:documentation "is a concrete subclass of positionable-stream that represents
 an accessor that can only read elements form its collection."))

(defmethod initialize-instance :after ((r read-stream) &key)
  (with-slots (contents limit-stream is-empty-p) r
    (setf limit-stream (1- (length contents)))
    (setf is-empty-p (= 0 (length contents)))))

(defun read-stream-on (value)
  (make-instance 'read-stream :on value))

(defmethod at-end-p ((stream read-stream))
  (with-slots (limit-stream) stream
    (>= (get-position stream) limit-stream)))

(defmethod next ((stream read-stream))
  (if (not (at-end-p stream))
      (with-slots (contents position) stream
        (setf position (1+ position))
        (elt contents position))
      nil))

(defmethod reset ((stream read-stream))
  (with-slots (position) stream
    (setf position -1)
    stream))

(defmethod peek ((stream read-stream))
  (if (not (at-end-p stream))
      (with-slots (contents position) stream
        (elt contents (1+ position)))
      nil))

(defmethod peek-for ((stream read-stream) value)
  (let ((same? (eql (peek stream) value)))
    (if same?
        (with-slots (position) stream
          (setf position (1+ (get-position stream))))
        same?)))

(defmethod set-to-end ((stream read-stream))
  (with-slots (position limit-stream) stream
    (setf position limit-stream)))

(defmethod up-to ((stream read-stream) value)
  (labels ((recur (result at-end-p)
             (let ((n (next stream)))
               (if (or (eql n value) at-end-p)
                   (nreverse result)
                   (progn
                     (push n result)                     
                     (recur result (at-end-p stream)))))))
    (recur '() (at-end-p stream))))

(defmethod up-to-end ((stream read-stream))
  (labels ((to-end (result)
             (if (at-end-p stream)
                 (reverse result)
                 (progn
                   (push (next stream) result)
                   (to-end result)))))
    (to-end '())))

(defmethod skip ((stream read-stream) n)
  (with-slots (position) stream
    (setf position (+ (get-position stream) n 1))
    (if (at-end-p stream)
        (set-to-end stream))))

(defmethod next-match-for ((stream read-stream) value)
  (eql (next stream) value))


