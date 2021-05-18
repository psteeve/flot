(defpackage :stream-test
  (:use
   :common-lisp
   :read-stream
   :st-test))

(in-package :stream-test)

(st-test:deftest test-read-stream-on ()
  (check (equal (type-of (read-stream:read-stream-on "test")) 'read-stream::read-stream)))

(st-test:deftest test-at-end-p ()
  (check (equal (read-stream:at-end-p
                 (read-stream:read-stream-on "test"))
                nil)))

(st-test:deftest test-next ()
  (check (equal (read-stream:next
                 (read-stream:read-stream-on "test"))
                #\t)))

(st-test:deftest test-reset ()
  (check (equal (read-stream:get-position
                 (read-stream:reset (read-stream:read-stream-on "test")))
                -1)))

(st-test:deftest test-peek ()
  (let ((stream (read-stream:read-stream-on "test")))
    (combine-results
      (check (equal (read-stream:peek stream)
                    #\t))
      (check (equal (read-stream:get-position stream) -1)))))
                    
(st-test:deftest test-peek-for ()
  (let ((stream (read-stream:read-stream-on "test")))
    (combine-results
      (check (equal (read-stream:peek-for stream #\e)
                    nil))
      (check (equal (read-stream:peek-for stream #\t) 0)))))

(st-test:deftest test-set-to-end ()
  (let ((stream (read-stream:read-stream-on "test")))
    (read-stream:set-to-end stream)
    (check (equal (read-stream:at-end-p stream) 't))))
                   



