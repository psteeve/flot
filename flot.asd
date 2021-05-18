(asdf:defsystem :flot
  :serial t
  :components ((:file "stream")
               (:file "positionable-stream")
               (:file "read-stream")               
               (:file "write-stream")               
               (:file "read-write-stream")               
               (:file "flot")
               (:file "stream-test"))
  :depends-on (:st-test))

