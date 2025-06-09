(asdf:defsystem :voipms
  :serial t
  :description "Common Lisp HTTP api client to the voip.ms api."
  :license "BSD"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :depends-on (:erjoalgo-webutil
               :local-time
               :cl-date-time-parser
               :vom)
  :components
  ((:file "voipms")))
