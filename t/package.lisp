(defpackage :sigil-cl/t
  (:use :cl :sigil-cl)
  (:local-nicknames (:jzon :com.inuoe.jzon)
                    (:bt2 :bordeaux-threads-2))
  (:export #:run-tests))
