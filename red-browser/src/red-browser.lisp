(in-package :org.iodb.red-browser)

(defun http-get (url)
  (restart-case (trivial-http:http-get url)
    (try-again ()
      (http-get url))))
