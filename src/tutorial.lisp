
(defpackage :s-blog-example
  (:use :cl :s-blog))

(in-package :s-blog-example)

;; put in data

(defblog
  :host ""
  :static-paths '(#p"~/dev/2015/s-blog/public")
  :s3-config '(:ACCESS-KEY ""
               :SECRET-KEY ""
               :BUCKET "")
  :sync-on-compile t)

(defpage hello
  (:html (:body (:p "yo"))))
