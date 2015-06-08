
(asdf:defsystem :s-blog
  :description "s-blog helps creating static websites hosted on Amazon S3"
  :version "0.0.2"
  :author "Crackbot <thecrackbot@gmail.com>"
  :licence "The MIT License (MIT)"
  :serial t
  :components ((:static-file "s-blog.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:file "main")
                                     (:file "api")
                                     (:file "s3")
                                     (:file "doc"))))
  :depends-on (:alexandria :cl-who :cl-fad :puri :zs3 :hunchentoot :parenscript :mgl-pax))
