
(in-package :s-blog)

(defsection @main-manual (:title "Main")
  (s-blog asdf:system)
  
  "Very opinionated, small library, to create static websites."
  (@api section)
  (@s3-sync section)
  (@blacklist section)
  (@lowlevel section))

(defsection @lowlevel (:title "Low level")
  (*blogs* variable)
  (blog class)
  (no-postfix (reader blog))
  (host (accessor blog))
  (static-paths (accessor blog))
  (s3-config (accessor blog)))

;; toplevel definitions

(defparameter *blogs* (make-hash-table)
  "Global variable holding the package -> blog mapping")

(defclass blog ()
  ((pages :initform (make-hash-table :test #'equal) :accessor pages)
   (host
    :initform nil :initarg :host :accessor host :type string
    :documentation "Host where you can access the blog.")
   (static-paths
    :initform (list) :initarg :static-paths :accessor static-paths
    :documentation "Static paths holds different static files that
    needs to be synced, javascript or css for example, for some files
    types there is also preprocessing that will happen.")
   (s3-config
    :initform nil :initarg :s3-config :accessor s3-config
    :documentation "S3 config is a plist with :access-key :secret-key
    props. Optionally you can specify :bucket, otherwise it will be
    determined from hostname, which may not always produce the results
    that you want. You need to specify this if you want to publish
    your blog through amazon s3.")
   (no-postfix
    :initform nil :initarg :no-postfix :reader no-postfix
    :documentation "If set to t will create a path named as page name
    and place page source as index.html. This is done to have nicer
    URLs in browser, for example instead of /test.html you will get
    /test/")
   (preview-port
    :initform 4242 :initarg :preview-port :accessor :preview-port
    :documentation "HTTP port that will be used to launch hunchentoot
    server if you want to preview your blog locally.")
   (sync-on-compile
    :initform nil :initarg :sync-on-compile
    :documentation "If set to ``t`` and s3-config is set will push the
    page whenever it gets compiled.")))

(defmethod initialize-instance :after ((blog blog) &rest init-args)
  "Some patchup work that needs to happen on new blog initialization."
  (let ((conf (getf init-args :s3-config)))
    (when (not (getf conf :bucket))
      (setf (getf conf :bucket) (dn-to-bucket (domain-name blog)))))
  ;; make sure bucket is created after initializing the blog
  (new-bucket (getf (s3-config blog) :bucket)
              (s3-creds (s3-config blog))))

(defclass page ()
  ((name :initform nil :initarg :name :accessor page-name)
   (content :initform nil :initarg :content :accessor page-content)
   (html-content :initform nil :initarg :html-content :accessor page-html-content)))

(defmethod process-content ((page page))
  "Return processed content, for page default content is in cl-who format"
  (let ((content (page-content page)))
    (eval `(cl-who:with-html-output-to-string (out) ,content))))

(defun find-blog (&optional (package *package*))
  "Find blog associated with package."
  (gethash package *blogs*))

(defun find-page (blog page-name)
  "Find page object named as page-name inside blog"
  (gethash page-name (pages blog)))

(defun every-page (fn blog)
  "Iterate through every page in blog calling function on each."
  (dolist (page (alexandria:hash-table-values (pages blog)))
    (funcall fn page)))

;; various helper functions

(pushnew '("text/javascript" "paren") hunchentoot::*mime-type-list*)
(setf (gethash "paren" hunchentoot::*mime-type-hash*)
      "text/javascript")

(defun dn-to-bucket (dn)
  "Transform domain name to s3 bucket name format"
  (puri:uri-host (puri:parse-uri dn)))

(defun s3-creds (s3-config)
  "Format s3 creds for zs3."
  (list (getf s3-config :access-key)
        (getf s3-config :secret-key)))

(defun new-bucket (bucket-name s3-config)
  "Create new bucket."
  (zs3:create-bucket bucket-name :credentials s3-config))

(defun page-name-to-file (page-name &key (ext ".html"))
  "Transform page name to path/filename with .html extension."
  (concatenate 'string
               (string-downcase (symbol-name page-name))
               ext))

(defun read-file (filename)
  (let* ((stream (open filename))
         (seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

;; process static files found in paths

(defun process-file (filename)
  "Process file based on extension."
  (let ((ext (intern (string-upcase (pathname-type filename)) :keyword)))
    (process-ext filename ext)))

(defmethod process-ext (filename (ext (eql :who)))
  "Opens up who-file and does the processing, output is a string."
  (let ((forms (read-file filename)))
    (values (eval `(cl-who:with-html-output-to-string (out) ,forms))
            "html")))

(defmethod process-ext (filename (ext (eql :paren)))
  "Process parenscript files."
  (values (ps:ps-compile-file filename) "js"))

(defmethod process-ext (filename ext)
  "Process all other file types."
  (values (read-file filename) (string-downcase (symbol-name ext))))

(defun process-page (page)
  "Given a page struct instance builds html-content from who-content."
  (setf (page-html-content page)
        (cl-who:with-html-output-to-string (preprocess-who (page-who-content page))))
  page)

(defun preprocess-who (who)
  "Page who needs to be preprocessed to use correct urls for other
   static resources."
  who)

;; blacklist files

(defsection @blacklist (:title "Blacklist static files")
  "Some files you don't want to get processed and put out there. For
cases like this there is a blacklist feature, which is a list of
functions if any of this functions returns t then sync process will
ignore the file."

  (*filename-bl* variable)
  (blacklisted-p function))

(defparameter *filename-bl*
  (list (lambda (fn name)
          (declare (ignore fn))
          (eq (- (length name) 1) (search "~" name))))
  "Variable holding the list of blacklist functions.")

(defun blacklisted-p (filename)
  "Checks if filename is blacklisted or not, also see *filename-bl*"
  (let ((name (namestring filename)))
    (some #'(lambda (fun)
              (funcall fun filename name))
          *filename-bl*)))
