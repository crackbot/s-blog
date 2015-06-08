
(in-package :s-blog)

(defsection @api (:title "s-blog API")
  (defblog macro)
  (defpage macro))
  
(defmacro defblog (&key host (static-paths '()) s3-config
                     (sync-on-compile nil))
  "Define new s-blog. Each blog is tied to defining package, meaning
that you can have only one blog per package."
  (let ((global-blog (intern "*BLOG*" *package*))
        (sync-blog (intern "SYNC-BLOG" *package*))
        (sync-name (intern "SYNC-NAME" *package*))
        (sync-page (intern "SYNC-PAGE" *package*))
        (sync-static-paths (intern "SYNC-STATIC-PATHS" *package*)))
  `(progn
     (let ((blog (make-instance 'blog
                                :host ,host
                                :static-paths ,static-paths
                                :s3-config ,s3-config
                                :sync-on-compile ,sync-on-compile)))
       
       (setf (gethash ,(find-package *package*) *blogs*)
             blog)
       
       (defparameter ,global-blog blog)
       
       (defun ,sync-blog ()
         "Sync whole blog including static paths to s3."
         (every-page #',sync-page blog)
         (,sync-static-paths))

       (defun ,sync-name (page-name)
         (,sync-page (find-page blog page-name)))
       
       (defmethod ,sync-page ((page page))
         "Sync specific page."
         (s-blog::s3-sync-page blog
                               page
                               (slot-value blog 's3-config)))
       
       (defun ,sync-static-paths ()
         "Sync static paths tied to a blog."
         (dolist (path (static-paths blog))
           (s-blog::s3-sync-path path (slot-value blog 's3-config))))
       
       ;; (defun preview ()
       ;;   "Preview builds hunchentoot controllers and views and launches http server
       ;;    so you can preview your pages."
       ;;   (every-page #'setup-page-route blog)
       ;;   (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port (preview-port blog))))
       
       ;; (defun stop ()
       ;;   "Stops the preview server."
       ;;   )
       
       ;; (defun build (&optional output-path)
       ;;   "Build creates all html pages and saves it to given path."
       ;;   (mapcar #'process-page *pages*)
       
       ;;   )
       ))))

(defmacro defpage (name &body body)
  "Define new page inside the blog.

Page name is later translated into url which is used to access it on
the web. For example page named as /test/page will be translated to
either test/page.html or test/page/index.html depending on :no-prefix
setting."
  (let ((page-name)
        (page-class)
        (pg (gensym))
        (sync-page (intern "SYNC-PAGE" *package*)))
    (cond ((listp name)
           (setq page-name (car name)
                 page-class (cadr name)))
          (t (setq page-name name
                   page-class 'page)))
    `(let ((blog (find-blog ,(find-package *package*)))
           (,pg (make-instance (quote ,page-class)
                               :name (quote ,page-name)
                               :content (quote ,@body))))
       
                               ;; :html-content (cl-who:with-html-output-to-string (out)
                               ;;                 ,@body))))
       (setf (gethash (quote ,page-name) (pages blog))
             ,pg)
       (when (eq (slot-value blog 'sync-on-compile) t)
         (,sync-page ,pg)))))

;; (defclass s-blog-acceptor (hunchentoot:acceptor)
;;   ()
;;   (:documentation "This is the acceptor of the s-blog preview."))

;; (defun setup-page-route (page)
;;   (let ((name (page-name page))
;;         (uri (page-name-to-file (page-name page))))
;;     ;; define-easy-handler doesn't support calculated names

;;     (push (list uri t name) *easy-handler-alist*)
    
;;     (hunchentoot:define-easy-handler (name :uri uri) ()
;;       (setf (hunchentoot:content-type*) "text/html")
;;       (format nil (page-html-content page)))))

;; (defun delete-page (page-name)
;;   (let ((conf ,s3-config)
;;         (bucket (dn-to-bucket ,domain-name))
;;         (path (page-name-to-file page-name)))
;;     (zs3:delete-object bucket path :credentials (s3-creds conf))))
