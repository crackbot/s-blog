
(in-package :s-blog)

(defsection @s3-sync (:title "Publish your files via Amazon S3.")
  (s3-sync-page function)
  (s3-sync-path function))
                             
(defun s3-sync-page (blog page s3-config)
  "Sync page to s3."

  (format t "page: ~A~%" page)
  (format t "content: ~A~%" (process-content page))
  
  (labels ((upload-page (filename)
             (format t "~%syncing page: ~A~%" filename)
             (zs3:put-object (process-content page)
                             (getf s3-config :bucket)
                             filename
                             :credentials (s3-creds s3-config)
                             :access-policy :PUBLIC-READ
                             :content-type "text/html")))
    (let* ((pn (page-name page))
          (name (symbol-name pn)))
      (if (eq (slot-value blog 'no-postfix) t)
          (upload-page (concatenate 'string (string-downcase name) "/index.html"))
          (upload-page (page-name-to-file pn))))))
  
(defun s3-sync-path (path s3-config)
  "Sync path to s3."
  (labels ((build-key (filename ext)
             (let ((pathname (subseq (namestring filename)
                                     (+ 1 (length (namestring path))))))
               (concatenate 'string
                            (subseq pathname 0 (search (pathname-type pathname) pathname))
                            ext)))
           (sync-file (filename)
             (when (not (blacklisted-p filename))
               (multiple-value-bind (data new-ext)
                   (process-file filename)
                 (zs3:put-object data
                                 (getf s3-config :bucket)
                                 (build-key filename new-ext)
                                 :credentials (s3-creds s3-config)
                                 :access-policy :public-read
                                 :content-type (hunchentoot:mime-type filename))))))
    (cl-fad:walk-directory path #'sync-file)))
