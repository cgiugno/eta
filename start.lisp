;; January 6/2020
;; ================================================
;;
;; Starts Eta using the configuration specified in config.lisp
;;

(load "./config.lisp")


(defun clean-io-files ()
;``````````````````````````
; Overwrites all io files used by Eta with blank files
;
  (ensure-directories-exist "./io/")
  (when *read-log-mode*
    (ensure-directories-exist "./logs/")
    (ensure-directories-exist "./logs/logs/")
    (ensure-directories-exist "./logs/logs_out/"))

  ; Delete the content of the sessionInfo.lisp file after reading
  (with-open-file (outfile "./io/sessionInfo.lisp" :direction :output :if-exists
                                                   :supersede :if-does-not-exist :create))
  ; Delete the content of output.txt, if it exists, otherwise create
  (with-open-file (outfile "./io/output.txt" :direction :output :if-exists 
                                             :supersede :if-does-not-exist :create))
  ; Delete the content of input.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/input.lisp" :direction :output :if-exists 
                                             :supersede :if-does-not-exist :create))
  ; Delete the content of ulf.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/ulf.lisp" :direction :output :if-exists 
                                           :supersede :if-does-not-exist :create))
  ; Delete the content of perceptions.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/perceptions.lisp" :direction :output :if-exists 
                                              :supersede :if-does-not-exist :create))
  ; Delete the content of answer.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/answer.lisp" :direction :output :if-exists 
                                              :supersede :if-does-not-exist :create))
  ; Delete the content of goal-request.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/goal-request.lisp" :direction :output :if-exists 
                                                    :supersede :if-does-not-exist :create))
  ; Delete the content of goal-rep.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/goal-rep.lisp" :direction :output :if-exists 
                                                :supersede :if-does-not-exist :create)) 
  ; Delete the content of planner-input.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/planner-input.lisp" :direction :output :if-exists 
                                                     :supersede :if-does-not-exist :create))    
  ; Delete the content of user-try-ka-success.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/user-try-ka-success.lisp" :direction :output :if-exists 
                                                           :supersede :if-does-not-exist :create))                                                                           
) ; END clean-io-files





(defun load-avatar-files (avatar-name)
;``````````````````````````````````````
; Loads all schema and rule files used by a particular avatar
;
  (labels ((load-files-recur (directory)
      (mapcar (lambda (d)
          (mapcar (lambda (f) (load f))
            (directory (concatenate 'string (namestring d) "/*.lisp")))
          (load-files-recur (coerce (butlast (explode (namestring d))) 'string)))
        (remove nil (mapcar (lambda (p)
            ; This is pretty awkward, but has to be done to handle differences btwn ACL and SBCL
            (if (fboundp 'probe-directory)
              (if (probe-directory p) p)
              (if (not (pathname-name p)) p)))
          (directory (concatenate 'string directory "/*")))))))
    (load-files-recur (concatenate 'string "./avatars/" avatar-name)))
) ; END load-avatar-files


; If live mode, load *user-id* from sessionInfo file (if it exists).
; Otherwise, manually set *user-id* (or prompt user for input).
;````````````````````````````````````````````````````````````````
(defparameter *user-id* nil)
(if (and *live-mode* (probe-file "./io/sessionInfo.lisp"))
  (load "./io/sessionInfo.lisp"))
(when (not *user-id*)
  (defparameter *user-id* "_test")
  ;; (format t "~%~%Enter user-id ~%")
  ;; (princ "user id: ") (finish-output)
  ;; (setq *user-id* (write-to-string (read))))
)


; Clean IO files, load Eta, and load avatar-specific files
;``````````````````````````````````````````````````````````
(clean-io-files)
(load "load-eta.lisp")
(load-avatar-files *avatar*)


(cond

  ; Run Eta (safe mode)
  ;`````````````````````````
  (*safe-mode*
    (handler-case (eta nil *live-mode* *perceptive-mode* *responsive-mode*)
      (error (c)
        (error-message "Execution of Eta failed due to an internal error." *live-mode*)
        (values 0 c))))

  ; Run Eta (read-log mode)
  ;`````````````````````````
  (*read-log-mode*
    (let ((logs (if (stringp *read-log-mode*)
                  (directory (concatenate 'string "logs/logs/" *read-log-mode*))
                  (directory "logs/logs/*"))))
      ; Create empty log_out file
      (mapcar (lambda (log)
        (with-open-file (outfile (pathname (concatenate 'string "logs/logs_out/" (pathname-name log)))
          :direction :output :if-exists :supersede :if-does-not-exist :create))) logs)
      ; Start eta using log
      (mapcar (lambda (log)
        (format t "==:: READING LOG ~a ::==~%" log)
        (load "load-eta.lisp")
        (load-avatar-files *avatar*)
        (eta log nil t t)) logs)))

  ; Run Eta
  ;`````````````````````````
  (t (eta nil *live-mode* *perceptive-mode* *responsive-mode*)))


; Write user gist clauses to file
;````````````````````````````````````
(print-gist-kb :filename
  (ensure-directories-exist (concatenate 'string "./gist-kb/" *user-id* ".txt")))