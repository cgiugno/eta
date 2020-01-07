;; January 6/2020
;; ================================================
;;
;; Starts Eta. Specify the avatar being used, and the modes to be used, using the
;; global parameters at the top of this file.
;;

; Which avatar to use
(defparameter *avatar* "david")

; T for speak mode
; NIL for text mode
(defparameter *mode* NIL)

; T to observe coordinates
; NIL to ignore
(defparameter *coords-mode* NIL)

; T to exit smoothly if exception is thrown
; NIL otherwise
(defparameter *safe-mode* NIL)

; ID of user
(defparameter *user-id* NIL)



(defun clean-io-files ()
;``````````````````````````
; Overwrites all io files used by Eta with blank files
;
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
  ; Delete the content of coords.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/coords.lisp" :direction :output :if-exists 
                                              :supersede :if-does-not-exist :create))
  ; Delete the content of reaction.lisp, if it exists, otherwise create
  (with-open-file (outfile "./io/answer.lisp" :direction :output :if-exists 
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
        (remove nil (mapcar (lambda (p) (if (not (pathname-name p)) p))
          (directory (concatenate 'string directory "/*")))))))
    (load-files-recur (concatenate 'string "./avatars/" avatar-name)))
) ; END load-avatar-files


; If text mode (or sessionInfo missing), manually set user-id
;````````````````````````````````````````````````````````````````
  (if (or (not *mode*) (not (probe-file "./io/sessionInfo.lisp")))
    (progn
      ;; (format t "~%~%Enter user-id ~%")
      ;; (princ "user id: ") (finish-output)
      ;; (setq *user-id* (write-to-string (read))))
      (setq *user-id* "test"))
; Otherwise, load user-id from sessionInfo file
;``````````````````````````````````````````````````````
    (progn
      (load "./io/sessionInfo.lisp")))


; Clean IO files, load Eta, and load avatar-specific files
;``````````````````````````````````````````````````````````
(clean-io-files)
(load "load-eta.lisp")
(load-avatar-files *avatar*)


; Create hash table to store gist clauses
;```````````````````````````````````````````
(defvar *gist-kb* (make-hash-table :test #'equal)); for storage of gist
                ; clauses, particularly ones obtained from user inputs;
(clrhash *gist-kb*)


; Run Eta
;`````````````
(if *safe-mode*
  (handler-case (eta *mode* :perceive-coords *coords-mode*)
    (error (c)
      (error-message "Execution of Eta failed due to an internal error." *mode*)
      (values 0 c)))
  (eta *mode* :perceive-coords *coords-mode*))