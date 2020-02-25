;;
;; *avatar* : specify the name of one of the available avatars to use
;; *live-mode* : T for speak mode (inputs/outputs are done using file IO, to be used in conjunction with ASR and TTS programs),
;;               NIL for text mode (inputs/outputs are done through the command line)
;; *perceptive-mode* : T to observe coordinates of objects in the world during spatial questions,
;;                     NIL to skip this step
;;                    (NOTE: currently only relevant to david/blocks world)
;; *responsive-mode* : T to allow for natural language response generation (THIS REQUIRES QUICKLISP DEPENDENCIES TO BE INSTALLED),
;;                     NIL to use local versions of dependencies and skip response generation step
;;                     (NOTE: currently only relevant to david/blocks world, can safely be disabled for other avatars)
;; *safe-mode* : T to exit smoothly if exception is thrown during execution,
;;               NIL otherwise
;; *user-id* : unique ID of user (potentially overwritten by sessionInfo.lisp if in live mode)
;;

(defparameter *avatar* "sophie")
(defparameter *live-mode* NIL)
(defparameter *perceptive-mode* NIL)
(defparameter *responsive-mode* NIL)
(defparameter *safe-mode* NIL)
(defparameter *user-id* "test")
