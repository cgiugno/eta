
;; fast start-up: with this file, we can use
;;   (load "start-lissa5.lisp") or (load "start-lissa5")
;;   (lissa t)
;; to get under way
;(load *ttt-addr*)
;(load "C:/inetpub/wwwroot/RocSpeakRafayet/ttt/ttt/src/load")
(load "lissa5-schema"); schematic dialog steps
(load "choose-gist-clause-trees-for-input.lisp")
(load "choose-reaction-to-input.lisp")
(load "rules-for-how-long-in-rochester-input.lisp")
(load "rules-for-like-about-rochester-input.lisp")
(load "rules-for-not-like-about-rochester-input.lisp")
(load "rules-for-changing-rochester-input.lisp")

(format t "~%~%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ~
             ~% TO RUN LISSA IN PRINT-MODE OR TALK-MODE, USE RESPECTIVE CALLS
             ~%           (lissa nil)    (lissa t)
             ~%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ~
             ~% ALSO NOTE: For inhibiting repetitive outputs via latency, do ~
             ~%            (setq *use-latency* T) ~
             ~% THOUGH THAT'S NOT RECOMMENED IN THE LISSA DEVELOPMENT PHASE
             ~%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        


; activate one of the following code lines:
(lissa *mode*) ; for one round
;(mainLoop)  ; for repeated round
