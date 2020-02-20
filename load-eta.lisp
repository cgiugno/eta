; If *responsive-mode* is enabled, we need various dependencies such as ulf2english and ulf-pragmatics.
; Otherwise, we can just use local ttt.
(cond
(*responsive-mode*

    ; Load ttt
    ;```````````````````
    (ql:quickload "ttt")

    ; Load ulf-lib
    ;```````````````````
    (ql:quickload "ulf-lib")

    ; Load ulf2english
    ;```````````````````
    (ql:quickload "ulf2english")

    ; Load ulf-pragmatics
    ;``````````````````````
    (ql:quickload "ulf-pragmatics"))

((not *responsive-mode*)

    ; Load local ttt
    ;``````````````````
    (load (truename "local_packages/ttt/src/load.lisp"))

    ; Load local ulf-lib
    ;`````````````````````
    (load (truename "local_packages/ulf-lib/load.lisp"))

    ; Load local ulf2english
    ;````````````````````````
    (load (truename "local_packages/ulf2english/load.lisp"))

    ; Load local ulf-pragmatics
    ;```````````````````````````
    (load (truename "local_packages/ulf-pragmatics/load.lisp"))))


; Load core code
; (in directory 'core/')
;````````````````````````
(mapcar (lambda (file) (load file))
    (directory "core/*.lisp"))


; Load core response generation code
; (in directory 'core/response')
;`````````````````````````````````
(mapcar (lambda (file) (load file))
    (directory "core/response/*.lisp"))


; Load core coreference code
; (in directory 'core/coref')
;``````````````````````````````
(mapcar (lambda (file) (load file))
    (directory "core/coref/*.lisp"))


; Load core resources
; (in directory 'core/resources/')
;``````````````````````````````````
(mapcar (lambda (file) (load file))
    (directory "core/resources/*.lisp"))