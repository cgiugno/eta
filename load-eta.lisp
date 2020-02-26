; If *responsive-mode* is enabled, we need various dependencies such as ulf2english and ulf-pragmatics.
; Otherwise, we can just use local ttt.
(cond
(*responsive-mode*

    ; Quickload packages
    ;```````````````````
    (load (truename "packages/quickload-dependencies.lisp"))
    (quickload-packages '("ttt" "ulf-lib" "ulf2english" "ulf-pragmatics")))

((not *responsive-mode*)

    ; Load local ttt
    ;``````````````````
    (load (truename "packages/local/ttt/src/load.lisp"))

    ; Load local ulf-lib
    ;`````````````````````
    (load (truename "packages/local/ulf-lib/load.lisp"))

    ; Load local ulf2english
    ;````````````````````````
    (load (truename "packages/local/ulf2english/load.lisp"))

    ; Load local ulf-pragmatics
    ;```````````````````````````
    (load (truename "packages/local/ulf-pragmatics/load.lisp"))))


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