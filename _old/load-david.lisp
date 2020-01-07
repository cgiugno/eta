; Load schema files
; (in directory 'schemas/')
;````````````````````````````
(mapcar (lambda (file) (load file))
    (directory "avatars/david/schemas/*.lisp"))


; Load general rule files
; (in directory 'rules/')
;`````````````````````````
(mapcar (lambda (file) (load file))
    (directory "rules/*.lisp"))


; Load topic-specific rule files
; (in subdirectories of 'rules/')
;`````````````````````````````````
(mapcar (lambda (directory)
    (mapcar (lambda (file) (load file))
        (directory (concatenate 'string (namestring directory) "/*.lisp"))))
    (remove nil (mapcar (lambda (p) (if (not (pathname-name p)) p)) (directory "rules/*"))))