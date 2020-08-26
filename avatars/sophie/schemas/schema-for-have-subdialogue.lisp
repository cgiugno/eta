;; *have-subdialogue*: development version 6
;;
;; TODO
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *have-subdialogue*

'(event-schema :header (((set-of ^me ^you) have-subdialogue.v ?response ?gists) ** ?e)
;```````````````````````````````````````````````````````````````````````````````````````
;
;

:episodes (

?e1 (^me say-to.v ^you '?response)
 
?e2 (^you reply-to.v '?gists)

?e3 (^me react-to.v ?e2)

)

)) ; END defparameter *have-subdialogue*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'have-subdialogue.v '*have-subdialogue*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*have-subdialogue* 'semantics) (make-hash-table))
(setf (get '*have-subdialogue* 'gist-clauses) (make-hash-table))
(setf (get '*have-subdialogue* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*have-subdialogue*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*have-subdialogue*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*have-subdialogue*))
  '()
) ; END mapcar #'store-topic-keys
