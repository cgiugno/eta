;; *have-subdialogue*: development version 6
;;
;; TODO
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *have-subdialogue*

'(Event-schema (((set-of me you) have-subdialogue.v ?answer ?gists) ** ?e)
;``````````````````````````````````````````````````````````````````````````
;
;

:episodes 

?a1. (Me say-to.v you '?answer)
 
?a2. (You reply-to.v '?gists)

?a3. (Me react-to.v ?a2.)

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
