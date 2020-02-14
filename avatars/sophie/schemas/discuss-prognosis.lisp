;; *prognosis-schema*: development version 6
;;
;; TODO
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *prognosis-schema*

'(Event-schema (((set-of me you) discuss-prognosis.v) ** ?e)
;```````````````````````````````````````````````````````````
;
;

:episodes 

?a1. (Me say-to.v you '(Can you tell me what kind of time we\'re looking at ?))

?a2. (You reply-to.v ?a1.)

?a3. (Me react-to.v ?a2.)

)) ; END defparameter *prognosis-schema*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'discuss-prognosis.v '*prognosis-schema*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*prognosis-schema* 'semantics) (make-hash-table))
(setf (get '*prognosis-schema* 'gist-clauses) (make-hash-table))
(setf (get '*prognosis-schema* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*prognosis-schema*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*prognosis-schema*))
  '(
    (?a1.  ((What is my prognosis ?)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*prognosis-schema*))
  '(
    (?a1.  (prognosis))
  )
) ; END mapcar #'store-topic-keys
