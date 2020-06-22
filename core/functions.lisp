;; August 8/19 
;; ================================================
;;
;; Definitions for any ELF functions used in schema formulas.
;;

(defun get-object-locations.f (x)
; `````````````````````````````
; Extracts all locations (at-loc.p propositions) from a list of
; propositions reflecting Eta's current perceptions.
;
  (setq x (eval x))
  (if (listp x) (remove-if-not #'at-loc-prop? x) nil)
) ; END get-object-locations.f



(defun get-actions.f (x)
; `````````````````````````
; Extracts all perceived actions (verb phrases) from a list of
; propositions reflecting Eta's current perceptions.
;
  (setq x (eval x))
  (if (listp x) (remove-if-not #'verb-phrase? x) nil)
) ; END get-object-locations.f



(defun ulf-of.f (x)
; ````````````````````
; Retrieves ulf attached to action proposition name.
; TODO: Currently this just returns the first ulf if there
; are multiple attached to an action name. This doesn't make
; much sense, and should be changed in the future. This also
; ties into the issue of a user say-to.v act having a potentially
; indefinite amount of gist clauses attached... might it make sense
; to use iteration in interpreting the user's response until, say,
; an end of turn is recorded in context?
;
  (if (and (symbolp x) (get x 'ulf)) `(quote ,(car (remove nil (get x 'ulf)))) nil)
) ; END ulf-of.f



(defun gist-of.f (x)
; `````````````````````
; Retrives gist clauses attached to action proposition name.
; TODO: See issue with ulf-of.f
;
  (if (and (symbolp x) (get x 'gist-clauses)) `(quote (car (get x 'gist-clauses))) nil)
) ; END gist-of.f



(defun main-answer.f (x)
; ```````````````````````
; Given a list of an answer and alternatives, split off the main answer from the list.
; If a string is given, it is assumed that it is the main answer (with no alternatives).
;
  (cond
    ((and (listp (eval x))) (car (eval x)))
    ((stringp (eval x)) (eval x)))
) ; END main-answer.f



(defun relevant-answers.f (x y)
; ```````````````````````````````
; Given a list of answer relations and a query ulf, we want to get the answers that are needed to generate a response
; TODO (?)

) ; END relevant-answers.f