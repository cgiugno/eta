(defun generate-response (query-ulf relations)
; `````````````````````````````````````````````
; NOTE: Check for poss-ques:
; "You are not sure if you understood the question correctly\, but your answer is"
;
; Example queries:
; '(((THE.D (SRI  BLOCK.N)) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
; '(((WHAT.D BLOCK.N) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
; '(((WHAT.D (PLUR BLOCK.N)) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
; '(((PRES BE.V) THERE.PRO (A.D BLOCK.N) (ON.P (THE.D (|SRI | BLOCK.N)))) ?)
; '(((PRES BE.V) THERE.PRO (K (PLUR BLOCK.N)) (ON.P (THE.D (|SRI | BLOCK.N)))) ?)
; '((DOES.V (SOME.D BLOCK.N) (TOUCH.V (THE.D (|SRI | BLOCK.N)))) ?)
; '((SUB (OF.P (WHAT.D COLOR.N)) ((PRES BE.V) (THE.D (MOST-N LEFT.A BLOCK.N)) *H)) ?)
; '(((HOW_MANY.D (RED.A (PLUR BLOCK.N))) ((PRES BE.V) (ON.P (THE.D TABLE.N)))) ?)
; '(((WHAT.D BLOCK.N) ((PRES BE.V) (BETWEEN.P ((THE.D (|SRI | BLOCK.N)) AND.CC (THE.D (|NVidia| BLOCK.N)))))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((PRES BE.V) (THE.D (|SRI | BLOCK.N)) *H)) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((PRES BE.V) (THE.D (MOST-N LEFT.A (GREEN.A BLOCK.N))) *H)) ?)
; '((WHAT.PRO ((PRES BE.V) (= (THE.D (MOST-N LEFT.A BLOCK.N))))) ?)
;

; Example relations:
; 'None
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.6))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.8))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.8) (((the.d (|Twitter| block.n)) on.p (the.d (|SRI | block.n))) 0.8))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.8) (((the.d (|Twitter| block.n)) on.p (the.d (|SRI | block.n))) 0.8) (((the.d (|Texaco| block.n)) on.p (the.d (|SRI | block.n))) 0.8))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.6) (((the.d (|Twitter| block.n)) on.p (the.d (|SRI | block.n))) 0.8))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.8) (((the.d (|Twitter| block.n)) on.p (the.d (|SRI | block.n))) 0.6))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.6) (((the.d (|Twitter| block.n)) on.p (the.d (|SRI | block.n))) 0.6))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.8) (((the.d (|NVidia| block.n)) near.p (the.d (|Twitter| block.n))) 0.8))
; '((((the.d (|NVidia| block.n)) on.p (the.d (|SRI | block.n))) 0.8) (((the.d (|NVidia| block.n)) on.p (the.d (|Twitter| block.n))) 0.8))
; '((((the.d (|SRI | block.n)) on.p (the.d (|NVidia| block.n))) 0.8) (((the.d (|SRI | block.n)) on.p (the.d (|Texaco| block.n))) 0.8) (((the.d (|SRI | block.n)) near.p (the.d (|Twitter| block.n))) 0.8))
; '((((the.d (|SRI | block.n)) on.p (the.d (|NVidia| block.n))) 0.8) (((the.d (|SRI | block.n)) near.p (the.d (|Texaco| block.n))) 0.8) (((the.d (|SRI | block.n)) between.p ((the.d (|Cat | block.n)) and.cc (the.d (|Dog | block.n)))) 0.8) (((the.d (|SRI | block.n)) between.p ((the.d (|Bob | block.n)) and.cc (the.d (|Ben | block.n)))) 0.8))
;
;; '((SUB (AT.P (WHAT.D PLACE.N)) ((PRES BE.V) (THE.D (|SRI | BLOCK.N)) *H)) ?)
;; '((THE.D (|SRI | BLOCK.N)) ((PRES BE.V)  ((ON.P (THE.D (|NVidia| BLOCK.N))) AND.CC (NEAR.P (THE.D (|Twitter| BLOCK.N))))  ))
;; '((THE.D (|SRI | BLOCK.N)) ((PRES BE.V)  ((ON.P (SET-OF (THE.D (|Mercedes| BLOCK.N)) (THE.D (|NVidia| BLOCK.N)) (THE.D (|Texaco| BLOCK.N)))) AND.CC (NEAR.P (THE.D (|Twitter| BLOCK.N))))  ))
;; '((THE.D (|SRI | BLOCK.N)) ((PRES BE.V)  (ON.P (SET-OF (THE.D (|Mercedes| BLOCK.N)) (THE.D (|NVidia| BLOCK.N)) (THE.D (|Texaco| BLOCK.N))))  ))
;
; Issues with ulf2english:
; converting to english: (THERE.PRO ((PRES BE.V) (3.D (N+PREDS (RED.A (PLUR BLOCK.N)) (THAT.REL ((PRES BE.V) (ON.P (THE.D TABLE.N))))))))
; => (THERE IS 3 RED BLOCKS THAT BE ON THE TABLE |.|)
; converting to english: ((SET-OF (THE.D (|NVidia| BLOCK.N)) (THE.D (|Twitter| BLOCK.N)) (THE.D (|Texaco| BLOCK.N))) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))))
; => (THE NVIDIA BLOCK THE TWITTER BLOCK AND THE TEXACO BLOCK IS ON THE SRI BLOCK |.|)
;
  (let ((query-type (get-query-type query-ulf)))
    (setq query-ulf (car query-ulf))

    (format t "Query: ~a~%" query-ulf)
    ;; (format t "Query type: ~a~%" query-type)
    ;; (format t "Relations: ~a~%" relations)

    (cond
      ; Query is ERROR type
      ((equal query-type 'ERROR)
        '(Sorry \, you was unable to find an object that satisfies given constraints \, please rephrase in a simpler way \.))
      ; Query is CONFIRM type
      ((equal query-type 'CONFIRM)
        (ulf-to-english (respond-confirm query-ulf relations)))
      ; Query is ATTR-COLOR type
      ((equal query-type 'ATTR-COLOR)
        (ulf-to-english (respond-attr-color query-ulf relations)))
      ; Query is COUNT type
      ((equal query-type 'COUNT)
        (ulf-to-english (respond-count query-ulf relations)))
      ; Query is EXIST type
      ((equal query-type 'EXIST)
        (ulf-to-english (respond-exist query-ulf relations)))
      ; Query is IDENT type
      ((equal query-type 'IDENT)
        (ulf-to-english (respond-ident query-ulf relations)))
      ; Query is DESCR type
      ((equal query-type 'DESCR)
        (ulf-to-english (respond-descr query-ulf relations)))
      (t
        '(Sorry \, you was unable to find an object that satisfies given constraints \, please rephrase in a simpler way \.))))
) ; END generate-response


(defun respond-confirm (query-ulf relations)
; `````````````````````````````````````````````
; Creates response ULF for confirmation questions. This is pretty simple, just yes/no/probably answers.
; Should this be modified to list satisfying blocks nonetheless?
;
  (cond
    ((relations-empty? relations) '(no.yn))
    ((> (second (car relations)) *certainty-threshold*) '(yes.yn))
    (t '(probably.adv-s yes.yn)))
) ; END respond-confirm


(defun respond-attr-color (query-ulf relations)
; ```````````````````````````````````````````````
; TODO
;
  (cond
    ; If no relations are returned
    ((relations-empty? relations)
      (ttt:apply-rules `(
        (/ (sub (of.p color-flag?) ((! verb-pres? verb-untensed?) (the.d _!) _!1)) (there.pro ((pres be.v) (= (no.d (such.a _!))))))
        (/ (color-flag? ((! verb-pres? verb-untensed?) (the.d _!))) (there.pro ((pres be.v) (= (no.d (such.a _!))))))
        (/ (color-flag?) (there.pro ((pres be.v) (= (no.d (such.a block.n))))))
      ) query-ulf))
    ;; ; If one or more relations are returned
    ;; (t (let ((certain-set (get-certain-subjs relations)) (uncertain-set (get-uncertain-subjs relations)))
    ;;   (cond
    ;;     ; If only uncertain results are given
    ;;     ((null certain-set)
    ;;       (ttt:apply-rules `(
    ;;         (/ (yn-flag? there.pro _!1 _!2) (perhaps.adv-s (,uncertain-set ((pres might.aux-v) (be.v _!2)))))
    ;;         (/ (yn-flag? there.pro _*) (perhaps.adv-s (,uncertain-set ((pres might.aux-v) be.v))))
    ;;         ) query-ulf))
    ;;     ; If any certain results are given
    ;;     (t
    ;;       (ttt:apply-rules `(
    ;;         (/ (yn-flag? there.pro _!1 _!2) (yes.yn (,certain-set (yn-flag? _!2))))
    ;;         (/ (yn-flag? there.pro _*) (yes.yn (,certain-set (pres be.v))))
    ;;         ) query-ulf)))))
    )
) ; END respond-attr-color


(defun respond-count (query-ulf relations)
; `````````````````````````````````````````````
; Creates response ULF for counting questions. For satisfying relations with more than one block,
; we want to replace the determiner with a number determiner.
; NOTE: this should also be modified to list some of the satisfying blocks, perhaps for answers with
; 5 or less relations.
;
  (cond
    ; If no relations are returned
    ((relations-empty? relations)
      (ttt:apply-rules `(
        (/ ((how_many.d _!) ((! verb-pres? verb-untensed?) _*)) ((no.d _!) (! _*)))
        ) query-ulf))
    ; If a single relation is returned
    ((relations-singular? relations)
      (let ((certain-set (get-certain-subjs relations)) (uncertain-set (get-uncertain-subjs relations)))
        (if certain-set
          ; If result is certain
          (ttt:apply-rules `(
            (/ ((how_many.d _!) ((! verb-pres? verb-untensed?) _*)) (just.adv-s (,certain-set ((pres be.v) _*))))
            ) query-ulf)
          ; If result is uncertain
          (ttt:apply-rules `(
            (/ ((how_many.d _!) ((! verb-pres? verb-untensed?) _*)) (probably.adv-s (just.adv-s (,uncertain-set ((pres be.v) _*)))))
            ) query-ulf))))
    ; If one or more relations are returned
    (t (let ((certain-set (get-certain-subjs relations)) (uncertain-set (get-uncertain-subjs relations)))
      (cond
        ; If only uncertain results are given
        ((null certain-set)
          (ttt:apply-rules `(
            (/ ((how_many.d _!) ((! verb-pres? verb-untensed?) _*))
               (there.pro (probably.adv-s ((pres be.v) (,(length-as-number-det uncertain-set) (n+preds _! (that.rel ((pres be.v) _*))))))))
            ) query-ulf))
        ; If any certain results are given
        (t
          (ttt:apply-rules `(
            (/ ((how_many.d _!) ((! verb-pres? verb-untensed?) _*))
               (there.pro ((pres be.v) (,(length-as-number-det certain-set) (n+preds _! (that.rel ((pres be.v) _*)))))))
            ) query-ulf))))))
) ; END respond-count


(defun respond-exist (query-ulf relations)
; `````````````````````````````````````````````
; Creates response ULF to existence questions.
; NOTE: add "is there a block that is on ..." when fixed
;
  (cond
    ; If no relations are returned
    ((relations-empty? relations)
      (ttt:apply-rules `(
        (/ (yn-flag? there.pro _!1 _!2) (no.yn (there.pro (yn-flag? not.adv-s _!1 _!2))))
        (/ (yn-flag? there.pro _*) (no.yn))
      ) query-ulf))
    ; If one or more relations are returned
    (t (let ((certain-set (get-certain-subjs relations)) (uncertain-set (get-uncertain-subjs relations)))
      (cond
        ; If only uncertain results are given
        ((null certain-set)
          (ttt:apply-rules `(
            (/ (yn-flag? there.pro _!1 _!2) (perhaps.adv-s (,uncertain-set ((pres might.aux-v) (be.v _!2)))))
            (/ (yn-flag? there.pro _*) (perhaps.adv-s (,uncertain-set ((pres might.aux-v) be.v))))
            ) query-ulf))
        ; If any certain results are given
        (t
          (ttt:apply-rules `(
            (/ (yn-flag? there.pro _!1 _!2) (yes.yn (,certain-set (yn-flag? _!2))))
            (/ (yn-flag? there.pro _*) (yes.yn (,certain-set (pres be.v))))
            ) query-ulf))))))
) ; END respond-exist


(defun respond-ident (query-ulf relations)
; `````````````````````````````````````````````
; Creates response ULF to identification questions.
;
  (cond
    ; If no relations are returned
    ((relations-empty? relations)
      (ttt:apply-rules `(
        (/ ((ident-flag? _!) ((! verb-pres? verb-untensed?) _*)) ((no.d _!) (! _*))) ; In case of "what block is ..."
        (/ (ident-flag? ((! verb-pres? verb-untensed?) _*)) (nothing.pro (! _*))) ; In case of "what is ..."
        ) query-ulf))
    ; If a single relation is returned
    ((relations-singular? relations)
      (let ((certain-set (get-certain-subjs relations)) (uncertain-set (get-uncertain-subjs relations)))
        (if certain-set
          ; If result is certain
          (ttt:apply-rules `(
            (/ ((ident-flag? (^* (plur _!))) ((! verb-pres? verb-untensed?) _*)) (only.adv-s (,certain-set (! _*)))) ; If correctional phrasing needed
            (/ ((!1 (ident-flag? _!) ident-flag?) ((! verb-pres? verb-untensed?) _*)) (,certain-set (! _*))) ; Otherwise
            ) query-ulf)
          ; If result is uncertain
          (ttt:apply-rules `(
            (/ ((ident-flag? (^* (plur _!))) ((! verb-pres? verb-untensed?) _*)) (probably.adv-s (only.adv-s (,uncertain-set (! _*))))) ; If correctional phrasing needed
            (/ ((!1 (ident-flag? _!) ident-flag?) ((! verb-pres? verb-untensed?) _*)) (probably.adv-s (,uncertain-set (! _*)))) ; Otherwise
            ) query-ulf))))
    ; If one or more relations are returned
    (t (let ((certain-set (get-certain-subjs relations)) (uncertain-set (get-uncertain-subjs relations)))
      (cond
        ; If only uncertain results are given
        ((null certain-set)
          (ttt:apply-rules `(
            (/ ((!1 (ident-flag? _!) ident-flag?) ((! verb-pres? verb-untensed?) _*)) (possibly.adv-s (,uncertain-set (! _*))))
            ) query-ulf))
        ; If only certain results are given
        ((null uncertain-set)
          (ttt:apply-rules `(
            (/ ((!1 (ident-flag? _!) ident-flag?) ((! verb-pres? verb-untensed?) _*)) (,certain-set (! _*)))
            ) query-ulf))
        ; If a mix of certain and uncertain results are given
        (t
          (ttt:apply-rules `(
            (/ ((!1 (ident-flag? _!) ident-flag?) ((! verb-pres? verb-untensed?) _*))
               ((,certain-set (! _*)) but.cc (it.pro ((pres be.v) (less.mod-a certain.a) (for.p ,uncertain-set)))))
            ) query-ulf))))))
) ; END respond-ident


(defun respond-descr (query-ulf relations)
; `````````````````````````````````````````````
; TODO
;
  (cond
    ; If no relations are returned
    ((relations-empty? relations)
      (ttt:apply-rules `(
        (/ (_!1 (at.p (what.d place.n)) ((! verb-pres? verb-untensed?) _! _*))
          (you.pro ((past can.aux-s) not (figure_out.v (ans-to ((sub where.pq (_! ((pres be.v) *h))) ?))))))
        ) query-ulf))
    ; If one or more relations are returned
    (t (let ((certain-set (get-certain-rels relations)) (uncertain-set (get-uncertain-rels relations)))
      (cond
        ; If only uncertain results are given
        ((null certain-set)
          (ttt:apply-rules `(
            (/ (_!1 (at.p (what.d place.n)) ((! verb-pres? verb-untensed?) _! _*))
              (possibly.adv-s (_! (! ,(conjoin-relations uncertain-set)))))
            ) query-ulf))
        ; If any certain results are given
        (t
          (ttt:apply-rules `(
            (/ (_!1 (at.p (what.d place.n)) ((! verb-pres? verb-untensed?) _! _*))
              (_! (! ,(conjoin-relations certain-set))))
            ) query-ulf))))))
) ; END respond-descr


(defun ulf-to-english (ulf)
; ``````````````````````````
; For converting a ulf response to a surface form response via the ulf2english library.
;
  (format t "converting to english: ~a~%" ulf)
  ;; (format t "~a~%" (ulf2english:ulf2english '(NO.YN
  ;;                       (THERE.PRO
  ;;                        ((PRES BE.V) NOT.ADV-S (A.D (RED.A BLOCK.N))
  ;;                         (ON.P (THE.D (SRI  BLOCK.N))))))))
  (str-to-output (ulf2english:ulf2english ulf))
) ; END ulf-to-english


(defun get-query-type (ulf)
; ```````````````````````````
; Classify a ULF query as one of a few types by checking corresponding TTT flags.
;
  (cond
    ((ttt:match-expr '(^* color-flag?) ulf) 'ATTR-COLOR)
    ((ttt:match-expr '(^* descr-flag?) ulf) 'DESCR)
    ((ttt:match-expr '(^* ident-flag?) ulf) 'IDENT)
    ((ttt:match-expr '(^* count-flag?) ulf) 'COUNT)
    ((ttt:match-expr '(^* exist-flag?) ulf) 'EXIST)
    ((ttt:match-expr '(^* yn-flag?) ulf) 'CONFIRM)
    (t 'ERROR)
  )
) ; END get-query-type


; TTT flags and other preds are defined as follows
; ``````````````````````````````````````````````````
(defun yn-flag? (p)
  (ttt:match-expr '(! yn-word? (tense? yn-word?) ((tense? tense?) yn-word?) ((tense? tense?) (yn-word? _*))
                      ((tense? tense?) (tense? yn-word?)) ((tense? tense?) ((tense? yn-word?) _*))) p))

(defun count-flag? (p)
  (ttt:match-expr '(! (HOW.ADV-A MANY.A) (HOW.MOD-A MANY.A) HOW_MANY.D) p))

(defun ident-flag? (p)
  (ttt:match-expr '(! WHAT.PRO WHICH.PRO WHAT.D WHICH.D) p))

(defun descr-flag? (p)
  (ttt:match-expr '(! ((! WHERE.PRO WHERE.D) (^* (yn-flag? (^* BLOCK.N)))) (AT.P (WHAT.D PLACE.N))) p))

(defun exist-flag? (p)
  (ttt:match-expr '(yn-flag? THERE.PRO _*) p))

(defun color-flag? (p)
  (ttt:match-expr '(WHAT.D (! COLOR.N (PLUR COLOR.N))) p))

(defun tense? (p)
  (member p '(PRES PAST PROG PERF)))

(defun yn-word? (p)
  (member p '(BE.V DO.AUX-S DO.AUX-V CAN.AUX-S CAN.AUX-V DOES.V)))

(defun color-word? (p)
  (member p '(RED.A ORANGE.A YELLOW.A GREEN.A BLUE.A PURPLE.A PINK.A WHITE.A BLACK.A MAGENTA.A GRAY.A GREY.A VIOLET.A INDIGO.A BROWN.A)))


(defun length-as-number-det (set)
; `````````````````````````````````
; Creates ULF determiner corresponding to the size of a set/list.
;
  (intern (concatenate 'string (format nil "~a" (- (length set) 1)) ".D"))
) ; END length-as-number-det


(defun relations-empty? (relations)
; ```````````````````````````````````
; Checks if a list of relations is empty (i.e. an atomic element equal to 'None).
;
  (and (symbolp relations) (equal relations 'None))
) ; END relations-empty?


(defun relations-singular? (relations)
; ````````````````````````````````````
; Checks if a list of relations only has a singular element.
;
  (and (listp relations) (= (length relations) 1))
) ; END relations-singular?


(defun relations-some? (relations)
; `````````````````````````````````
; Checks if a list of relations has a few elements (i.e. 5 or less, but greater than one).
;
  (and (listp relations) (> (length relations) 1) (<= (length relations) 5))
) ; END relations-some?


(defun relations-many? (relations)
; `````````````````````````````````
; Checks if a list of relations has many elements (i.e. more than 5).
;
  (and (listp relations) (> (length relations) 5))
) ; END relations-many?


(defun compare-certainty (relation)
; ```````````````````````````````````
; Compares certainty of relation to the global threshold parameter.
;
  (> (second relation) *certainty-threshold*)
) ; END compare-certainty


(defun filter-relations (relations filter)
; ``````````````````````````````````````````
; Given a list of relations, filter so only the specific relations in filter are kept.
;
  (let ((filtered (remove-if-not (lambda (rel) (member (second (first rel)) filter)) relations)))
    (if filtered filtered 'None))
) ; END filter-relations


(defun get-certain-rels (relations)
; ````````````````````````````````````
; Retrieves relations from a list of relations with certainties above the threshold.
;
  (remove nil (mapcar (lambda (rel)
    (if (compare-certainty rel) rel)) relations))
) ; END get-certain-rels


(defun get-uncertain-rels (relations)
; ````````````````````````````````````
; Retrieves relations from a list of relations with certainties below the threshold.
;
  (remove nil (mapcar (lambda (rel)
    (if (not (compare-certainty rel)) rel)) relations))
) ; END get-uncertain-rels


(defun get-certain-subjs (relations)
; ````````````````````````````````````
; Retrieves subjects from a list of relations with certainties above the threshold.
;
  (let ((subjs (remove-duplicates (remove nil (mapcar (lambda (rel)
          (if (compare-certainty rel) (first (first rel)))) relations)) :test #'equal)))
    (if (<= (length subjs) 1) (car subjs) (cons 'SET-OF subjs)))
) ; END get-certain-subjs


(defun get-uncertain-subjs (relations)
; ````````````````````````````````````
; Retrieves subjects from a list of relations with certainties below the threshold.
;
  (let ((subjs (remove-duplicates (remove nil (mapcar (lambda (rel)
          (if (not (compare-certainty rel)) (first (first rel)))) relations)) :test #'equal)))
    (if (<= (length subjs) 1) (car subjs) (cons 'SET-OF subjs)))
) ; END get-uncertain-subjs


(defun get-certain-objs (relations)
; ````````````````````````````````````
; Retrieves objects from a list of relations with certainties above the threshold.
;
  (let ((subjs (remove-duplicates (remove nil (mapcar (lambda (rel)
          (if (compare-certainty rel) (third (first rel)))) relations)) :test #'equal)))
    (if (<= (length subjs) 1) (car subjs) (cons 'SET-OF subjs)))
) ; END get-certain-subjs


(defun get-uncertain-objs (relations)
; ````````````````````````````````````
; Retrieves objects from a list of relations with certainties below the threshold.
;
  (let ((subjs (remove-duplicates (remove nil (mapcar (lambda (rel)
          (if (not (compare-certainty rel)) (third (first rel)))) relations)) :test #'equal)))
    (if (<= (length subjs) 1) (car subjs) (cons 'SET-OF subjs)))
) ; END get-uncertain-subjs


(defun group-relations (relations)
; `````````````````````````````````
; Groups relations into sublists according to predicate.
;
  (when (equal relations 'None) (return-from group-relations 'None))
  (let ((i -1) preps result)
    (mapcar (lambda (rel)
      (let ((prep (car (member rel preps :test (lambda (r p) (equal (second (first rel)) (first p)))))))
        (cond
          (prep (let* ((n (second prep)) (prev (nth n result)))
            (setq result (replace-n n result (cons rel prev)))))
          (t (setq i (1+ i))
             (setq preps (cons (list (second (first rel)) i) preps))
             (setq result (append result (list (list rel))))))))
      relations)
  result)
) ; END group-relations


(defun condense-by-subjs (relations)
; ```````````````````````````````````
; Given a list of relations (assumed to have the same object and predicate), condense relations into a
; single relation with a (set-of ...) subject.
; NOTE: we make an exception for "between" (or any other predicates with more than one subject). Certainties
; are ignored at this point.
;
  (let ((pred (second (first (first relations)))) (subjs (remove-duplicates (mapcar #'third (mapcar #'first relations)))))
    (if (<= (length subjs) 1) (setq subjs (car subjs)) (setq subjs (cons 'SET-OF subjs)))
    (if (equal pred 'BETWEEN.P) (mapcar #'first relations)
      `(,(first (first (first relations))) ,pred ,subjs)))
) ; END condense-by-subjs


(defun conjoin-relations (relations)
; ```````````````````````````````````
; Given a list of relations, create a ULF conjunction of predicates and subjects.
;
  (when (equal relations 'None) (return-from conjoin-relations 'None))
  (cdr (mapcan (lambda (group)
    (if (and (listp group) (>= (length group) 2) (listp (second group)))
      (cons 'and.cc (cdr (mapcan (lambda (rel) (list 'and.cc (list (second rel) (third rel)))) group)))
      (list 'and.cc (list (second group) (third group)))
    )

    ;; (list 'and.cc (list (second group) (third group)))
    )
    (mapcar #'condense-by-subjs (group-relations relations))))
) ; END conjoin-relations


;; (defun get-certain-colors (relations)
;; ; ````````````````````````````````````
;; ;
;; ;
;;   (let ((subjs (remove-duplicates (remove nil (mapcar (lambda (rel)
;;           (if (compare-certainty rel) (ttt:apply-rule '(/ ()) rel))) relations)) :test #'equal)))
;;     (if (<= (length subjs) 1) (car subjs) (cons 'SET-OF subjs)))
;; ) ; END get-certain-colors


;; (defun get-uncertain-colors (relations)
;; ; ````````````````````````````````````
;; ;
;; ;
;;   (let ((subjs (remove-duplicates (remove nil (mapcar (lambda (rel)
;;           (if (not (compare-certainty rel)) (third (first rel)))) relations)) :test #'equal)))
;;     (if (<= (length subjs) 1) (car subjs) (cons 'SET-OF subjs)))
;; ) ; END get-uncertain-colors