;; Dec 9/19
;; ================================================
;;
;; Functions to answer spatial questions in a "rough and ready" way
;; NOTE: We currently assume, for convenience, that each block is of unit size,
;; so that the centroid coordinates of two blocks which are touching will differ by 1
;;

; List of supported spatial prepositions (potentially aliased in the case of prepositions
; with multiple senses or ones that are essentially synonyms)
; TODO: on_to in facing abutting between visible flush_with towards)
(defvar *spatial-prep-list*
  '(touching.p to_the_left_of.p to_the_right_of.p below.p (under.p below.p) (beneath.p below.p) above.p behind.p
    in_front_of.p on.p (on_top_of.p on.p) near.p (near-to.p near.p) (close-to.p near.p) next_to.p (adjacent_to.p next-to.p)
    between.p))
    
(defvar *blocksize* 1)
(defvar *std-error* (/ *blocksize* 10))


(defun ~= (x y)
; ``````````````````````
; Approximately equal to
;
  (let ((eps *std-error*))
    (< (abs (- x y)) eps))
) ; END ~=


(defun ~equal (l1 l2)
; `````````````````````
; List approximately equal to
;
  (cond
    ((and (numberp l1) (numberp l2))
      (~= l1 l2))
    ((and (symbolp l1) (symbolp l2))
      (equal l1 l2))
    ((and (listp l1) (listp l2))
      (let* ((padded (pad l1 l2)) (l1p (first padded)) (l2p (second padded)))
        (every (lambda (x y) (~equal x y)) l1p l2p))))
) ; END ~equal


(defun eval-spatial-relation-bool (rel coords1 coords2 &optional coords3)
; `````````````````````````````````````````````````````````````````````````
; Evaluate whether relation rel holds between an object with centroid at coords1, and
; an object with centroid at coords2 (plus an additional object at coords3 for between.p)
; Returns boolean value.
; NOTE: coords is a list of the form e.g. (|Texaco| 2 3 0)
;
  (let* ((prep-lookup (find-car rel *spatial-prep-list*))
         (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
    (if (fboundp prep)
      (let ((v (apply prep (append (cdr coords1) (cdr coords2) (cdr coords3)))))
        (if (numberp v) (>= v *certainty-threshold*) v))))
) ; END eval-spatial-relation-bool


(defun eval-spatial-relation (rel coords1 coords2 &optional coords3)
; ```````````````````````````````````````````````````````````````````
; Returns a certainty value for the relation rel between coords1 and coords2 (and
; coords3 in the case of between).
; NOTE: coords is a list of the form e.g. (|Texaco| 2 3 0)
;
  (let* ((prep-lookup (find-car rel *spatial-prep-list*))
         (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
    (if (fboundp prep)
      (let ((v (apply prep (append (cdr coords1) (cdr coords2) (cdr coords3)))))
        (if (numberp v) v (if v t nil)))))
) ; END eval-spatial-relation


(defun sigmoid (x &key (a 0.1))
; ```````````````````````````````
; Sigmoid function with coefficient a
;
  (/ 1 (+ 1 (exp (- (* a x)))))
) ; END sigmoid


(defun exp-decay (x &key (a 0.1))
; `````````````````````````````````
; Exponential decay with a coefficient a
;
  (exp (- (* a x)))
) ; END exp-decay


(defun same-height (z1 z2)
; `````````````````````````
; Check whether two objects are at the same height
;
  (exp-decay (abs (- z1 z2)))
) ; END same-height


(defun dist (x1 y1 z1 x2 y2 z2)
; ``````````````````````````````
; Euclidean distance
;
  (let ((d1 (- x1 x2))
        (d2 (- y1 y2))
        (d3 (- z1 z2)))
      (sqrt (+ (+ (* d1 d1) (* d2 d2)) (* d3 d3))))
) ; END dist


(defun scaled-dist (x1 y1 z1 x2 y2 z2 size1 size2)
; ``````````````````````````````````````````````````
; Distance scaled by the sizes of the two objects
; 
  (/ (dist x1 y1 z1 x2 y2 z2) (max (+ size1 0.001) (+ size2 0.001)))
) ; END scaled-dist


(defun touching.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (if (or
      (and (~= 0 diffx)           (>  *blocksize* diffy) (~= *blocksize* diffz))
      (and (>  *blocksize* diffx) (~= 0 diffy)           (~= *blocksize* diffz))
      (and (~= 0 diffx)           (~= *blocksize* diffy) (>  *blocksize* diffz))
      (and (>  *blocksize* diffx) (~= *blocksize* diffy) (~= 0 diffz))
      (and (~= *blocksize* diffx) (~= 0 diffy)           (>  *blocksize* diffz))
      (and (~= *blocksize* diffx) (>  *blocksize* diffy) (~= 0 diffz)))
    1.0 0.0))
) ; END touching.p


(defun to_the_left_of.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (if (<= diffx (* -1 *blocksize*))
      (exp-decay (dist x1 y1 z1 x2 y2 z2)) 0))
) ; END to_the_left_of.p


(defun to_the_right_of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (if (>= diffx *blocksize*)
      (exp-decay (dist x1 y1 z1 x2 y2 z2)) 0))
) ; END to_the_right_of.p


(defun below.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (if (<= diffz (* -1 *blocksize*))
      (exp-decay (dist x1 y1 z1 x2 y2 z2)) 0))
) ; END below.p


(defun above.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (if (>= diffz *blocksize*)
      (exp-decay (dist x1 y1 z1 x2 y2 z2)) 0))
) ; END above.p


(defun behind.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (if (>= diffy *blocksize*)
      (exp-decay (dist x1 y1 z1 x2 y2 z2)) 0))
) ; END behind.p


(defun in_front_of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (if (<= diffy (* -1 *blocksize*))
      (exp-decay (dist x1 y1 z1 x2 y2 z2)) 0))
) ; END in_front_of.p


(defun on.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  ;; (and (above.p x1 y1 z1 x2 y2 z2) (touching.p x1 y1 z1 x2 y2 z2))
  (+ (* 0.5 (above.p x1 y1 z1 x2 y2 z2)) (* 0.5 (touching.p x1 y1 z1 x2 y2 z2)))
) ; END on.p


;; (defun near.p (x1 y1 z1 x2 y2 z2 size1 size2 context)
(defun near.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````````````
  ;; (exp (- (* 0.1 (scaled-dist x1 y1 z1 x2 y2 z2 size1 size2))))
  (exp-decay (scaled-dist x1 y1 z1 x2 y2 z2 *blocksize* *blocksize*))
) ; END near.p


;; (defun next_to.p (x1 y1 z1 x2 y2 z2 size1 size2)
(defun next_to.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````````
  ;; (+ (* 0.5 (near.p x1 y1 z1 x2 y2 z2 size1 size2)) (* 0.5 (same-height z1 z2)))
  (+ (* 0.5 (near.p x1 y1 z1 x2 y2 z2)) (* 0.5 (same-height z1 z2)))
) ; END next_to.p


(defun between.p (x1 y1 z1 x2 y2 z2 x3 y3 z3)
; ````````````````````````````````````````````````
  (max
    (+ (* 0.5 (to_the_left_of.p x1 y1 z1 x2 y2 z2)) (* 0.5 (to_the_right_of.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (to_the_right_of.p x1 y1 z1 x2 y2 z2)) (* 0.5 (to_the_left_of.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (in_front_of.p x1 y1 z1 x2 y2 z2)) (* 0.5 (behind.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (behind.p x1 y1 z1 x2 y2 z2)) (* 0.5 (in_front_of.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (above.p x1 y1 z1 x2 y2 z2)) (* 0.5 (below.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (below.p x1 y1 z1 x2 y2 z2)) (* 0.5 (above.p x1 y1 z1 x3 y3 z3))))
) ; END between.p