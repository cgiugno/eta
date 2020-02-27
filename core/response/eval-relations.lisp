;; Dec 9/19
;; ================================================
;;
;; Functions to answer spatial questions in a "rough and ready" way
;; NOTE: We currently assume, for convenience, that each block is of unit size,
;; so that the centroid coordinates of two blocks which are touching will differ by 1
;;

(defvar *spatial-prep-list*
  '(touching.p to_the_left_of.p to_the_right_of.p below.p above.p behind.p in_front_of.p on.p near.p next-to.p))
(defvar *blocksize* 1)
(defvar *std-error* (/ *blocksize* 10))



(defun eval-relation (rel coords1 coords2 &optional coords3)
; ````````````````````````````````````````````````````````````
; Evaluate whether relation rel holds between an object with centroid at coords1, and
; an object with centroid at coords2 (plus an additional object at coords3 for between.p)
; NOTE: coords is a list of the form e.g. (|Texaco| 2 3 0)
;
  (if (fboundp rel)
    (let ((v (apply rel (append (cdr coords1) (cdr coords2) (cdr coords3)))))
      (if (numberp v) (>= v *certainty-threshold*) v)))
) ; END eval-relation



(defun ~= (x y)
; ``````````````````````
; Approximately equal to
;
  (let ((eps *std-error*))
    (< (abs (- x y)) eps))
) ; END ~=



(defun same_height (z1 z2)
; `````````````````````````
; Check whether two objects are at the same height
;
  (exp (- (* 0.1 (abs (- z1 z2)))))
) ; END same_height



(defun dist (x1 y1 z1 x2 y2 z2)
; ``````````````````````````````
; Euclidean distance
;
  (let ((d1 (- x1 x2))
        (d2 (- y1 y2))
        (d3 (- z1 z2)))
      (sqrt (+ (+ (* d1 d1) (* d2 d2)) (* d3 d3))))
) ; END dist



(defun scaled_dist (x1 y1 z1 x2 y2 z2 size1 size2)
; ``````````````````````````````````````````````````
; Distance scaled by the sizes of the two objects
; 
  (/ (dist x1 y1 z1 x2 y2 z2) (max (+ size1 0.001) (+ size2 0.001)))
) ; END scaled_dist



(defun touching.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (or
      (and (~= 0 diffx)           (>  *blocksize* diffy) (~= *blocksize* diffz))
      (and (>  *blocksize* diffx) (~= 0 diffy)           (~= *blocksize* diffz))
      (and (~= 0 diffx)           (~= *blocksize* diffy) (>  *blocksize* diffz))
      (and (>  *blocksize* diffx) (~= *blocksize* diffy) (~= 0 diffz))
      (and (~= *blocksize* diffx) (~= 0 diffy)           (>  *blocksize* diffz))
      (and (~= *blocksize* diffx) (>  *blocksize* diffy) (~= 0 diffz))))
) ; END touching.p



(defun to_the_left_of.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (<= diffx (* -1 *blocksize*)))
) ; END to_the_left_of.p



(defun to_the_right_of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (>= diffx *blocksize*))
) ; END to_the_right_of.p



(defun below.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (<= diffz (* -1 *blocksize*)))
) ; END below.p



(defun above.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (>= diffz *blocksize*))
) ; END above.p



(defun behind.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (>= diffy *blocksize*))
) ; END behind.p



(defun in_front_of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (<= diffy (* -1 *blocksize*)))
) ; END in_front_of.p



(defun on.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  (and (above.p x1 y1 z1 x2 y2 z2) (touching.p x1 y1 z1 x2 y2 z2))
) ; END on.p



;; (defun near.p (x1 y1 z1 x2 y2 z2 size1 size2 context)
(defun near.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````````````
  ;; (exp (- (* 0.1 (scaled_dist x1 y1 z1 x2 y2 z2 size1 size2))))
  (exp (- (* 0.1 (scaled_dist x1 y1 z1 x2 y2 z2 *blocksize* *blocksize*))))
) ; END near.p



;; (defun next-to.p (x1 y1 z1 x2 y2 z2 size1 size2)
(defun next-to.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````````
  ;; (+ (* 0.5 (near.p x1 y1 z1 x2 y2 z2 size1 size2)) (* 0.5 (same_height z1 z2)))
  (+ (* 0.5 (near.p x1 y1 z1 x2 y2 z2)) (* 0.5 (same_height z1 z2)))
) ; END next-to.p