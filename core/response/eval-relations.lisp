;; Dec 9/19
;; ================================================
;;
;; Functions to answer spatial questions in a "rough and ready" way
;; NOTE: We currently assume, for convenience, that each block is of unit size,
;; so that the centroid coordinates of two blocks which are touching will differ by 1
;;


(defun eval-relation (rel coords1 coords2)
; ``````````````````````````````````````````
; Evaluate whether relation rel holds between an object with centroid at coords1, and
; an object with centroid at coords2.
; NOTE: coords is a list of the form e.g. (|Texaco| 2 3 0)
;
  (apply rel (append (cdr coords1) (cdr coords2)))
) ; END eval-relation



(defun ~= (x y)
; ``````````````````````
; Approximately equal to
;
  (let ((eps 0.1))
    (< (abs (- x y)) eps))
) ; END ~=



(defun touching.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (or
      (and (~= 0 diffx) (>  1 diffy) (~= 1 diffz))
      (and (>  1 diffx) (~= 0 diffy) (~= 1 diffz))
      (and (~= 0 diffx) (~= 1 diffy) (>  1 diffz))
      (and (>  1 diffx) (~= 1 diffy) (~= 0 diffz))
      (and (~= 1 diffx) (~= 0 diffy) (>  1 diffz))
      (and (~= 1 diffx) (>  1 diffy) (~= 0 diffz))))
) ; END touching.p



(defun to-the-left-of.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (<= diffx -1))
) ; END to-the-left-of.p



(defun to-the-right-of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (>= diffx 1))
) ; END to-the-right-of.p



(defun below.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (<= diffz -1))
) ; END below.p



(defun above.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (>= diffz 1))
) ; END above.p



(defun behind.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (<= diffy -1))
) ; END behind.p



(defun in-front-of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (>= diffy 1))
) ; END in-front-of.p



(defun on.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  (and (above.p x1 y1 z1 x2 y2 z2) (touching.p x1 y1 z1 x2 y2 z2))
) ; END on.p