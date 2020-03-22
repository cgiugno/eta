;; March 20/2020
;; ================================================
;;
;; Functions to help Eta work with time, such as comparisons, relation-checking,
;; and maintaining/updating knowledge of time.
;;

; List of supported temporal prepositions (potentially aliased in the case of prepositions
; with multiple senses or ones that are essentially synonyms)
(defvar *temporal-prep-list*
  '(during.p (at.p during.p) (in.p during.p) (on.p during.p) (when.p during.p) (while.p during.p) (within.p during.p)
    before.p (prior_to.p before.p) (preceding.p before.p) (until.p before.p)
    after.p (following.p after.p) (since.p after.p) (from.p after.p)))
(defvar *temporal-adj-list*
  '(first.a (original.a first.a) (initial.a first.a) second.a third.a current.a (now.a current.a)
    recent.a ago.a (previous.a ago.a) (before.a ago.a)
     )
)

  (adv-e recently ever since last adv-e-number)

; Possible temporal nouns of different categories
(defun temporal-unit-noun? (ulf)
  (member ulf '(second.n minute.n hour.n)))
(defun temporal-turn-noun? (ulf)
  (member ulf '(turn.n time.n stage.n iteration.n period.n)))
(defun temporal-question-noun? (ulf)
  (member ulf '(question.n utterance.n)))
(defun temporal-move-noun? (ulf)
  (member ulf '(move.n action.n step.n)))

; Possible temporal mod-a words of different categories
(defun temporal-recent-mod-a? (ulf)
  (member ulf '(recently.mod-a slightly.mod-a)))
(defun temporal-not-mod-a? (ulf)
  (member ulf '(not.mod-a)))
(defun temporal-just-mod-a? (ulf)
  (member ulf '(just.mod-a only.mod-a exactly.mod-a precisely.mod-a right.mod-a directly.mod-a immediately.mod-a)))
(defun temporal-most-mod-a? (ulf)
  (member ulf '(most.mod-a very.mod-a)))

; "Recently" is considered anything within a 2 minute threshold
(defvar *recently-threshold* 120)

; "n hour ago" is +- 20 mins, "n minute ago" is +- 60 secs, "n seconds ago" is +- 10 secs
(defvar *delta-seconds* 10)
(defvar *delta-minutes* 60)
(defvar *delta-hours* 1200)


(defun update-time ()
; ``````````````````````
; Updates time to a "new period", i.e. creates a new constant denoting
; a new time period (and stores before/after relationships in context)
;
  (let ((time-old *time*) time-new pred-before pred-after)
    (setq time-new (intern (format nil "NOW~a"
      (1+ (chars-to-int (cdddr (explode *time*)))))))
    (setq *time* time-new)
    (store-time)
    (setq pred-before (list time-old 'before.p time-new))
    (setq pred-after  (list time-new 'after.p time-old))
    (store-fact pred-before *context* :keys (list (car pred-before)))
    (store-fact pred-after  *context* :keys (list (car pred-after))))
) ; END update-time


(defun get-time ()
; ``````````````````
; Gets the system time and returns a record structure. The format of the
; record is ($ time ?second ?minute ?hour ?day ?month ?year).
; 
  (multiple-value-bind (seconds minutes hours days months years) (get-decoded-time)
    `($ date-time ,years ,months ,days ,hours ,minutes ,seconds))
) ; END get-time


(defun to-universal-time (time-record)
; ``````````````````````````````````````
; Converts time record to universal time.
;
  (apply #'encode-universal-time (reverse (cddr time-record)))
) ; END to-universal-time


(defun store-time ()
; ````````````````````
; Gets and stores the date-time of the current time proposition.
;
  (let ((pred-time (list *time* 'at-about.p (get-time))))
    (store-fact pred-time *context* :keys (list (car pred-time))))
) ; END store-time


(defun get-elapsed-time (time-record)
; ````````````````````````````````````
; Gets the elapsed time between time-record and the current time as a ULF noun phrase (up to hours).
;
  (let ((seconds (- (get-universal-time) (to-universal-time time-record))) minutes hours)
    (if (> seconds 60)
      (setq minutes (floor (/ seconds 60))))
    (if (> seconds (* 60 60))
      (setq hours (floor (/ seconds (* 60 60)))))
    (cond
      (hours (list (num-to-det hours) (if (> hours 1) '(plur hour.n) 'hour.n)))
      (minutes (list (num-to-det minutes) (if (> minutes 1) '(plur minute.n) 'minute.n)))
      (seconds (list (num-to-det seconds) (if (> seconds 1) '(plur second.n) 'second.n)))))
) ; END get-elapsed-time


(defun get-time-of-episode (ep-sym)
; ``````````````````````````````````
; Gets the time record corresponding to an episode symbol.
;
  (third (car (remove-if-not #'at-about-prop? (gethash ep-sym *context*))))
) ; END get-time-of-episode


(defun compare-time (Ti Tj)
; ``````````````````````````
; Returns -1 if Ti is before Tj, 0 if they're the same, 1 if Ti is after Tj
;
  (let ((i (to-universal-time (get-time-of-episode Ti))) (j (to-universal-time (get-time-of-episode Tj))))
    (if (equal Ti Tj) 0
      (if (> i j) 1 -1)))
) ; END compare-time


(defun diff-times (Ti Tj)
; `````````````````````````
; Returns the difference between time Ti and Tj
;
  (let ((i (chars-to-int (cdddr (explode Ti)))) (j (chars-to-int (cdddr (explode Tj)))))
    (abs (- i j)))
) ; END diff-times


(defun get-next-time (Ti)
; `````````````````````````
; Get a constant denoting the subsequent period (related by before.p/after.p propositions in context).
;
  (third (car (remove-if-not #'before-prop? (gethash Ti *context*))))
) ; END get-next-time


(defun get-prev-time (Ti)
; `````````````````````````
; Get a constant denoting the previous period (related by before.p/after.p propositions in context).
;
  (third (car (remove-if-not #'after-prop? (gethash Ti *context*))))
) ; END get-prev-time


;; (defun get-times-final (n)
;; ; ``````````````````````````
;; ; Gets a list of all times, going backward n hops.
;; ;
;;   (get-times-before *time* n)
;; ) ; END get-times-final


;; (defun get-times-init (&optional n)
;; ; ```````````````````````````````````
;; ; Gets the initial time(s), going forward n hops.
;; ; NOTE: should this assume initial time always 'NOW0, or
;; ; use recursive method from *time* to find initial time?
;; ;
;;   (if (null n) (setq n 1))
;;   (cons 'NOW0 (get-times-after 'NOW0 (- n 1)))
;; ) ; END get-times-init


;; (defun get-time-nth (n)
;; ; ``````````````````````
;; ; Gets the nth time since the beginning.
;; ;
;;   (last (get-times-init (+ 1 n)))
;; ) ; END get-time-nth


;; (defun get-time-current ()
;; ; `````````````````````````
;; ; Gets the current time.
;; ; NOTE: for now, this is just the value of the parameter *time*.
;; ;
;;   *time*
;; ) ; END get-time-current


;; (defun get-times-all ()
;; ; ```````````````````````
;; ; Gets a list of all times (excluding the current time, unless the current time is 'NOW0).
;; ;
;;   (let ((times-before (get-times-before *time* -1)))
;;     (if times-before times-before (append times-before (list *time*))))
;; ) ; END get-times-all


(defun latest-time (times &optional n)
; `````````````````````````````````````
; Gets the most recent time (or n most recent times, if n is given).
;
  (if (null n) (setq n 1))
  (subseq (sort (copy-seq times) (lambda (x y) (> (compare-time x y) 0))) 0 n)
) ; END latest-time


(defun earliest-time (times &optional n)
; ```````````````````````````````````````
; Gets the least recent time (or n least recent times, if n is given).
;
  (if (null n) (setq n 1))
  (subseq (sort (copy-seq times) (lambda (x y) (< (compare-time x y) 0))) 0 n)
) ; END earliest-time


(defun time-np-to-num (np)
; ````````````````````````
; Converts an np such as "three turns" to the number 3.
;
  (let ((det (first np)) (noun (second np)) count type)
    (setq count (if (numerical-det? det) (numerical-det! det) 1))
    (setq type (get-head-noun noun))
    count)
) ; END time-np-to-num


(defun time-np-to-seconds (np)
; `````````````````````````````
; Converts an np such as "three minutes" to some interval of seconds within some delta (in this case,
; (210 150), i.e. between 210 and 150 seconds)
;
  (let ((det (first np)) (noun (second np)) count type)
    (setq count (if (numerical-det? det) (numerical-det! det) 1))
    (setq type (get-head-noun noun))
    (cond
      ((equal type 'second.n)
        (list (+ count *delta-seconds*) (- count *delta-seconds*)))
      ((equal type 'minute.n)
        (list (+ (* count 60) *delta-minutes*) (- (* count 60) *delta-minutes*)))
      ((equal type 'hour.n)
        (list (+ (* count 3600) *delta-hours*) (- (* count 3600) *delta-hours*)))))
) ; END time-np-to-seconds


(defun is-apart (time1 time2 interval)
; ```````````````````````````````````````
; Checks whether the time difference between time1 and time2 lies within some interval.
; 
  (let* ((utime1 (to-universal-time (get-time-of-episode time1)))
         (utime2 (to-universal-time (get-time-of-episode time2)))
         (diff (abs (- utime2 utime1))))
    (and (<= diff (first interval)) (>= diff (second interval))))
) ; END is-apart


(defun is-apart-type (time1 time2 type n)
; ````````````````````````````````````````
; Checks whether time1 and time2 are apart by n instances of some prop.
;
  (let ((type-pred (case type (turn #'identity) (question #'ask-prop?) (move #'move-prop?))))
    (labels
      ; Past direction
      ((is-apart-type-prev (time-prev n)
        (cond
          ; If reach end
          ((null time-prev) nil)
          ; If all n hops have been taken, see if landed on time1
          ((<= n 0) (equal time1 time-prev))
          ; Otherwise, take a hop in the past direction
          (t (is-apart-type-prev (get-prev-time time-prev)
            (- n (if (remove-if-not type-pred (gethash time-prev *context*)) 1 0))))))
       ; Future direction
       (is-apart-type-next (time-next n)
        (cond
          ; If reach end
          ((null time-next) nil)
          ; If all n hops have been taken, see if landed on time1
          ((<= n 0) (equal time1 time-next))
          ; Otherwise, take a hop in the future direction
          (t (is-apart-type-next (get-next-time time-next)
            (- n (if (remove-if-not type-pred (gethash time-next *context*)) 1 0)))))))
      (or (is-apart-type-prev time2 n) (is-apart-type-next time2 n))))
) ; END is-apart-type


(defun is-recent (time1 time2)
; `````````````````````````````
; Checks if time1 is recent to time2, i.e. if the difference between
; the two is within the recency threshold.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time1)))
        (utime2 (to-universal-time (get-time-of-episode time2))))
    (< (abs (- utime2 utime1)) *recently-threshold*))
) ; END is-recent


(defun is-before (time1 time2)
; `````````````````````````````
; Checks if time1 is before time2.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time1)))
        (utime2 (to-universal-time (get-time-of-episode time2))))
    (and (<= utime1 utime2) (not (equal time1 time2))))
) ; END is-before


(defun is-prev (time1 time2)
; `````````````````````````````
; Checks if time1 is the previous time to time2.
;
  (equal time1 (get-prev-time time2))
) ; END is-prev


(defun is-after (time1 time2)
; `````````````````````````````
; Checks if time1 is after time2.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time1)))
        (utime2 (to-universal-time (get-time-of-episode time2))))
    (and (>= utime1 utime2) (not (equal time1 time2))))
) ; END is-after


(defun is-next (time1 time2)
; ```````````````````````````
; Checks if time1 is the next time to time2.
;
  (equal time1 (get-next-time time2))
) ; END is-next


(defun time-inclusive (times)
; ````````````````````````````
; Makes list of times inclusive by including previous time, e.g. (NOW1 NOW2 NOW3) => (NOW0 NOW1 NOW3 NOW4)
;
  (let* ((least (least-recent times)) (prev (get-prev-time (car least))))
    (append (if prev (list prev)) times))
) ; END time-inclusive


(defun eval-temporal-relation (rel time1 time2 &optional mod-a)
; ```````````````````````````````````````````````````````````````````
; Returns a boolean value for the relation rel between time1 and time2.
; time2 is a symbol representing a time individual, e.g. NOW0, or a
; set of times, e.g. (set-of NOW0 NOW1). time1 is a time individual.
; If mod-a is given, this is passed to the predicate.
; NOTE: if relation is not found, return true by default.
;
  (let* ((prep-lookup (find-car rel *temporal-prep-list*))
         (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
    (if (fboundp prep)
      (funcall prep time1 time2 mod-a)
      t))
) ; END eval-temporal-relation


(defun during.p (time1 time2 mod-a)
; ```````````````````````````````````
; Evaluates if time1 is during time2. If time2 is an atom, simply check if
; they're the same time, otherwise time1 is an element of the times in time2.
;
  (if (listp time2) (find time1 time2) (equal time1 time2))
) ; END during.p


(defun before.p (time1 time2 mod-a)
; ``````````````````````````````````
; Check if time1 is before time2, applying any mod-a as appropriate.
;
  (if (listp time2) (before.p time1 (earliest-time (cdr time2)) mod-a)
    (cond
      ; recently before
      ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
        (and (is-recent time1 time2) (is-before time1 time2)))
      ; not before
      ((ttt:match-expr 'temporal-not-mod-a? mod-a)
        (not (is-before time1 time2)))
      ; just before
      ((ttt:match-expr 'temporal-just-mod-a? mod-a)
        (is-prev time1 time2))
      ; two seconds/minutes/hours before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart time1 time2 (time-np-to-seconds (cadadr mod-a)))))
      ; two turns before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart-type time1 time2 'turn (time-np-to-num (cadadr mod-a)))))
      ; two questions before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart-type time1 time2 'question (time-np-to-num (cadadr mod-a)))))
      ; two moves before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart-type time1 time2 'move (time-np-to-num (cadadr mod-a)))))
      ; no/unknown mod-a
      (t (is-before time1 time2))
    ))
) ; END before.p


(defun after.p (time1 time2 mod-a)
; ``````````````````````````````````
; Check if time1 is after time2, applying any mod-a as appropriate.
;
  (if (listp time2) (after.p time1 (latest-time (cdr time2)) mod-a)
    (cond
      ; recently after
      ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
        (and (is-recent time1 time2) (is-after time1 time2)))
      ; not after
      ((ttt:match-expr 'temporal-not-mod-a? mod-a)
        (not (is-after time1 time2)))
      ; just after
      ((ttt:match-expr 'temporal-just-mod-a? mod-a)
        (is-next time1 time2))
      ; two seconds/minutes/hours after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart time1 time2 (time-np-to-seconds (cadadr mod-a)))))
      ; two turns after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart-type time1 time2 'turn (time-np-to-num (cadadr mod-a)))))
      ; two questions after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart-type time1 time2 'question (time-np-to-num (cadadr mod-a)))))
      ; two moves after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart-type time1 time2 'move (time-np-to-num (cadadr mod-a)))))
      ; no/unknown mod-a
      (t (is-after time1 time2))
    ))
) ; END after.p