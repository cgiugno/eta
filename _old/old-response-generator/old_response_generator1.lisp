; This contains a function for transforming output from the
; constraint solver into readable english.  The format of the

; question should be the (corrected) ULF of the user's question

; answer set should be:
; [<(arg0.0 type0.0 ...) rel (arg1.0 type1.0 ...) cert>
;  <(arg0.1 type0.1 ...) rel (arg1.1 type1.1 ...) cert> ... ]

; constraints should be a number with the following properties:
;   3 Leftmost Bits should represent the question type
;   then
;

(defconstant ERROR_RESPONSE 0)
(defconstant CONFIRM_QUESTION 1)
(defconstant EXIST_QUESTION 2)
(defconstant COLOR_QUESTION 3)
(defconstant COUNT_QUESTION 4)
(defconstant IDENT_QUESTION 5)
(defconstant DESCR_QUESTION 6)

(defun arg0 (ans_set i) (car (nth i ans_set)))
(defun arg1 (ans_set i) (nth 2 (nth i ans_set)))

(defun append_pos (sym pos) (intern (concatenate 'string (symbol-name sym) pos)))

(print (append_pos 'red ".n"))
(print (intern (concatenate 'string (symbol-name 'YAASS) ".yn")))

(defun arg_to_text (arg)
  (cond ((= (list-length arg) 2)
         (list 'the.d (car arg) (nth 1 arg)))
        ((= (list-length arg) 4)
         (list 'the.d (car arg) (nth 1 arg) 'and 'the.d (nth 2 arg) (nth 3 arg)))
        (t ())))


(print (arg_to_text '(Toyota block Nvidia block)))

(defun generate_response (question ans_set constraints)
    (let ((question_type  (logand constraints 7))
          (arg0_question (ash (logand constraints 8) -3))
          (arg1_question (ash (logand constraints 16) -4))
          (var2 (logand constraints 32))
          (var3 (logand constraints 64))
          (surf_type (logand constraints 16))
          (subj_adjs (logand constraints 16))
          (surf_plur (logand constraints 16)))
        ;(print question_type)
        ;(print arg0_question)
        ;(print arg1_question)
        ;(print (arg1 ans_set 0))
        ;(print (eql (arg1 ans_set 0) '()))
        (cond ((= question_type CONFIRM_QUESTION)
               (cond ((and (= arg0_question 1) (eql (arg0 ans_set 0) '())
                       'No.yn))
                     ((and (= arg0_question 1) (not (eql (arg0 ans_set 0) '()))
                       'Yes.yn))
                     ((and (= arg1_question 1) (eql (arg1 ans_set 0) '())
                       'No.yn))
                     ((and (= arg1_question 1) (not (eql (arg1 ans_set 0) '()))
                       'Yes.yn))))
              ((= question_type EXIST_QUESTION)
               (cond ((and (= arg0_question 1) (= arg1_question 1)) ; both slots filled
                      '(Mwaahahaha))
                     ((= arg0_question 1) ; only first slot filled
                      (cond ((eql (arg0 ans_set 0) '())
                             '(No))
                            (t
                             (list 'Yes.yn\, (arg_to_text (arg0 ans_set 0))))))
                     ((= arg1_question 1) ; only second slot filled
                      '(Mwhaha))))
              ((= question_type COLOR_QUESTION))
              ((= question_type COUNT_QUESTION))
              ((= question_type IDENT_QUESTION))
              ((= question_type DESCR_QUESTION))
              ((= question_type ERROR_RESPONSE))
              (t (print "Question type not defined")))))

; TESTs ---------------------------------------------------------------------------------------------------------------------

; CONFIRMATION Qs ---------------------------------------------------------------------

; Is the Target block slightly to the left of some red block? ... Yes
(print '(Is the Target block slightly to the left of some red block?))
(print (generate_response '(((pres be.v) (the.d (|Target| block.n)) (slightly.adv-a (to_the_left_of.p (some.d (red.a block.n))))) ?)
            '((('Target 'block) 'to_the_left_of ('Starbucks 'block) 0.9)) 17)) ; 0001 0001

; Is the Target block slightly to the left of some red block? ... No
(print '(Is the Target block slightly to the left of some red block?))
(print (generate_response '(((pres be.v) (the.d (|Target| block.n)) (slightly.adv-a (to_the_left_of.p (some.d (red.a block.n))))) ?)
            '((('Target 'block) 'to_the_left_of () 1)) 17)) ; 0001 0001

; EXIST Qs ------------------------------------------------------------------------------

; Are there two blocks facing each other? ... Yes, the Target block is facing the Nvidia block
(print '(Are there two blocks facing each other?))
(print (generate_response '(((pres be.v) there.pro (two.d (n+preds (plur block.n) (face.v each_other.pro)))) ?)
            '(((Target block) facing (Nvidia block) 1)) 26)) ; 0001 1010

; Are there two blocks facing each other? ... No
(print '(Are there two blocks facing each other?))
(print (generate_response '(((pres be.v) there.pro (two.d (n+preds (plur block.n) (face.v each_other.pro)))) ?)
            '((() facing () 1)) 26)) ; 0001 1010

; Is the SRI block fully on top of any red block? ... No
(print '(Is the SRI block fully on top of any red block?))
(print (generate_response '(((pres be.v) (the.d (|SRI| block.n)) (fully.adv-a (on_top_of.p (any.d (red.a block.n))))) ?)
            '(((SRI block) on_top_of () 1)) 18)) ; 0001 0010

; Is the SRI block fully on top of any red block? ... Yes, it is /(they are) on top of the Toyota block
(print '(Is the SRI block fully on top of any red block?))
(print (generate_response '(((pres be.v) (the.d (|SRI| block.n)) (fully.adv-a (on_top_of.p (any.d (red.a block.n))))) ?)
            '(((SRI block) on_top_of () 1)) 18)) ; 0001 0010

; Is there a block touching the Nvidia block? ... Yes, the McDonalds block
(print '(Is there a block touching the Nvidia block?))
(print (generate_response '(((pres be.v) there.pro (a.d (n+preds block.n (touch.v (the.d (|Nvidia| block.n)))))) ?)
            '(((McDonalds block) touching (Nvidia block) 1)) 10)) ; 0000 1010

; Is there a block touching the Nvidia block? ... No
(print '(Is there a block touching the Nvidia block?))
(print (generate_response '(((pres be.v) there.pro (a.d (n+preds block.n (touch.v (the.d (|Nvidia| block.n)))))) ?)
            '((() touching (Nvidia block) 1)) 10) ; 0000 1010

; How many red blocks are between two blue blocks?
  ; count arg0s
  '(((McDonalds block) touching (Nvidia block) 1)) 10)

; How many blocks touch other blocks?


;



(defun to-english-list (ulf)
; ```````````````````````````
; Converts the ulfument of a relation, e.g. (set-of (|McDonalds| block.n) (|Starbucks| block.n))
; to an english output.
; NOTE: function is messy currently, refactor/improve
;
  (cond
    ; ulf is missing
    ((null ulf) '(nothing))
    ; ulf is a set of blocks
    ((and (listp ulf) (equal (car ulf) 'SET-OF))
      ; Check if the elements of the set are homogeneous in type
      (if (every (lambda (b) (and (listp b) (noun? (second b)) (equal (second b) (second (first (cdr ulf)))))) (cdr ulf))
        ; If they are, we just want to list all the names and specify the type at the end with a -s attached
        (let ((names (mapcar #'first (cdr ulf))))
          (append (cons 'the (append (apply #'append (mapcar
                                                      (lambda (x) (if (> (length (cdr ulf)) 2) `(,x \,) `(,x)))
                                                      (butlast names)))
                                     `(and ,(car (last names)))))
                  (list (intern (concatenate 'string (string (remove-type (second (first (cdr ulf))))) "S")))))
        ; Otherwise, list each name/type pair seperately
        (cons 'the (append (apply #'append (mapcar
                                            (lambda (x) (if (> (length (cdr ulf)) 2) (append (cdr (to-english-list x)) '(\,)) (cdr (to-english-list x))))
                                            (butlast (cdr ulf))))
                           (cons 'and (cdr (to-english-list (car (last (cdr ulf))))))))))
    ; ulf is a single block
    ((and (listp ulf) (noun? (second ulf)))
      `(the ,(first ulf) ,(remove-type (second ulf))))
    ; ulf is the table
    ((equal ulf 'TABLE.N) '(the table))
    ; ulf is something else
    (t `(the ,ulf)))
) ; END to-english-list


