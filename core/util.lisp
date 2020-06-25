;; July 10/19 
;; ================================================
;;
;; Various utility functions used by Eta
;;


;``````````````````````````````````````````````````````
; Store a global hash table of schema headers to
; the corresponding schema variable names.
; NOTE: This is only defined in the eta-schema.lisp file.
;
(defparameter *schemas* (make-hash-table))



(defun cddr1 (x)
;`````````````````
; For DEBUGGING
;
  (cddr x)
) ; END cddr1



(defun copy1 (list)
;````````````````````
; Creates top-level copy of list
;
  (cond
    ((atom list) list)
    (t (cons (car list) (copy1 (cdr list)))))
) ; END copy1



(defun conc1 (l1 l2)
;````````````````````
; Graft l2 onto top-level copy of l1
; Avoids unnecessary CONSing (unlike CONC)
;
  (nconc (copy1 l1) l2)
) ; END conc1



(defun pad (l1 l2)
;``````````````````
; Pads either l1 or l2 to make them both the same length
;
  (let ((len1 (length l1)) (len2 (length l2)))
    (cond
      ((= len1 len2) (list l1 l2))
      ((> len1 len2) (pad l1 (append l2 '(nil))))
      ((< len1 len2) (pad (append l1 '(nil)) l2))))
) ; END pad



(defun shuffle (l)
;``````````````````
; Shuffles top-level elements of a list
;
  (cond
    ((or (null l) (atom l)) l)
    ((= 1 (length l)) l)
    ((= 2 (length l))
      (if (= (random 2) 1)
        (list (first l) (second l))
        (list (second l) (first l))))
    (t (let* ((mid (floor (length l) 2))
              (l1 (butlast l mid)) (l2 (last l mid)))
      (if (= (random 2) 1)
        (append (shuffle l1) (shuffle l2))
        (append (shuffle l2) (shuffle l1))))))
) ; END shuffle



(defun flatten (lst)
;````````````````````````
; Flattens an arbitrary list using mapcar. Note that we can do this fairly easily
; using recursion: use mapcar to flatten each sublist (or if an atom is reached, create a
; list consisting of that atom). Then just append all of the flattened sublists together.
;
  (if (not (listp lst))
    (return-from flatten nil))

  ; Recursively flatten list
  (labels
    ((flatten-recur (p)
      (if (atom p)
        (list p)
        (apply #'append (mapcar #'flatten-recur p)))))
    (flatten-recur lst))
) ; END flatten



(defun intersection1 (l)
;`````````````````````````
; Intersection of all sub-lists in l
;
  (cond
    ((null l) nil)
    ((= 1 (length l)) (car l))
    (t (intersection (car l) (intersection1 (cdr l)) :test #'equal)))
) ; END intersection



(defun union1 (l)
;``````````````````
; Union of all sub-lists in l
;
  (cond
    ((null l) nil)
    ((= 1 (length l)) (car l))
    (t (union (car l) (union1 (cdr l)) :test #'equal)))
) ; END union1



(defun replace-n (n l e)
; ```````````````````````
; Replaces the nth element of list l with e
;
  (if (< n (length l))
    (append (butlast l (- (length l) n)) (list e) (last l (- (length l) n 1)))
    l)
) ; END replace-n



(defun chars-to-int (chars)
; ``````````````````````````````
; Converts list of chars to integer
;
  (read-from-string (coerce chars 'string))
) ; END chars-to-int



(defun explode (s)
;``````````````````
; The list of the characters making up symbol s
;
  (coerce (string (if (numberp s) (write-to-string s) s)) 'list)
) ; END explode



(defun implode (l)
;``````````````````
; (IMPLODE LIST-OF-CHARACTERS) => a symbol
; The symbol is interned in the current package and has as name a string 
; with the characters in LIST-OF-CHARACTERS.
;
  (intern  (coerce l 'string))
) ; END implode



(defun decompress (input)
;``````````````````````````
; Replaces contractions (e.g. 'don't' or 'dont' by 'do not')
;
  (cond
    ((null input) nil)
    ((and
        (symbolp (car input))
        (get (car input) 'twowords))
      (conc1 (get (car input) 'twowords) (decompress (cdr input))))
    (t (cons (car input) (decompress (cdr input)))))
) ; END decompress



(defun compress (input)
;``````````````````````````
; Replaces auxiliary-NOT combinations by -N'T contractions
;
  (cond
    ((null input) nil)
    ((null (cdr input)) input)
    (t (cond
      ((or
          (not (eq (cadr input) 'not))
          (null (get (car input) 'neg)))
        (cons (car input) (compress (cdr input))))
      (t (cons (get (car input) 'neg) (compress (cddr input)))))))
) ; END compress



(defun find-car (x list)
;`````````````````````````
; Finds a sublist of list which has x as its car (or matches x if atom)
;
  (find x list :test (lambda (y l)
    (or (and (atom l) (equal y l)) (and (listp l) (equal y (car l))))))
) ; END find-car



(defun find-cars-var (x list)
;``````````````````````````````
; Finds a list of sublists of list which have x as its car,
; where x may be either a specific symbol, or a variable (possibly
; with a restrictor).
; NOTE: Currently unused
;
  (if (and x (every #'listp list)) (cond
    ((nnp? x) (remove-if-not (lambda (l) (equal x (car l))) list))
    ((variable? x) list)
    ((restricted-variable? x)
      (let ((r (cadr x))) (remove-if-not
        (lambda (l) (equal (funcall (first r) (car l)) (second r))) list)))
    (t list)))
) ; END find-car-var



(defun find-cars-list (x list)
;```````````````````````````````
; Finds a list of sublists of list which have x as its car (if x is an atom),
; or have some member of x as its car (if x is a list).
;
  (if (and x (every #'listp list)) (cond
    ((atom x) (remove-if-not (lambda (l) (equal (car l) x)) list))
    (t (remove-if-not (lambda (l) (member (car l) x :test #'equal)) list))))
) ; END find-cars-list



(defun front (x &optional (n 1))
;````````````````````````````````
; Does the opposite of last.
;
  (reverse (last (reverse x) n))
) ; END front



(defun symbol? (x)
;``````````````````
; Returns t if x is a symbol, nil otherwise.
;
  (symbolp x)
) ; END symbol?



(defun fbound? (x)
;`````````````````````
; Returns t if x is a bound function, nil otherwise.
;
  (and (symbolp x) (fboundp x))
) ; END fbound?



(defun not-fbound? (x)
;`````````````````````````
; Returns t if x is not a function, nil otherwise.
;
  (not (fbound? x))
) ; END not-fbound?



(defun restricted-variable? (atm)
;``````````````````````````````````
; Check whether a symbol is a variable, or a variable with a restriction following it.
;
  (or (variable? atm) (and (listp atm) (variable? (car atm))))
) ; END restricted-variable?



(defun variable? (atm)
;`````````````````````
; Check whether a symbol is a variable, i.e. starts with '?', '!', or '$'.
; NOTE: this excludes indexical variables, such as '~you'.
;
  (and (symbolp atm) (member (car (explode atm)) '(#\? #\! #\$) :test #'char-equal))
) ; END variable?



(defun lambda-descr? (lst)
;```````````````````````````
; Check whether a list is a lambda abstract, e.g.,
; (:l (?x ?y) (and (?x on.p ?y) (not (?x red.a))))
;
  (and (listp lst) (= 3 (length lst)) (equal :l (first lst))
       (listp (second lst)) (every #'variable? (second lst))
       (listp (third lst)))
) ; END lambda-descr?



(defun record-structure? (lst)
;```````````````````````````````
; Checks whether a list is a record structure, e.g.,
; ($ loc :x 1 :y 2 :z 0)
  (and (listp lst) (cddr lst) (equal '$ (car lst))
       (symbolp (second lst)))
) ; END record-structure?



(defun get-variables (lst)
;``````````````````````````
; Returns a list of all unique variables in lst.
;
  (remove-duplicates (remove nil
    (mapcar #'(lambda (x) (if (variable? x) x nil)) 
      (flatten lst))))
) ; END get-variables



(defun answer-list? (list)
;``````````````````````````
; Check whether a list is a list of propositional answers with associated certainties.
; TODO: needs cleaning
;
  (and (listp list) (every (lambda (l)
      (and (listp l) (= 2 (length l)) (numberp (second l))
          (or (np? (first l)) (symbolp (first l))
              (and (listp (first l)) (symbolp (car (first l))))
              (and (listp (first l)) (np? (car (first l)))))))
    list))
) ; END answer-list?



(defun prop-var? (atm)
;`````````````````````
; Check whether a symbol is a proposition variable, i.e. ends with .
;
  (and (variable? atm) (char-equal #\. (car (last (explode atm)))))
) ; END prop-var?



(defun ep-var? (atm)
;`````````````````````
; Check whether a symbol is an episode variable, i.e. does not end with .
;
  (and (variable? atm) (not (prop-var? atm)))
) ; END ep-var?



(defun function? (atm)
;``````````````````````
; Check whether a symbol is a function, i.e. ends with '.f'
;
  (and (symbolp atm) (equal '(#\. #\F) (last (explode atm) 2)))
) ; END function?



(defun ground-wff? (wff)
;`````````````````````````
; Check whether a wff is a ground wff, i.e. it has no variables
;
  (cond
    ((atom wff) (not (variable? wff)))
    (t (every #'ground-wff? wff)))
) ; END ground-wff?



(defun quoted-list? (list)
;````````````````````````````````````
; Is list of form (quote (...))?
;
  (if (and
        (listp list)
        (eq (car list) 'quote)
        (listp (second list))) t nil)
) ; END quoted-list?



(defun quoted-sentence? (list)
;````````````````````````````````````````````````
; Is list of form (quote (word1 word2 ... wordn)) ?
;
  (if (and
        (quoted-list? list)
        (every #'symbolp (second list))) t nil)
) ; END quoted-sentence?



(defun quoted-sentence-list? (list)
;````````````````````````````````````````````````
; Is list of the form (quote ((word1 ... wordn) ... (word1 ... wordm))) ?
;
  (if (and
        (quoted-list? list)
        (every #'listp (second list))
        (every (lambda (s) (every #'symbolp s)) (second list))) t nil)
) ; END quoted-sentence-list?



(defun question? (sentence)
;```````````````````````````````````````
; Is sentence of form (quote (<word> ... <word> ?)), or with the
; question mark attached to the last word? (One could make more
; elaborate checks that would also work w/o a question mark, using
; patterns (... <aux> you{r} ...), (... <wh-word> <word> you{r} ...),
; etc.). But we assume we have ensured that output questions end in
; "?".
;
  (let (word)
    ;; (format t "~% ****** quoted-question? first line = ~a **** ~%" (listp sentence))
    ;; (format t "~% ****** quoted-question? third line = ~a **** ~%" sentence) ; DEBUGGING
    (if (and (listp sentence) (every #'atom sentence))
      (setq word (car (last sentence)))
      (return-from quoted-question? nil))
    (or
      (eq word '?)
      (char-equal #\? (car (last (explode word))))))
) ; END question?



(defun quoted-question? (sentence)
;```````````````````````````````````````
; Is sentence of form (quote (<word> ... <word> ?)), or with the
; question mark attached to the last word? (One could make more
; elaborate checks that would also work w/o a question mark, using
; patterns (... <aux> you{r} ...), (... <wh-word> <word> you{r} ...),
; etc.). But we assume we have ensured that output questions end in
; "?".
;
  (let (word)
    ;; (format t "~% ****** quoted-question? first line = ~a **** ~%" (listp sentence))
    ;; (format t "~% ****** quoted-question? third line = ~a **** ~%" sentence) ; DEBUGGING
    (if (quoted-sentence? sentence)
      (setq word (car (last (second sentence))))
      (return-from quoted-question? nil))
    (or
      (eq word '?)
      (char-equal #\? (car (last (explode word))))))
) ; END quoted-question?



(defun ttt-non-initial-var? (x)
;```````````````````````````````
; Is x a TTT match variable starting with '_'?
;
  (let (chars)
    (cond
      ((not (symbolp x)) nil)
      (t (setq chars (explode x))
        (and
          (char-equal (car chars) #\_)
          (find (second chars)
            '(#\! #\? #\+ #\*) :test 'char-equal))))
)) ; END ttt-non-initial-var? 



(defun ttt-initial-var? (x)
;````````````````````````````
; Is x a TTT match variable starting with on of {! ? + * ^}
; or with <> or {}?
;
  (let (chars)
    (cond
      ((not (symbolp x)) nil)
      (t (setq chars (explode x))
        (or
          (find (car chars)
            '(#\! #\? #\+ #\* #\^) :test 'char-equal)
          (and
            (char-equal (car chars) #\<)
            (char-equal (second chars) #\>))
          (and
            (char-equal (car chars) #\{)
            (char-equal (second chars) #\})))))
)) ; END ttt-initial-var?



(defun ttt-var? (x)
;```````````````````
; Is x a TTT match variable?
;
  (or (ttt-non-initial-var? x) (ttt-initial-var? x))
) ; END ttt-var?



(defun ensure-bound! (x)
; ```````````````````````
; Ensures that x isn't a TTT pred (i.e. any symbol with ? as the last character).
;
  (if (and (symbolp x) (char-equal (car (last (explode x))) #\?)) nil x)
) ; END ensure-bound!



(defun ttt-match-vars (patt)
;````````````````````````````
; Form a list of distinct TTT match-variables that occur in 'patt';
; Duplicate variables that occur earlier in a left-to-right scan are
; discarded.
;
  (let (var vars)
    (cond
      ; Base case - if patt is a symbol, return the pattern if it is a
      ; non-initial var, or nil otherwise
      ((symbolp patt)
        (if (ttt-var? patt) `(,patt) nil))
      ; Recursive case
      (t
        (remove-duplicates
          (remove nil (mapcan #'ttt-match-vars patt))
          :test #'equal))))
) ; END ttt-match-vars



(defun bindings-from-ttt-match (patt expr)
;```````````````````````````````````````````
; From the TTT pattern 'patt', create a rule that generates the
; binding list for the match variables of 'expr', when matched
; to that expression. Apply the rule to 'expr', hence obtain
; a list of bindings. A non-sticky match is assumed.
;
(let ((vars (ttt-match-vars patt)) vals)
  (if (null vars) (return-from bindings-from-ttt-match nil))
  (setq vals (ttt:apply-rule `(/ ,patt ,(mapcar #'list vars)) expr :shallow t))
  ; For rules that don't match a given expr, 'ttt:apply-rule' 
  ; returns a result 'eq' to the expr.  Since that's a failure 
  ; case, return nil for it:
  (if (eq vals expr) (return-from bindings-from-ttt-match nil))
  ; Otherwise return the variables matched with their values:
  (mapcar #'list vars vals)
)) ; END bindings-from-ttt-match



(defun get-single-binding (bindings)
;````````````````````````````````````
; Retrieves a single bound symbol from the first match variable.
;
  (car (second (car bindings)))
) ; END get-first-single-binding



(defun get-multiple-bindings (bindings)
;```````````````````````````````````````
; Retrieves multiple bound symbols from the first match variable.
;
  (second (car bindings))
) ; END get-first-multiple-bindings



(defun tagword (word)
;```````````````````````
; Returns a list headed by WORD and followed by its features
; (tags). These features are obtained from the FEATS
; property of the word, from the FEATS property of the 
; features (usually further words) thus obtained, etc.

  ; Check if non-empty
  (if (not (get word 'feats)) (return-from tagword (list word)))

  (let (words feats feat)
    (setq feats (get word 'feats))
    ;; (format t "features = ~a~%" feats) ; DEBUGGING
    (setq words (list word))
    ;; (format t "words = ~a~%" words) ; DEBUGGING

    ; Current word/feature
    (loop (setq feat (pop feats))
      ;; (format t "--------------------------------------------------~%")
	    ;; (format t "feat = ~a~%" feat)
	    ;; (format t "feats = ~a ~%" feats)
	    ;; (format t "membership = ~a ~%" (member feat feats)) ; DEBUGGING
	    (if (not (find feat words)) (push feat words))
      ;; (format t "words = ~a ~%" words) ; DEBUGGING
      (setq feats (add-new-feats feat feats words))
      ;; (format t "updated feature vector = ~a~%" feats) ; DEBUGGING

      ; Avoid a feature cycle
      (if (member feat feats) (setq feats (remove feat feats)))
      (if (null feats) (return-from tagword (reverse words))))
)) ; END tagword



(defun add-new-feats (feat old-feats-list words)
;`````````````````````````````````````````````````
; Add new features to words list
;
  (let ((new-feats-list old-feats-list) feat-feature-list)
    (setq feat-feature-list (get feat 'feats))
    (loop for x in feat-feature-list do
      (if (not (member x words)) 
	      (setq new-feats-list (append (list x) new-feats-list))))
  new-feats-list)
) ; END add-new-feats



(defun attachfeat (list)
;`````````````````````````
; Add the first element of LIST to the FEATS list ; of each 
; remaining element; allow for symbolic atoms as features only
;
  (mapc (lambda (x)
    (if (not (find (car list) (get x 'feats)))
      (setf (get x 'feats) (cons (car list) (get x 'feats)))))
  (cdr list))
) ; END attachfeat



(defun store-output-semantics (var wff schema-name)
;````````````````````````````````````````````````````
; Stores a wff as the semantic interpretation of an episode in
; a schema - this is stored in the schema's semantics hash table,
; with the key being the propositional var for the episode.
;
  (setf (gethash var (get schema-name 'semantics)) wff)
) ; END store-output-semantics



(defun store-output-gist-clauses (var clauses schema-name)
;````````````````````````````````````````````````````
; Stores a gist clause corresponding to an episode in
; a schema - this is stored in the schema's gist-clauses hash table,
; with the key being the propositional var for the episode.
;
  (setf (gethash var (get schema-name 'gist-clauses)) clauses)
) ; END store-output-gist-clauses



(defun store-topic-keys (var keys schema-name)
;````````````````````````````````````````````````````
; Stores a topic key corresponding to an episode in
; a schema - this is stored in the schema's topic-keys hash table,
; with the key being the propositional var for the episode.
;
  (setf (gethash var (get schema-name 'topic-keys)) keys)
) ; END store-topic-keys



(defun store-schema-name (header schema-name)
;````````````````````````````````````````````````````
; Stores the schema variable name in the *schemas* hash table,
; using the schema header as the key.
;
  (setf (gethash header *schemas*) schema-name)
) ; END store-schema-name



(defun schema-header? (x)
;``````````````````````````
; Predicate which returns true if x is a schema header, e.g. 'discuss-food.v'.
;
  (gethash x *schemas*)
) ; END schema-header?



(defun schema-name! (header)
;`````````````````````````````
; Gets the schema variable name given the header.
;
  (gethash header *schemas*)
) ; END schema-name!



(defun store-gist (gist keys kb)
;`````````````````````````````````
; Put 'gist' into the 'kb' (a hash table) using the given keys.
; Avoid duplication of already stored gists. This is intended 
; primarily for acquired gists (as gist clauses) about the user,
; but should also be usable for gists about Eta, that Eta
; could consult in answering questions from the user.
;
  (let ((gists (gethash keys kb)))
    (if (not (member gist gists :test #'equal))
      (setf (gethash keys kb) (cons gist gists)))
)) ; END store-gist



(defun detach-final-punctuation (wordlist)
;```````````````````````````````````````````
; Ensures that the punctuation at the end of the word list is
; a separate token, and not attached to the final word
; 
  (let* ((lastword (car (last wordlist))) (chars (explode lastword))
        ch punc)
    (if (null wordlist) (return-from detach-final-punctuation nil))
    (if (= (length chars) 1)
      (return-from detach-final-punctuation wordlist))
    (cond
      ((setq ch (find (car (last chars)) '(#\. #\? #\! #\;)))
        (setq lastword (implode (butlast chars)))
        (setq punc (implode (list ch)))
	      (append (butlast wordlist) (list lastword) (list punc)))
      (t wordlist))
)) ; END detach-final-punctuation



(defun nil-gist-clause? (gist-clause)
;`````````````````````````````````````
; Return t if gist-clause is the nil gist clause (i.e. '(NIL Gist ...))
;
  (and (>= (length gist-clause) 2) (equal (subseq gist-clause 0 2) '(NIL GIST)))
) ; END nil-gist-clause?



(defun nil-gist-question? (gist-clause)
;`````````````````````````````````````
; Return t if gist-clause is the nil gist question (i.e. '(NIL Question ...))
;
  (and (>= (length gist-clause) 2) (equal (subseq gist-clause 0 2) '(NIL QUESTION)))
) ; END nil-gist-question"



(defun purify-func (user-gist-clauses)
;````````````````````````````````````````
; Remove user gist clauses identical with '(NIL Gist ...) or '(NIL Question ...), unless it is the only
; gist clause (note: prefer nil question to nil gist).
; Also remove duplicate gist clauses
;
  (remove-duplicates
  (let ((purified-gist-clauses (remove-if (lambda (x) (or (nil-gist-clause? x) (nil-gist-question? x))) user-gist-clauses)))
    (if purified-gist-clauses purified-gist-clauses
      (let ((purified-gist-questions (remove-if (lambda (y) (not (nil-gist-question? y))) user-gist-clauses)))
        (if purified-gist-questions (list (car purified-gist-questions)) (list (car user-gist-clauses))))))
  :test #'equal)
) ; END purify-func



(defun gist-contradiction (current-gist-list gist-clause)
;`````````````````````````````````````````````````````````
; Finds gist clauses which contradict one another
;
  (let (cont-flag)
    (loop for ix from 1 to (list-length current-gist-list) do
      (if (equal (car (or
                  (set-difference (nth (- ix 1) current-gist-list)
                    gist-clause)
                  (set-difference gist-clause
                    (nth (- ix 1) current-gist-list))))
          'NOT)
        (setf cont-flag t)))
  cont-flag)
) ; END gist-contradiction



(defun remove-contradiction (gist-list)
;`````````````````````````````````````````````````````````
; Remove any contradicting gist clauses (get rid of the
; latter one, which appears later in the conversation)
;
  (cond
    ((<= (list-length gist-list) 1) gist-list)
    ((gist-contradiction (butlast gist-list) (car (last gist-list)))
      (butlast gist-list))
    (t (append
      (remove-contradiction (butlast gist-list))
      (last gist-list))))
) ; END remove-contradiction



(defun store-turn (agent text &key gists ulfs)
;``````````````````````````````````````````````
; Stores a turn (consisting of some text, and lists of gist clauses and semantic interpretations, if any)
; in the discourse history, along with the agent taking the turn.
; NOTE: currently *discourse-history* is just a list. If it's changed to a hash table in the future,
; this will need to be modified.
;
  (let ((turn (list agent (list text gists ulfs))))
    (setq *discourse-history* (cons turn *discourse-history*)))
) ; END store-turn



(defun print-gist-kb (&key filename)
;`````````````````````````````````````
; Prints all gist clauses in the gist-kb for a user.
; If a file is specified, append to that file.
;
  (if filename
    (with-open-file (outfile filename :direction :output :if-exists :supersede :if-does-not-exist :create)))

  (maphash (lambda (key val)
    (if (not (nil-gist-clause? (car val)))
      (if filename
        (with-open-file (outfile filename :direction :output :if-exists
                                          :append :if-does-not-exist :create)
          (format outfile "~% ~a   ~a" key val))
        (format t "~% ~a   ~a" key val))))
  *gist-kb-user*)
) ; END print-gist-kb



(defun print-history ()
;```````````````````````
; Pretty-prints the discourse history in order.
;
  (let ((i 1))
    (mapcar (lambda (turn)
      (let ((agent (first turn)) (text (first (second turn)))
            (gists (second (second turn))) (ulfs (third (second turn))))
        (format t "~a. ~a : ~a~%" i agent text)
        (mapcar (lambda (gist)
          (format t "   gist: ~a~%" (if gist gist "None"))) gists)
        (mapcar (lambda (ulf)
          (format t "   ulf: ~a~%" (if ulf ulf "None"))) ulfs))
      (setq i (1+ i)))
    (reverse *discourse-history*)))
) ; END print-history



(defun print-hash (ht)
; `````````````````````
; Print the contents of a hash table
;
  (maphash (lambda (k v) (format t "k: ~a~%  v: ~a~%" k v)) ht)
) ; END print-hash



(defun print-context ()
;```````````````````````
; Prints the context (dividing between full propositions, and ones that
; are stored under some specific key)
;
  (let (l1 l2)
    (maphash (lambda (k v)
      (if (equal v t) (setq l1 (cons k l1))
        (setq l2 (cons (list k v) l2)))) *context*)
    (mapcar (lambda (f)
      (format t "~a~%" f)) l1)
    (format t "~%")
    (mapcar (lambda (f)
      (format t "~a: ~a~%~%" (first f) (second f))) l2))
) ; END print-context



(defun store-in-context (wff)
;```````````````````````````````
; Stores a wff in context. Fact may be quoted, e.g., '(E2 finished.a), in
; which case the unquoted predicate is stored (although this format is unused
; in the current schema syntax). Otherwise, evaluate all functions in fact and
; store in context.
;
  (let ((fact (if (equal (car wff) 'quote) (eval wff) wff)))
    (store-fact fact *context*))
) ; END store-in-context



(defun get-from-context (pred-patt)
;````````````````````````````````````
; Retrieves a fact from context
;
  (get-matching-facts pred-patt *context*)
) ; END get-from-context



(defun remove-from-context (pred-patt)
;```````````````````````````````````````
; Removes a fact from context
;
  (remove-facts (get-matching-facts pred-patt *context*)
    *context*)
) ; END remove-from-context



(defun find-all-instances-context (descr)
;```````````````````````````````````````````
; Given a lambda description, find all instances
; from context (see 'find-all-instances').
  (find-all-instances descr *context*)
) ; END find-all-instances-context



(defun add-alias (alias canonical-name)
;````````````````````````````````````````
; Adds a given alias for a canonical name to the equality sets hash table,
; i.e., indexing on the canonical name.
; For example, (add-alias '(k BW-arch.n) '|BW-concept-3|) creates the equality
; set (|BW-concept-3| (k BW-arch.n)) hashed on |BW-concept-3|, or else appends
; (k BW-arch.n) to the existing set under that index.
;
  (if (member alias (gethash canonical-name *equality-sets*) :test #'equal)
    (return-from add-alias nil))
  (when (not (gethash canonical-name *equality-sets*))
    (push canonical-name (gethash canonical-name *equality-sets*)))
  (push alias (gethash canonical-name *equality-sets*))
) ; END add-alias



(defun get-aliases (canonical-name)
;````````````````````````````````````
; Gets a list of aliases for a particular canonical name.
;
  (gethash canonical-name *equality-sets*)
) ; END get-aliases



(defun remove-alias (alias canonical-name)
;```````````````````````````````````````````
; Removes a given alias for a canonical name.
;
  (when (gethash canonical-name *equality-sets*)
    (setf (gethash canonical-name *equality-sets*) 
      (remove alias (gethash canonical-name *equality-sets*) :test #'equal)))
) ; END remove-alias



(defun print-aliases ()
;````````````````````````
; Prints all aliases in equality sets hash table.
;
  (maphash (lambda (canonical-name aliases)
      (format t "~a: ~a~%" canonical-name aliases))
    *equality-sets*)
) ; END print-aliases



(defun get-record-structure (canonical-name)
;``````````````````````````````````````````````
; Gets the record structure aliased to a canonical name,
; if one exists.
;
  (find-if #'record-structure? (get-aliases canonical-name))
) ; END get-record-structure



(defun storage-keys (fact)
;``````````````````````````
; Find the list of keys for hash-table storage of 'fact': the fact as a whole, 
; the predicate, and the predicate with one argument at a time, if there
; is more than one (while other arguments are set to nil).
; 
; We also allow facts that are atoms or of form (<atom>); in the 1st case
; the atom is the only key on the output list, and in the second both 
; <atom> and (<atom>) are on the output list; (we always want to be able
; to retrieve via the fact as a whole, and via the predicate)..
;
  (let (keys key n)
    (cond ((atom fact) (list fact))
          ((null (cdr fact)) (list (car fact) fact)); e.g., (pred (pred))
          (t (setq keys (list fact (second fact))); entire fact, & pred
            (when (cddr fact); more than one argument?
              (setq n (length (cddr fact))); no. of non-subject args
              ; push key for subject arg first
              (setq key (list (second fact) (first fact))); in reverse
              (dotimes (i n) (push nil key))
              (setq key (reverse key))
              (push key keys)
              ; push keys for all other args
              (dotimes (i n)
                (setq key (list (second fact) nil)); in reverse
                (dotimes (j n)
                  (if (= i j) (push (nth i (cddr fact)) key)
                              (push nil key)))
                (setq key (reverse key))
                (push key keys)))
            keys))
)) ; END storage-keys



(defun store-fact (fact ht)
;````````````````````````````
; Store the 'fact' in hash table 'ht' (that uses 'equal' as test) using as keys: 
; - the entire fact, 
; - the predicate (unless the fact is atomic or a 1-element list 
;   (i.e., pred with 0 arguments)
; - if the pred has >= 2 arguments, patterns of form  (pred arg1 nil nil ...), 
;   (pred nil arg2 nil nil ...), (pred nil nil arg3 nil nil ...), etc.
; Return T if the fact was new, and NIL otherwise.
;
  (let ()
    (setq fact (eval-functions fact))
    (if (gethash fact ht) (return-from store-fact nil))
    (dolist (key (storage-keys fact))
      (if (equal key fact)
        (setf (gethash key ht) t); just store t in the case where key is the whole fact
        (push fact (gethash key ht))))
    T
)) ; END store-fact



(defun store-facts (facts ht)
;`````````````````````````````
  (dolist (fact facts) (store-fact fact ht))
) ; END store-facts



(defun remove-fact (fact ht)
;````````````````````````````
; Delete 'fact' from hash table 'ht', under all its keys
;
  (setq fact (eval-functions fact))
  (if (gethash fact ht); is 'fact' actually in ht? 
    (prog2 (dolist (key (storage-keys fact))
            (if (equal key fact)
              (remhash key ht)
              (setf (gethash key ht) 
                     (remove fact (gethash key ht) :test #'equal))))
            t) ; signal that the fact was removed
     nil ; fact wasn't present in ht
)) ; END remove-fact



(defun remove-facts (facts ht)
;```````````````````````````````
  (dolist (fact facts) (remove-fact fact ht))
) ; END remove-facts



(defun get-matching-facts (pred-patt ht)
;``````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred-patt: e.g., (B between.p ?x C); any vars in pred-patt are
;    assumed to be distinct;
; Note that we need to use retrieval key (B between nil nil) or
;    (nil between nil C), and filter the results, because ground
;    wffs are stored under the entire wff as key, under the pred,
;    and under the pred in combination with one argument at a time.
;
; Retrieve the list of facts matching pred-patt from hash table ht.
; This is dependent on the restricted set of keys that are used in 
; storing facts: If there is a variable in pred-patt (as in the
; example), we need to make the variable and all but one non-var
; argument "don't-cares" (nil) in the retrieval. The we filter out
; instances that don't have the required non-variable arguments.
;
; NOTE: here we use the equality sets, so that any match to a constant
; or alias of that constant is successful.
;
  (setq pred-patt (eval-functions pred-patt))
  (if (atom pred-patt) ; special (unexpected) cases of arg-less preds
    (if (symbolp pred-patt) ; facts are stored as list elements
      (return-from get-matching-facts (gethash pred-patt ht))
      ; not a symbol
      (return-from get-matching-facts nil)))
  (if (null (cdr pred-patt)); of form (p)
    (if (symbolp (car pred-patt)) ; use p as key
      (return-from get-matching-facts (gethash (car pred-patt) ht))
      ; of form (p) where p is not a symbol
      (return-from get-matching-facts nil)))
    
  ; at this point we have something of form (x0 p x1 ... xk) where x1 ... xk
  ; are optional and some of the xi may be variables (the expected case).
  (let (arglist pred (nvars 0) nconst key const facts select-facts)
    (setq arglist (cons (car pred-patt) (cddr pred-patt)))
    (setq pred (second pred-patt))
    (dolist (arg arglist)
      (if (variable? arg) (incf nvars)))
    (setq nconst (- (length arglist) nvars))
    ; 4 cases: no vars; no consts; vars & 1 const; vars & > 1 consts
    (cond
      ; no vars
      ((zerop nvars) (gethash (cons (car arglist) (cons pred (cdr arglist))) ht))
      ; no facts
      ((zerop nconst) (reverse (gethash pred ht)))
      ; vars & 1 const
      ((= nconst 1) 
        (setq key (mapcar #'(lambda (x) (if (variable? x) nil x)) pred-patt))
        (reverse (gethash key ht)))
      ; vars & > 1 consts
      (t 
        ; for the key, set all var's and all but one const. to nil
        ; Pick a constant to retain in the key:
        (setq const (find-if #'(lambda (x) (not (variable? x))) 
                      arglist))
        (setq key 
          (let ((arglist-const
                  (mapcar #'(lambda (x) (if (not (eq x const)) nil x)) arglist)))
            (cons (car arglist-const) (cons pred (cdr arglist-const)))))
        (setq facts (gethash key ht)); (in reverse store-order)
        ; filter out facts whose constant args don't match those
        ; of pred-patt:
        (dolist (fact facts)
          (if (not (member nil
                (mapcar #'(lambda (x y) (or (variable? x) (equal x y)))
                  arglist (cons (car fact) (cddr fact)))))
            (push fact select-facts))); this causes correct order
        select-facts))
)) ; END get-matching-facts



(defun find-all-instances (descr curr-state-ht)
;``````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; 'descr' is a lambda abstract, using ':l' for lambda.
;    The body of 'descr' consists of conjoined +ve or -ve literals, one
;    of which must be +ve & contain all variables, including the lambda 
;    variable(s).
;    e.g., (:l (?x ?y) (and (?x between.p ?z ?y) (?x red.a) (?y blue.a)));
;    e.g., (:l (?x) (?x on.p ?y)); everything that's on something
;    NB: A non-equality constraint like "?y is not the table" would
;        have to be expressed as something like (not (?y table.n)),
;        where we've added (is-table table) to the set of facts.
; 'curr-state-ht' is a hash table for positive facts comprising the 
;    current state, indexed under the fact as a whole, the predicate
;    alone, and the predicate in combination with one argument (for
;    each argument, if there's more than one).
; We find all instances of the :l-variable(s)? as follows: 
; (simplest possible method) Start with all the instances of the
; positive predication (in the lambda-description) that contains 
; all the variables, and successively restrict the tuples with the
; remaining conjuncts. Use predication templates, in addition to the
; current set of instances, as arguments in 'constrain-relation', 
; so that argument correspondences will be clear, w/o any indexing.
; In the end, project the resulting list of predicate instances onto
; the :l-variable "dimensions"
;
  (let (body lambda-vars vars main-conjunct facts neglist poslist ind inds)
    (setq lambda-vars (second descr))
    (setq body 
      (if (not (eq (car (third descr)) 'and)); just one predication?
        (list (third descr)); list it, for uniformity
        (cdr (third descr)))); drop the "and"
    (setq body (eval-functions body))
    (setq vars (get-variables
      (mapcar #'(lambda (x) (if (eq (car x) 'not) (second x) x))
        body)))
    ; find positive conjunct containing all variables
    (dolist (conjunct body)
      (when (subsetp vars
              (get-variables (cons (car conjunct) (cddr conjunct))))
            (setq main-conjunct conjunct)
            (return nil))) ; exit loop
    (when (null main-conjunct)
          (format t "~%*** Description ~s ~
                     ~%      given to 'find-all-instances' didn't contain ~
                     ~%      a predication that covers all variables" descr)
          (return-from find-all-instances nil))

    ; Retrieve the facts in curr-state-ht matching main-conjunct
    (setq facts (get-matching-facts main-conjunct curr-state-ht))
    ; Now apply the remaining conjuncts as constraints on facts;
    ; first place negative facts after positive ones (also omitting 
    ; main-conjunct):
    (dolist (conjunct body)
        (if (eq (car conjunct) 'not) 
            (push conjunct neglist)
            (if (not (equal conjunct main-conjunct))
                (push conjunct poslist))))
    (setq body (append (reverse poslist) (reverse neglist)))
    (dolist (conjunct body)
        (setq facts 
          (constrain-relation main-conjunct conjunct facts curr-state-ht)))

    ; Now project 'facts" onto the dimensions picked out by the :l-variables
    ; 'Project-relation' uses indices for argument positions to be picked
    ; out by the projection, so we compute these first by assigning
    ; index properties to the variables (the properties will be global, 
    ; but harmless):
    (setq ind 1)
    (when (cdr main-conjunct)
      (setf (get (car main-conjunct) 'index) ind)
      (incf ind))
    (dolist (arg (cddr main-conjunct))
        (incf ind) (if (variable? arg) (setf (get arg 'index) ind)))
    (setq inds (mapcar #'(lambda (x) (get x 'index)) lambda-vars))
    ; Return the projecton of facts onto the positions corr. to the
    ; Lambda variables:
    (project-relation facts inds)
)) ; END find-all-instances



(defun constrain-relation (pred1-patt {~}pred2-patt rel1 kb-ht)
;````````````````````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred1-patt: of form (arg1 pred1 ... argn); in general, argi may
;     be a variable.
; {~}pred2-patt: of form (arg'1 pred2 ... arg'm) or 
;     (not (arg'1 pred2 ... arg'm)), where m =< n and all variables 
;     of pred2-patt occur in pred1-patt; {~}pred2-patt also allows
;     for equalities or negated equalities, using one of {=,eq,equal};
; rel1: a list of relation instances, a subset of pred1-patt instances;
;
; If {-}pred2-patt is unnegated, the result is a restricted subset
; of the rel1-instances, where only those are retained and returned 
; whose arguments, when used to instantiate {~}pred2-patt, lead to 
; existing hash-table entries.
;
; If {-}pred2-patt is negated, the procedure is similar, except that
; only rel1-instances are retained where the pred2 hash-table lookup
; does *not* lead to existing entries.
;
  (let (positive key rel2)
    ; deal with equality constraints first
    (if (and (listp {~}pred2-patt) (member (second {~}pred2-patt) '(= eq equal)))
        (return-from constrain-relation
            (constrain-relation-by-equality pred1-patt {~}pred2-patt rel1)))
    ; now inequality constraints
    (if (and (listp {~}pred2-patt) (eq (car {~}pred2-patt) 'not)
              (listp (second {~}pred2-patt)) 
              (member (second (second {~}pred2-patt)) '(= eq equal)))
        (return-from constrain-relation 
            (constrain-relation-by-inequality pred1-patt {~}pred2-patt rel1)))
    ; other constraints (=> require consulting '{~}pred2-patt'-facts in kb-ht)
    (setq positive (not (and (listp {~}pred2-patt) 
                              (eq (car {~}pred2-patt) 'not))))
    (dolist (item rel1)
      ; set variables occurring in pred1-patt to corresponding
      ; elements of the item predication:
      (mapcar #'(lambda (x y) (if (variable? x) (set x y)))
              (cons (car pred1-patt) (cddr pred1-patt))
              (cons (car item) (cddr item)))
      ; construct a retrieval key for pred2 accordingly:
      (setq key (mapcar #'(lambda (x) (if (variable? x) (eval x) x))
                    (if positive {~}pred2-patt (second {~}pred2-patt))))
      (if (gethash key kb-ht)
          (if positive (push item rel2))
          (if (not positive) (push item rel2))))
    (reverse rel2)
    ; It will come out preserving the ordering in rel1.
)) ; END constrain-relation



(defun constrain-relation-by-equality (pred-patt equality rel)
;```````````````````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred-patt: provides variable argument positions for the predications in rel;
; equality: e.g., (?y = B3), i.e., we fix the value of a variable in pred-patt
;           and hence of corresponding arguments in the rel predications;
;           the reverse form, e.g., (B3 = ?y), is also handled;
; rel: a set of ground predications (of form  pred-patt), to be filtered with
;           the equality; NB: no error check is done to ensure that pred-patt
;           and rel use the same predicate. Also no check is made on the form
;           of equality. The calling program might use 'eq' or 'equal', instead
;           of '=', but the initial symbol in the given equality is ignored.
;    
  (let (var val (i -1) result)
    ; find the position of the variable in the equality in pred-patt, and
    ; then go through the elements of rel, retaining for output just those
    ; that have the constant specified by the equality at the position
    ; determined from the pred-patt:
    (setq var (first equality); e.g., (?y = B3) ... the expected form
          val (third equality))
    (if (not (variable? var))
        (setq var (third equality); e.g., (B3 = ?y)
              val (first equality)))
    (cond ((atom pred-patt) rel); unexpected: pred-patt should be (pred ...)
          (t (dolist (x pred-patt)
              (incf i) (if (equal x var) (return nil))); exit loop (i is set)
              (dolist (r rel)
                (if (equal (nth i r) val) (push r result)))
              result))
)) ; END constrain-relation-by-equality



(defun constrain-relation-by-inequality (pred-patt inequality rel)
;```````````````````````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred-patt: provides variable argument positions for the predications in rel;
; inequality: e.g., (not (?y = B3)), i.e., we preclude the value of a variable 
;           in pred-patt and hence of corresponding arguments in the rel pred-
;           ications; the reverse form, e.g., (not (B3 = ?y)), is also handled;
; rel: a set of ground predications (of form  pred-patt), to be filtered with
;           the inequality; NB: no error check is done to ensure that pred-patt
;           and rel use the same predicate. Also no check is made on the form
;           of inequality.
;    
  (let (var val (i -1) result)
    ; find the position of the variable in the equality in pred-patt, and
    ; then go through the elements of rel, retaining for output just those
    ; that have the constant specified by the equality at the position
    ; determined from the pred-patt:
    (setq var (first (second inequality)); e.g., (not (?y = B3)) ... expected 
          val (third (second inequality)))
    (if (not (variable? var))
        (setq var (third (second inequality)); e.g., (not (B3 = ?y))
              val (first (second inequality))))
    (cond ((atom pred-patt) rel); unexpected: pred-patt should be (pred ...)
          (t (dolist (x pred-patt)
              (incf i) (if (equal x var) (return nil))); exit loop (i is set)
              (dolist (r rel)
                (if (not (equal (nth i r) val)) (push r result)))
              result))
)) ; END constrain-relation-by-inequality



(defun project-relation (rel indices)
;``````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; 'rel' is assumed to be a set of k-tuples, with k  >= 1. (If it is 1,
; the list 'rel' is returned unchanged.) 'Indices' is the list of argument
; indices indicating what "dimensions" of rel should be retained (projecting
; other dimensions onto them), and in what order they should be returned.
; The result is a set k-tuples, where k =|indices|, except that if k=1,
; the 1-tuples are reduced to individual atoms, not singleton lists.
;
; Method: Run through the 'rel' tuples, and for each tuple, pull out the
; elements corresponding to the 'indices', and if this "subtuple" has not yet
; been encountered before (as registered in an ad-hoc hash table), push it
; onto the result list and register it in the ad-hoc hash table.
; Return the reverse of the final result list.
; 
  (let (ht key result)
    ; Is 'rel' unary, i.e., a list of atoms or single-element lists?
    (if (or (null rel) (atom (car rel)) (null (cdar rel)))
        (return-from project-relation rel))
    (setq ht (make-hash-table :test #'equal))
    (dolist (tuple rel)
        (setq key nil)
        (dolist (i indices)
            (push (nth (- i 1) tuple) key))
        (setq key (reverse key))
        (when (null (gethash key ht))
            (setf (gethash key ht) T) 
            (push key result)))
    (if (null (cdr indices)); if just one index, "flatten" the result
        (setq result (apply #'append result)))
    (reverse result)
)) ; END project-relation 



(defun update-prop (prop prop-list)
;```````````````````````````````````
; Given a proposition and a list of propositions, remove propositions in list with the same
; predicate as the given proposition, and add the new one to the list.
;
  (subst prop (car prop) prop-list
    :test (lambda (x y) (and (listp y) (>= (length y) 2) (equal (second prop) (second y)) (equal x (first y)))))
) ; END update-prop



(defun split-sentences (words)
;```````````````````````````````
; Given the list of words 'words', split into multiple lists of words for each sentence,
; delimited by punctuation.
;
  (let (result cur)
    ; Loop through each word, keeping a buffer which is emptied once punctuation is reached
    (mapcar (lambda (word)
        (cond
          ((member word '(|.| ? !))
            (setq result (cons (reverse (cons word cur)) result))
            (setq cur nil))
          (t
            (setq cur (cons word cur)))))
      words)
    ; Empty buffer (in case the last sentence is missing punctuation)
    (when cur
      (setq result (cons (reverse cur) result)))
  (reverse result))
) ; END split-sentences



(defun form-chunks (words) ; NOTE: currently unused
;```````````````````````````
; Given the list of words 'words', form 10-word chunks overlapping by
; 5 words; if there are just 15 words or less, form a single chunk,
; i.e., a singleton list containing the list of words. For non-initial
; chunks whose first word is preceded in 'words' by a negative word,  
; add that word to the beginning of the chunk (so that negatives
; won't be mistaken for positives).
;
  (let ((n (length words)) chunks chunk negword changed result)
    (cond
      ; If less than 15 words, return only 1 chunk
      ((< n 15) 
        (return-from form-chunks (list words)))
      ; Otherwise, form multiple word chunks, 10 words long,
      ; overlapping by 5 words
      (t
        (loop
          (setq n (- n 5))
          (setq chunk (butlast words (- n 5)))
          (setq words (last words n))
          (push chunk chunks)
          (when (<= n 10)
            (push words chunks)
            (return nil)))
        (setq chunks (reverse chunks))
        ; 'negword' copy-over:
        (dolist (chunk chunks)
          ;; (format t "~%negword = ~a" negword) ; DEBUGGING
          ;; (format t "~%  chunk = ~a" chunk) ; DEBUGGING
          (when negword (push negword chunk)
                        (setq changed t))
          (push chunk result)
          (setq negword
            (find (car (last chunk 6))
                  '(no not don\'t cannot can\'t won\'t couldn\'t
                    wouldn\'t never hardly))))
        (if changed (reverse result) chunks)))
)) ; END form-chunks



(defun modify-response (resp)
;``````````````````````````````
; A set of word-level operations, formerly part of the main (doolittle)
; program, to prepare choice-packet-derived responses for proper output.
; Changes YOU ARE to YOU ARE2 in preparation for replacement of YOU ARE2
; by I AM (whereas ARE remains ARE), and similarly for some other words.
;
  (compress
    (dual
      (presubst resp)))
) ; END modify-response



(defun dual (sentence)
;``````````````````````
; Replaces 'I' by 'you', 'you' by 'I', 'my' by 'your', etc.
;
  (cond
    ((null sentence) nil)
    ((numberp (car sentence))
      (cons (car sentence) (dual (cdr sentence))))
    ((null (get (car sentence) 'subst))
      (cons (car sentence) (dual (cdr sentence))))
    (t (cons (get (car sentence) 'subst)
      (dual (cdr sentence)))))
) ; END dual



(defun duals (word1 word2)
;``````````````````````````
; Forms duals between two words
;
  (progn (setf (get word1 'subst) word2)
         (setf (get word2 'subst) word1))
) ; END duals



(defun presubst (response)
;`````````````````````````````
; This function is applied to eta's responses before 
; their "dual" is formed and printed out. It helps avoid 
; outputs like
;      WHY DO YOU SAY I ARE STUPID
; (as the dual of WHY DO I SAY YOU ARE STUPID), while
; still correctly producing
;      WHY DO YOU SAY YOUR BROTHERS ARE STUPID
; (as the dual of WHY DO I SAY YOUR BROTHERS ARE STUPID).
;
; It replaces ARE by ARE2 when preceded by YOU (In turn, DUAL
; will replace YOU by I and ARE2 by AM, so that YOU ARE
; becomes I AM (whereas WE ARE, THEY ARE, etc., remain
; unchanged). Similarly it replaces YOU by YOU2 when it is
; the last word, or when it is not one of the first two
; words and is not preceded by certain conjunctions (AND,
; OR, BUT, THAT, BECAUSE, IF, WHEN, THEN, WHY, ...) or
; by certain subordinating verbs (THINK, BELIEVE, KNOW,...)
; This is in preparation for replacement of YOU2 by ME
; (rather than I) when DUAL is applied. This could be
; done in a more sophisticated way by using MATCH! 
; WAS -> WAS2 (after I) and WERE -> WERE2 (after YOU)
; have not been implemented.
;
  (cond
    ((null response) nil)
    ; After the initial call, if the input is more than one word,
    ; a number = max(1, no. of words processed) is maintained as
    ; cdr of 'response', while the actual remainder of the response
    ; is in 'car response'
    ((null (cdr response))
      (cond
        ((member (car response) '(you you\. you! you?))
          '(you2))
        (t response)))
    ((numberp (cdr response))
      (cond
        ((null (car response)) nil)
        ((null (cdar response)) (presubst (car response)))
        ; Response has a 0 or 1 flag as cdr, and the car contains
        ; at least two words
        (t (cond
          ((and
              (eq (caar response) 'you)
              (eq (cadar response) 'are))
            (cons 'you (cons 'are2 (presubst (cons (cddar response) 1)))))
          ((= 0 (cdr response))
            (cons (caar response) (presubst (cons (cdar response) 1))))
          ; At least 1 word has been processed, i.e. cdr is 1, and there
          ; are 2 or more words left
          (t (cond
            ((or
                (not (eq (cadar response) 'you))
                (and (cddar response) (eq (caddar response) 'are))
                (member (caar response)
                  '(and or but that because if so when then why think
                  see guess believe hope than know i you - --))
                (member (car (last (coerce (string (caar response)) 'list)))
                  '(#\, #\. #\; #\! #\? #\:) :test #'char-equal))
              (cons (caar response) (presubst (cons (cdar response) 1))))
            ; You (second element of (car response)) to be replaced by you2
            (t (cons (caar response)
              (cons 'you2 (presubst (cons (cddar response) 1)))))))))))

    ; (cdr response) is non-numeric, so that this is the first call, with
    ; 'response' containing 2 or more words
    (t (presubst (cons response 0)))
    
)) ; END presubst



(defun eval-func! (f &rest args)
;````````````````````````````````
; Evaluate some ELF function <some-func>.f and its args.
;
  (apply f args)
) ; END eval-func!



(defun eval-functions (wff)
;```````````````````````````
; Evaluates all ELF functions (<some-func>.f arg1 arg2 ...) in a wff.
;
  (ttt:apply-rule '(/ (function? _*) (eval-func! function? _*)) wff)
) ; END eval-functions



(defun nsubst-variable (plan-name val var)
;``````````````````````````````````````````
; Substitutes (destructively) a given value (val) for a given variable (var)
; in a plan.
;
  (nsubst val var (get plan-name 'rest-of-plan))
) ; END nsubst-variable



(defun nsubst-schema-args (args schema)
;```````````````````````````````````````
; Substitute the successive arguments in the 'args' list for successive
; variables occurring in the schema or plan header exclusive of the 
; episode variable characterized by the header predication (for 
; episodic headers). In relational schemas, headers are assumed to 
; be simple (infix) predications,
;          (<term> <pred> <term> ... <term>),
; and for event schemas they are of form
;          ((<term> <pred> <term> ... <term>) ** <term>).
; We look for variables among the terms (exclusive of the one following
; "**" in the latter header type), and replace them in succession by
; the members of 'args'.
;
  (let (header predication vars)
    (setq header (second schema))
    (if (eq (second header) '**)
      (setq predication (first header)) ; episodic
      (setq predication header)) ; nonepisodic
    (if (atom predication) ; unexpected
      (return-from nsubst-schema-args schema))
    (dolist (x predication)
      (if (variable? x) (push x vars)))
    (when (null vars) ; unexpected
      (format t "~%@@@ Warning: Attempt to substitute values ~
                ~%    ~a~%    in header ~a, which has no variables"
                args predication)
      (return-from nsubst-schema-args schema))
    (setq vars (reverse vars))
    (cond
      ((> (length args) (length vars))
        (format t "~%@@@ Warning: More values supplied, viz., ~
                  ~%    ~a,~%    than header ~a has variables"
                  args predication)
        (setq args (butlast args (- (length args) (length vars)))))
      ((< (length args) (length vars))
        (format t "~%@@@ Warning: Fewer values supplied, viz., ~
                  ~%    ~a,~%    than header ~a has variables"
                  args predication)
        (setq vars (butlast vars (- (length vars) (length args))))))
            
      ; Length of 'args' and 'vars' are equal (or have just been equalized)
    (dotimes (i (length args))
      (nsubst (pop args) (pop vars) schema))

    schema
)) ; END nsubst-schema-args



(defun skolem (name)
;````````````````````
; Creates a unique skolem constant given a name.
;
  (intern (format nil "~a.SK" (gensym (string-upcase (string name)))))
) ; END skolem



(defun skolem? (sk-name)
;``````````````````````````
; Checks if sk-name is a skolem constant.
;
  (and (atom sk-name) (equal (second (sym-split sk-name 3)) '.SK))
) ; END skolem?



(defun episode-var ()
;````````````````````````````
; Creates an episode variable starting with '?E'.
;
  (intern (format nil "?~a" (string (gensym "E"))))
) ; END episode-var



(defun episode-name (ep-var)
;``````````````````````````````````
; Given an episode variable (e.g., '?e1'), generate and return a unique
; episode name to be substituted in for the variable (e.g., 'EP38').
;
  (when (not (char-equal #\? (car (explode ep-var))))
    (format t "~%***Attempt to form episode name from ~
               ~%   non-question-mark variable ~a" ep-var)
    (return-from episode-name nil))
  (gensym "EP")
) ; END episode-name



(defun episode-and-proposition-name (dual-var)
;````````````````````````````````````````````````
; NOTE: function deprecated as of 6/9/2020, after it was decided that we
; would abandon the proposition variable format (i.e., '?a1.') for the
; episode schemas. -B.K.
; 'dual-var' is normally a variable symbol starting with '?' and ending
; in '.'. As such, it actually stands for 2 variables: a reified-
; proposition variable (when the period is included) and implicitly,
; for an episode variable (when the period is dropped). E.g., '?a1.'
; is a (reified) proposition variable, but implicitly it also specifies
; an episode variable '?a1' (no period). Correspondingly, we create 
; two new names (constants): one for an episode (e.g., 'EP38'), and 
; one for the corresponding reified proposition (e.g., 'EP38.'); we 
; then return the two variables with the two new names, e.g.,
;   ((?a1 . EP38) (?a1. . EP38.))
; If there is no final period, we just return an episode name for the
; variable, e.g. if dual-var is just '?a1', we return
;   ((?a1 . EP38))
;
  (let ((chars (explode dual-var)) ep-var ep-name prop-name result)
    (when (not (char-equal #\? (car chars)))
      (format t "~%***Attempt to form episode and proposition ~
                 name from~%   non-question-mark variable ~a" dual-var)
      (return-from episode-and-proposition-name nil))
    (setq ep-var dual-var) ; will be used if there's no final period
    (setq ep-name (gensym "EP"))
    (when (char-equal #\. (car (last chars))) ; form proposition name
      (setq ep-var (implode (butlast chars))) ; implicit ep-var
      (setq prop-name (implode (append (explode ep-name) '(#\.))))
      (setq result (list (cons dual-var prop-name))))
    (push (cons ep-var ep-name) result)
)) ; END episode-and-proposition-name



(defun get-keyword-contents (lst keys)
;``````````````````````````````````````
; Gets the contents corresponding to a list of keywords in a record structure
; (assuming the contents are a single element immediately following the keyword).
;
  (mapcar (lambda (key)
    (if (keywordp key)
      (second (member key lst)))) keys)
) ; END get-keyword-contents



(defun get-schema-sections (schema)
;```````````````````````````````````
; Gets a hash table containing each schema section as a key, and the
; contents of that section as the value.
;
  (let ((schema-ht (make-hash-table :test #'equal)))
    (dolist (section '(:types :rigid-conds :episodes))
      (let ((contents (car (get-keyword-contents schema (list section)))))
        (when contents
          (setf (gethash section schema-ht) contents))))
    schema-ht)
) ; END get-schema-sections



(defun create-say-to-wff (content &key reverse)
;```````````````````````````````````````````````
; Creates and returns a wff consisting of a (~me say-to.v ~you '(...))
; action, or a (~you say-to.v ~me '(...)) action if :reverse t is given.
;
  (if (not reverse)
    `(~me say-to.v ~you (quote ,(modify-response content)))
    `(~you say-to.v ~me (quote ,content)))
) ; END create-say-to-wff



(defun get-episode-vars (plan)
;``````````````````````````````
; Form a list of all episode vars (in proposition form) from a plan.
;
  (let (var vars)
    (cond
      ; Base case - if plan is a symbol, return the symbol if it is an action
      ; var, or nil otherwise.
      ((symbolp plan)
        (if (variable? plan)
          `(,(if (ep-var? plan) (intern (format nil "~a" plan)) plan)) nil))
      ; Recursive case
      (t
        (remove-duplicates
          (remove nil (mapcan #'get-episode-vars plan))
          :test #'equal))))
) ; END get-episode-vars



(defun subst-duplicate-variables (plan-name plan)
;``````````````````````````````````````````````````
; Substitutes all variables in a plan with duplicate variables, inheriting
; the gist-clauses, ulf, etc. attached to them in the schema used (directly
; or indirectly) to create the current plan.
;
  (let* ((episode-vars (get-episode-vars plan))
        (new-episode-vars (mapcar (lambda (episode-var)
          (duplicate-variable plan-name episode-var)) episode-vars))
        (result plan))
    (mapcar (lambda (var new-var)
      (setq result (subst new-var var result)))
      episode-vars new-episode-vars)
  result)
) ; END subst-duplicate-variables



(defun duplicate-variable (plan-name var)
;```````````````````````````````````````````````````
; Duplicates an episode variable, inheriting the gist-clauses,
; ulf, etc. attached to it in the schema used (directly or
; indirectly) to create the current plan.
;
  (let (new-var schema-name)
    ; Create new episode variable
    (setq new-var
      (intern (format nil "~a" (gentemp (string var)))))
    (setq schema-name (get plan-name 'schema-name))
    ; Inherit gist-clauses, semantics, and topic keys
    (setf (gethash new-var (get schema-name 'gist-clauses))
      (gethash var (get schema-name 'gist-clauses)))
    (setf (gethash new-var (get schema-name 'semantics))
      (gethash var (get schema-name 'semantics)))
    (setf (gethash new-var (get schema-name 'topic-keys))
      (gethash var (get schema-name 'topic-keys)))
  ; Return new var
  new-var)
) ; END duplicate-variable



(defun print-current-plan-status (plan-name)
;`````````````````````````````````````````````
; Show plan names, action names and wffs reached in following 
; 'rest-of-plan' pointers from 'plan-name'; also show 'subplan-of'
; pointers. This function is intended for debugging.
;
  (let ((rest (get plan-name 'rest-of-plan)) step-name wff
        superstep-name subplan-name (cont t))
    (format t "~%Status of ~a " plan-name)
    (setq superstep-name (get plan-name 'subplan-of))
    (if superstep-name
      (format t "(subplan-of ~a):" superstep-name)
      (format t "(no superstep):" plan-name))
    (loop while cont do
      (setq step-name (car rest))
      (when (null step-name)
        (format t "~%  No more steps in ~a." plan-name)
        (format t "~%  --------------------")
        (return-from print-current-plan-status nil))
      (setq wff (second rest))
      (format t "~%  rest of ~a = (~a ~a ...)" plan-name step-name wff)
      (setq subplan-name (get step-name 'subplan))
      (when subplan-name
        (format t "~%  subplan ~a of ~a:" subplan-name step-name)
        (setq rest (get subplan-name 'rest-of-plan))
        (setq plan-name subplan-name))
      (unless subplan-name
        (setq cont nil)))
)) ; END print-current-plan-status



(defun read-log-contents (log)
;```````````````````````````````
; Reads the contents of a given log file and converts to list.
;
  (let (result)
    (with-open-file (logfile log :if-does-not-exist :create)
      (do ((l (read-line logfile) (read-line logfile nil 'eof)))
          ((eq l 'eof) "Reached end of file.")
        (setq result (concatenate 'string result " " l))))
    (read-from-string (concatenate 'string "(" result ")")))
) ; END read-log-contents



(defun write-ulf (ulf)
;````````````````````````
; Writes a ulf to the file ulf.lisp, so that it can be used
; by the blocksworld system.
;
  (with-open-file (outfile "./io/ulf.lisp" :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
    (format outfile "(setq *next-ulf* ~a)" ulf))
) ; END write-ulf



(defun print-words (wordlist)
;``````````````````````````````
; This is intended for the keyboard-based mode of interaction,
; i.e., with *live* = nil.
;
  (format t "~%...")
  (dolist (word wordlist)
    (princ " ")
    (princ word)
    (if (or (member word '(? ! \.))
            (member (car (last (explode word))) '(#\? #\! #\.)))
      (format t "~%")))
) ; END print-words



(defun say-words (wordlist)
;````````````````````````````
; This is intended for th *live* = T mode of operation, i.e., I/O
; is via the virtual agent; (but the output is printed as well).
; For terminal mode only, we use 'print-words'.
;
  (let (wordstring)
    ; Write ETA's words to "./io/output.txt" as a continuous string
    ; (preceded by the output count and a colon)
    (dolist (word wordlist)
      (push (string word) wordstring)
      (push " " wordstring))
    (setq wordstring (reverse (cdr wordstring)))
    (setq wordstring (eval (cons 'concatenate (cons ''string wordstring))))

    ; Increment output number
    (setq *output-count* (1+ *output-count*))
	  
    ; Output words
    (with-open-file (outfile "./io/output.txt" :direction :output
                                            :if-exists :append
                                            :if-does-not-exist :create)
      (format outfile "~%#~D: ~a" *output-count* wordstring))

    ; Also write ETA's words to standard output:
    (format t "~% ... ")
    (dolist (word wordlist)
      (format t "~a " word)
      (if (or (member word '(? ! \.))
              (member (car (last (explode word))) '(#\? #\! #\.)))
        (format t "~%")))
    (format t "~%")
)) ; END say-words



(defun read-words (&optional str) 
;``````````````````````````````````
; This is the input reader when ETA is used with argument live =
; nil (hence also *live* = nil), i.e., with terminal input rather
; than live spoken input.
; If optional str parameter given, simply read words from str.
;
  (finish-output)
  (parse-chars (coerce (if str str (read-line)) 'list))
) ; END read-words



(defun hear-words () 
;````````````````````
; This waits until it can load a character sequence from "./io/input.lisp",
; which will set the value of *next-input*, and then processes *input*
; in the same way as the result of (read-line) is processed in direct
; terminal input mode.
;
  ; Write empty star line to output to prompt avatar to listen
  ; TODO: there has to be a better way of doing this...
  (setq *output-count* (1+ *output-count*))
  (with-open-file (outfile "./io/output.txt" :direction :output
                                             :if-exists :append
                                             :if-does-not-exist :create)
    (format outfile "~%*~D: dummy" *output-count*))

  (setq *next-input* nil)
  (loop while (not *next-input*) do
    (sleep .5)
    (progn
      (load "./io/input.lisp")
		  (if *next-input*
        (progn
          (format t "~a~%" *next-input*)
          (with-open-file (outfile "./io/input.lisp" :direction :output 
                                                  :if-exists :supersede
                                                  :if-does-not-exist :create))))))
          
  (parse-chars (coerce *next-input* 'list))
) ; END hear-words



(defun get-perceptions () 
;``````````````````````
; This waits until it can load a list of block perceptions from "./io/perceptions.lisp".
; This should have a list of relations of the following two forms:
; ((the.d (|Twitter| block.n)) at-loc.p ($ loc ?x ?y ?z))
; ((the.d (|Toyota| block.n)) ((past move.v) (from.p-arg ($ loc ?x1 ?y1 ?z1)) (to.p-arg ($ loc ?x2 ?y2 ?z2))))
;
  (setq *next-perceptions* nil)
  (loop while (not *next-perceptions*) do
    (sleep .5)
    (progn
      (load "./io/perceptions.lisp")
		  (if *next-perceptions*
        (with-open-file (outfile "./io/perceptions.lisp" :direction :output 
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)))))
          
  *next-perceptions*
) ; END get-perceptions



(defun get-perceptions-offline () 
;``````````````````````````````````
; This is the perceptions reader when ETA is used with argument live =
; nil (hence also *live* = nil)
;
  (finish-output)
  (read-from-string (read-line))
) ; END get-perceptions-offline



(defun load-obj-schemas ()
;```````````````````````````````````````````````
; Load core object schemas
; (in directory 'core/resources/obj-schemas')
; NOTE: I don't like having this here (loaded during Eta's
; 'init' function), but it's currently necessary since
; the equality sets and context are only defined in 'init'.
;
(mapcar (lambda (file) (load file))
    (directory "core/resources/obj-schemas/*.lisp"))
) ; END load-obj-schemas



(defun store-obj-schema (obj-type canonical-name schema)
;``````````````````````````````````````````````````````````
; Stores an object schema with an associated canonical name. Also stores the
; generic name as an alias, e.g., (k BW-arch.n), generated from the header.
;
  (let (generic-name schema-record)
    (setq schema-record (cons '$ schema))
    (setq generic-name (list 'k (cadar (get-keyword-contents schema '(:header)))))
    (add-alias generic-name canonical-name)
    (add-alias schema-record canonical-name)
    (store-in-context (list canonical-name obj-type)))
) ; END store-obj-schema



(defun store-concept-set (set-type canonical-name concept-set)
;```````````````````````````````````````````````````````````````
; Stores a concept set (i.e., set of object schema names) with an associated
; canonical name. Also stores the generic name, i.e. a set of the generic
; names of the objects in the set.
;
  (let (generic-name)
    (setq generic-name (make-set (mapcar (lambda (concept)
        (find-if (lambda (alias)
          (equal (car alias) 'k)) (get-aliases concept)))
      concept-set)))
    (add-alias generic-name canonical-name)
    (store-in-context (list canonical-name set-type))
    (mapcar (lambda (concept)
        (store-in-context
          (list concept 'member-of.p canonical-name)))
      concept-set))
) ; END store-concept-set



(defun request-goal-rep (obj-schema)
;`````````````````````````````````````
; Writes a ulf to the file ulf.lisp, so that it can be used
; by the blocksworld system.
;
  (with-open-file (outfile "./io/goal-request.lisp" :direction :output
                                                    :if-exists :supersede
                                                    :if-does-not-exist :create)
    (format outfile "(setq *chosen-obj-schema* '~s)" obj-schema))
) ; END request-goal-rep



(defun get-goal-rep ()
;```````````````````````
; This waits until it can load a goal representation from "./io/goal-rep.lisp".
;
  (setq *goal-rep* nil)
  (loop while (not *goal-rep*) do
    (sleep .5)
    (progn
      (load "./io/goal-rep.lisp")
		  (if *goal-rep*
        (with-open-file (outfile "./io/goal-rep.lisp" :direction :output 
                                                      :if-exists :supersede
                                                      :if-does-not-exist :create)))))
  *goal-rep*
) ; END get-goal-rep



(defun get-answer () 
;``````````````````````
; This waits until it can load a list of relations from "./io/answer.lisp".
;
  (setq *next-answer* nil)
  (loop while (not *next-answer*) do
    (sleep .5)
    (progn
      (load "./io/answer.lisp")
		  (if *next-answer*
        (with-open-file (outfile "./io/answer.lisp" :direction :output 
                                                    :if-exists :supersede
                                                    :if-does-not-exist :create)))))
          
  (if (equal *next-answer* 'None) nil
    *next-answer*)
) ; END get-answer



(defun get-answer-offline () 
;`````````````````````````````
; This is the answer reader when ETA is used with argument live =
; nil (hence also *live* = nil)
;
  (finish-output)
  (let ((ans (read-from-string (read-line))))
    (if (equal ans 'None) nil ans))
) ; END get-answer-offline



(defun get-answer-string () 
;````````````````````````````
; This waits until it can load a character sequence from "./io/answer.lisp",
; which will set the value of *next-answer*, and then processes it.
;
  (setq *next-answer* nil)
  (loop while (not *next-answer*) do
    (sleep .5)
    (progn
      (load "./io/answer.lisp")
		  (if *next-answer*
        (with-open-file (outfile "./io/answer.lisp" :direction :output 
                                                   :if-exists :supersede
                                                   :if-does-not-exist :create)))))
          
  ;; (parse-chars (if (stringp *next-answer*) (coerce *next-answer* 'list)
  ;;                                            (coerce (car *next-answer*) 'list)))
  (cond
    ((stringp *next-answer*) (list (parse-chars (coerce *next-answer* 'list))))
    ((listp *next-answer*) (cons (parse-chars (coerce (car *next-answer*) 'list))
                            (cdr *next-answer*))))
) ; END get-answer-string



(defun update-block-coordinates (moves)
;````````````````````````````````````````
; Given a list of moves (in sequential order), update *block-coordinates*. Return a list of
; perceptions, i.e. the given moves combined with the current block coordinates.
;
  (mapcar (lambda (move)
    (setq *block-coordinates* (mapcar (lambda (coordinate)
        (if (equal (car move) (car coordinate))
          (list (car coordinate) 'at-loc.p (cadar (cddadr move)))
          coordinate))
      *block-coordinates*))) moves)
  (append *block-coordinates* moves)
) ; END update-block-coordinates



(defun verify-log (answer-new turn-tuple filename)
;```````````````````````````````````````````````````
; Given Eta's answer for a turn, allow the user to compare to the answer in the log
; and amend the correctness judgment for that turn. Output to the corresponding
; filename in log_out/ directory.
;
  (let ((filename-out (concatenate 'string "logs/logs_out/" (pathname-name filename)))
        (answer-old (read-words (third turn-tuple))) (feedback-old (fourth turn-tuple)) feedback-new)
    ;; (format t "/~a~%\\~a~%" answer-old answer-new)
    (with-open-file (outfile filename-out :direction :output :if-exists :append :if-does-not-exist :create)
      (cond
        ; If answer is the same, just output without modification
        ((equal answer-old answer-new)
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%" (first turn-tuple) (second turn-tuple) (third turn-tuple) (fourth turn-tuple)))
        ; If question was marked as non-historical, also skip
        ((member (fourth turn-tuple) '(XC XI XP XE))
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%" (first turn-tuple) (second turn-tuple) (third turn-tuple) (fourth turn-tuple)))
        ; Otherwise, check the new output with the user and prompt them to change feedback
        (t
          (format t " ----------------------------------------------------------~%")
          (format t "| A CHANGE WAS DETECTED IN LOG '~a':~%" (pathname-name filename))
          (format t "| * question: ~a~%" (first turn-tuple))
          (format t "| * old answer: ~a~%" answer-old)
          (format t "| * old feedback: ~a~%" (fourth turn-tuple))
          (format t "| * new answer: ~a~%" answer-new)
          (format t "| > new feedback: ")
          (finish-output) (setq feedback-new (read-from-string (read-line)))
          (format t " ----------------------------------------------------------~%")
          (if (not (member feedback-new '(C I P F E))) (setq feedback-new 'E))
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%"
            (first turn-tuple) (second turn-tuple) (format nil "~{~a~^ ~}" answer-new) feedback-new)))))
) ; END verify-log



(defun parse-chars (chars) 
;```````````````````````````
; Parses a list of chars by forming a list of character sublists,
; where each sublist is made into an atom (taking into account
; special characters)
;
; Takes a character list as input. Then tokenize into a list of
; upper-case atoms, treating (i) any nonblank character following a
; blank, (ii) any non-blank nonalphanumeric character other than
; #\', #\-, #\_ following an alphanumeric character, and (iii) any
; alphanumeric character following a nonalphanumeric character other
; than #\', #\-, #\_, as the start of a new atom.
;
  (let (prevch chlist chlists)
    (if (null chars) (return-from parse-chars nil))
    ; Form a list of character sublists, each sublist to be made
    ; into an atom; (the list & sublists will at first be backward,
    ; and so have to be reversed before interning & output)
    (setq prevch #\Space)
    (dolist (ch chars)
      ; Do we have the start of a new word?
      (if
        (or
          (and
            (char-equal prevch #\Space) 
            (not (char-equal ch #\Space)))
          (and
            (alphanumericp prevch)
            (not (alphanumericp ch))
            (not (member ch '(#\Space #\' #\- #\_) :test #'char-equal)))
          (and
            (not (alphanumericp prevch))
            (not (member prevch '(#\' #\- #\_) :test #'char-equal))
            (alphanumericp ch)))
        ; If so, push the current chlist (if nonempty) onto 
        ; chlists, and start a new chlist containing ch
        (progn (if chlist (push (reverse chlist) chlists))
          (setq chlist (list (char-upcase ch))))
        ; If not, push ch (if nonblank) onto the current chlist
        (if (not (char-equal ch #\Space))
          (push (char-upcase ch) chlist)))
      (setq prevch ch))
        
    ; Push the final chlist (if nonempty) onto chlists (in reverse)
    (if chlist (push (reverse chlist) chlists))
    ; Return the reverse of chlists, where each sublist has been
    ; interned into an atom
    (reverse (mapcar (lambda (x) (intern (coerce x 'string))) chlists))
)) ; END parse-chars



(defun str-to-output (str)
; ``````````````````````````
; Converts a string to a list of words/punctuation to output
; 
  (let ((char-list (coerce str 'list)) word words)
    (mapcar (lambda (c)
      (cond
        ((member c '(#\ ) :test #'char-equal)
          (if word (setq words (cons (reverse word) words)))
          (setq word nil))
        ((member c '(#\. #\, #\' #\"))
          (setq words (cons (reverse word) words))
          (setq word nil)
          (setq words (cons (intern (coerce (list c) 'string)) words)))
        (t
          (setq word (cons c word)))))
      char-list)
    (reverse (mapcar (lambda (w)
      (if (listp w) (read-from-string (coerce w 'string)) w)) words)))
) ; END str-to-output



(defun match1 (pattern input)
;``````````````````````````````
; For top-level match tracing
;
  (match pattern input)
) ; END match1



(defun match (pattern input)
;``````````````````````````````
; Match decomposition pattern PATTERN against context-embedded,
; tagged input utterance INPUT. PATTERN is a list consisting of
; particular words or features (tags), NIL elements (meaning
; "exactly one word"), and numbers 0 (meaning "0 or more words"),
; 1, 2, 3, ... (interpreted as upper bounds on the number of
; words). In addition, the first element of a pattern may be "-",
; meaning that the INPUT should NOT match the remainder of the
; pattern. The output, if successful (non-NIL), is a list of
; parts of the input, one for each word, tag, NIL element, and 
; numerical element of the pattern. A "part" is just a list
; of words that actually occurred in the sentence, where
; these words are initial elements of tag-lists in INPUT.
; For "negative" patterns headed by "-", the output in case of
; success (i.e., remainder of pattern NOT matched) is a list
; containing the list of actual input words.
;
  (let (result)
    ; If empty pattern, return nil
    (if (null pattern) (return-from match nil))

    ; If pattern starts with negation symbol '-', return input as
    ; result if the cdr of the pattern fails to match, nil otherwise
    (cond ((eq (car pattern) '-)
      (setq result (match (cdr pattern) input))
      (if result (return-from match nil)
        (return-from match (list (mapcar #'car input))))))

    ; If empty input, if pattern is simply wildcard, then return
    ; empty list, otherwise return nil
    (if (null input)
      (if (and
            (null (cdr pattern))
            (numberp (car pattern)))
        (return-from match '(()))
        (return-from match nil)))
    
    ; Otherwise, check first element of pattern
    (cond
      ; Pattern starts with 0 (matches any number of words)
      ((equal (car pattern) 0)
        (cond
          ; 0 only
          ((null (cdr pattern))
            (return-from match (list (mapcar #'car input))))
          ; More after the 0
          (t (setq result (match (cdr pattern) input))
            (cond (result (return-from match (cons nil result)))
              ; Matching 0 to null sequence failed
              (t (setq result (match pattern (cdr input)))
                ; Pattern with initial 0 matches (cdr input)
                (if result
                  (return-from match
                    (cons (cons (caar input) (car result)) (cdr result)))
                  (return-from match nil)))))))

      ; Pattern starts with 1 (matches at most one word)
      ((equal (car pattern) 1)
        (cond ((null (cdr pattern))
          (if (null input) (return-from match '(())))
          (if (null (cdr input)) (return-from match
            (list (list (caar input)))))
          (return-from match nil)))
        (setq result (match (cdr pattern) input))
        (cond (result (return-from match (cons nil result)))
          ; Matching 1 to null sequence failed
          (t (setq result (match (cdr pattern) (cdr input)))
            (if result
              (return-from match
                (cons (list (caar input)) result))
              (return-from match nil)))))

      ; Pattern starts with n (matches at most n words)
      ((numberp (car pattern))
        (setq result (match (cdr pattern) input))
        (cond (result (return-from match (cons nil result)))
          ; Matching number to empty sequence failed
          (t (setq result (match (cons (- (car pattern) 1)
                            (cdr pattern)) (cdr input)))
            (if result
              (return-from match
                (cons (cons (caar input) (car result)) (cdr result)))
              (return-from match nil)))))
    
      ; Pattern starts with nil (matches any single word)
      ((null (car pattern))
        (if (not (or (cdr pattern) (cdr input)))
          (return-from match (list (list (caar input)))))
        (setq result (match (cdr pattern) (cdr input)))
        (if result
          (return-from match (cons (list (caar input)) result))
          (return-from match nil)))

      ; Pattern starts with a word or feature (tag)
      (t
        (if (not (member (car pattern) (car input)))
          (return-from match nil))
        (if (not (or (cdr pattern) (cdr input)))
          (return-from match (list (list (caar input)))))
        (setq result (match (cdr pattern) (cdr input)))
        (if result
          (return-from match (cons (list (caar input)) result))
          (return-from match nil))))
)) ; END match



(defun instance (pattern parts)
;```````````````````````````````
; Top-level call to 'instance1', dealing with special case where pattern
; is an in-range integer and the part it indexes has just one word in it,
; which will be returned as result. Also this function serves for top-
; level tracing of pattern instantiation;
;
  (let ((n (length parts)) part)
    (cond
      ((and
          (integerp pattern)
          (> pattern 0)
          (<= pattern n))
        (setq part (car (nthcdr (- pattern 1) parts)))
        (if (= (length part) 1) (car part)
          (format nil "~%***Incoherent 'instance' arguments: ~%   ~
                      ~a, ~a" pattern parts)))
      (t (instance1 pattern parts)))
)) ; END instance



(defun instance1 (pattern parts)
;````````````````````````````````
; Substitute the words in the ith sublist of 'parts' for any occurrences 
; of i in 'pattern'. This done recursively by replacing in-range integers
; by corresponding parts, while recursively processing other elements and
; wrapping a pair of brackets around them, and in the end appending 
; everything;
;
  (if (atom pattern)
    pattern
    (let ((n (length parts)))
      (apply #'append
        (mapcar (lambda (x)
          (if (and (integerp x) (> x 0) (<= x n))
            (car (nthcdr (- x 1) parts))
            (list (instance1 x parts))))
        pattern))))
) ; END instance1



(defun readrules (rootname packet)
;``````````````````````````````````
; This reads in the set of decomposition and output rules. 
; It embeds these rules in a tree whose root node is 'rootname',
; and where first children are reached via the 'children' property,
; and children are connected via the 'next' property. Rule node
; names are generated by GENSYM. Decomposition and output patterns
; are stored under the 'pattern' property, and output rules are
; distinguished by having a non-NIL 'directive' property.
;
; READRULES also puts a numerical property called 'latency' 
; on the property list of output rules. This is the number
; of outputs that must be generated before the rule can be
; used again. As indicated below, in the data set the numeric
; value of latency and the directive symbol are supplied jointly
; as a 2-element list, but these become separate properties
; in the choice tree that is built.
;
; 'Packet' is of form (depth pattern optional-pair
;	                depth pattern optional-pair ...)
; where "depth" is a number =1 for top-level rules, =2 for
; direct descendants of top-level rules, etc.; "pattern" is
; a decomposition pattern or other output; and optional-pair
; is present iff "pattern" is a reassembly pattern or other 
; output. The first element of optional-pair, if present, is 
; the latency of the rule. The second element (the directive
; symbol) is :out (for reassembly), :subtree (for continuing
; recursion in a subtree), :subtrees (for a list of subtrees
; to be returned as output), :subtree+clause (for a recursive
; continuation where a choice tree root plus a clause constructed
; by a reassembly rule are used), :schema (when the output is
; the name of a schema), :schema+args (when the schema name is 
; accompanied by an argument list whose elements depend on
; reassembly), and :gist (when the output is a decontextualized
; user sentence -- a "gist clause").
;
  (let (stack rest n name)
    (setf (get rootname 'pattern) (cadr packet))
    ; The root is at depth 1, its 'next' or 'children' properties
    ; will be set if/when a rule at the same or lower depth is encountered
    (setq stack `((1 . ,rootname)))
    ; Advance past the 1st depth-# and pattern
    (setq rest (cddr packet))

    ; Loop until full rule tree is built
    (loop while rest do
      ; Save next depth number (or optional pair)
      (setq n (car rest))
      ; Advance past depth number to next pattern, or past the
      ; optional pair to the next depth number
      (setq rest (cdr rest))

      (cond
        ; If n is a number, it is the depth of a new rule
        ((numberp n)
          (setq name (gensym "RULE"))
          ;; (format t "~%New rule ~a " name) ; DEBUGGING
          (setf (get name 'pattern) (car rest))
          ;; (format t "~%'pattern' property of ~a is ~%  ~a" 
          ;;               name (get name 'pattern)) ; DEBUGGING
          ; Advance past the current pattern
          (setq rest (cdr rest))

          (cond
            ; New rule at same depth?
            ((equal n (caar stack))
              ; Let 'next' of previous rule point to new rule
              (setf (get (cdar stack) 'next) name)
              ;; (format t "~%@@ 'next' pointer of ~s has been ~
              ;;           set to ~s" (get (cdar stack) 'next) name) ; DEBUGGING
              (rplaca stack (cons n name)))

            ; New rule at greater depth?
            ((> n (caar stack))
              ; Let 'children' of previous rule point to new rule
              (setf (get (cdar stack) 'children) name)
              ;; (format t "~%@@ 'children' pointer of ~s has been ~
              ;;           set to ~s" (get (cdar stack) 'children) name) ; DEBUGGING
              ; Push new (level . rule name) pair onto stack
              (setq stack (cons (cons n name) stack)))

            ; New rule at lower depth?
            (t
              ; Depth differential
              (setq n (- (caar stack) n))
              ; Pop a number of stack elements equal to depth differential
              (dotimes (dummyvar n) (setq stack (cdr stack)))
              ; Resulting top element must be same depth, so set 'next' pointer to new rule
              (setf (get (cdar stack) 'next) name)
              ;; (format t "~%@@ 'next' pointer of ~s has been ~
              ;;           set to ~s" (get (cdar stack) 'next) name) ; DEBUGGING
              (rplaca stack (cons (caar stack) name)))))

        ; If n is a (latency directive) pair rather than depth number
        (t
          ; Set the 'latency' of the rule at the top of the stack to car of pair
          (setf (get (cdar stack) 'latency) (car n))
          ;; (format t "~%'latency' property of ~a is ~%  ~a"
          ;;           (cdar stack) (get (cdar stack) 'latency)) ; DEBUGGING
          ; Set the 'time-last-used' value to be more negative than any latency, so that
          ; no rule will be blocked initially
          (setf (get (cdar stack) 'time-last-used) -10000)
          ; Set the 'directive' property to the second element of the pair
          (setf (get (cdar stack) 'directive) (second n))
          ;; (format t "~%'directive' property of ~a is ~%  ~a"
          ;;           (cdar stack) (get (cdar stack) 'directive)) ; DEBUGGING
        )))
  "RULE TREE HAS BEEN BUILT")
) ; END readrules



(defun reset-rule (rule)
;`````````````````````````
; Reset 'time-last-used' to -100 in all assembly rules dominated by rule
;
  (cond
    ((null rule) nil)
    (t (reset-rule (get rule 'children))
      (reset-rule (get rule 'next))
      (if (get rule 'time-last-used)
        (setf (get rule 'time-last-used) -100))))
) ; END reset-rule



(defun test-parse (tree input)
;```````````````````````````````
; Used to check pattern matching/parsing given a rule tree and input (list of words)
;
  (choose-result-for (mapcar #'tagword input) tree)
) ; END test-parse



(defun error-check ()
;`````````````````````
; Checks whether program has entered an infinite loop using a counter
;
  (cond
    ((> *error-check* 100)
      (error-message "An error caused Eta to fall into an infinite loop. Check if the plan is being updated correctly." *live*)
      (error))
    (t (setq *error-check* (1+ *error-check*))))
) ; END error-check



(defun error-message (str mode)
;``````````````````````````
; Print error message to the console, and if in live mode, to output.txt
;
  (format t "~a~%" str)
  (if mode
    (with-open-file (outfile "./io/output.txt" :direction :output
                                            :if-exists :append
                                            :if-does-not-exist :create)
      (format outfile "~%#: ~a" str)))
) ; END error-message