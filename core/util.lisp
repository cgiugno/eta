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
;
  (if (every #'listp list) (cond
    ((nnp? x) (remove-if-not (lambda (l) (equal x (car l))) list))
    ((variable? x) list)
    ((restricted-variable? x)
      (let ((r (cadr x))) (remove-if-not
        (lambda (l) (equal (funcall (first r) (car l)) (second r))) list)))
    (t list)))
) ; END find-car-var



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
; Check whether a symbol is a variable, i.e. starts with '?' or '!'
;
  (and (symbolp atm) (member (car (explode atm)) '(#\? #\!) :test #'char-equal))
) ; END variable?



(defun answer-list? (list)
;``````````````````````````
; Check whether a list is a list of propositional answers with associated certainties.
;
  (or (and (listp list) (every (lambda (l) (and (listp l) (= 2 (length l)) (numberp (second l))
      (or (symbolp (first l)) (and (listp (first l)) (symbolp (car (first l))))))) list))
    (equal list 'None))
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
; Return t if gist-clause is the nil gist clause (i.e. '(NIL GIST : ...)).
;
  (and (>= (length gist-clause) 2) (equal (subseq gist-clause 0 2) '(NIL GIST)))
) ; END nil-gist-clause?



(defun purify-func (user-gist-clauses)
;````````````````````````````````````````
; Remove user gist clauses identical with '(NIL GIST), unless this is the only gist clause.
;
  (let ((purified-gist-clauses (remove-if (lambda (x) (nil-gist-clause? x)) user-gist-clauses)))
    (if purified-gist-clauses purified-gist-clauses '((NIL GIST))))
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
; Prints the context (dividing between full propositions, and ones that are stored under some specific key)
;
  (let (l1 l2)
    (maphash (lambda (k v)
      (if (equal v t) (setq l1 (cons k l1))
        (setq l2 (cons (list k v) l2)))) *context*)
    (mapcar (lambda (f)
      (format t "~a~%" f)) l1)
    (mapcar (lambda (f)
      (format t "~a~%" f)) l2))
) ; END print-context



(defun get-from-context (key)
;``````````````````````````````
; Retrieves a fact from context
;
  (gethash key *context*)
) ; END get-from-context



(defun store-fact (fact ht &key keys no-self)
;``````````````````````````````````````````````
; Stores a fact in a given hash table. The fact is always hashed on itself.
; Optionally, a list of additional keys can be specified for the fact to be hashed on.
; Also, if no-self is given as true, don't hash on the fact itself.
;
  (unless no-self (setf (gethash fact ht) t))
  (mapcar (lambda (key)
    (setf (gethash key ht) (append (gethash key ht) (list fact)))) keys)
) ; END store-fact



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



(defun new-var! ()
;```````````````````
; TTT pred for creating new variable starting with ?x
  (intern (format nil "?~a" (string (gensym "X"))))
) ; END new-var!



(defun action-var ()
;````````````````````````````
; Creates an action variable starting with "?A" % with final "."
  (intern (format nil "?~a." (string (gensym "A"))))
) ; END action-var



(defun action-name ()
;````````````````````````````
; Creates an action name starting with "A" % with final "."
  (intern (format nil "~a." (string (gensym "A"))))
) ; END action-name



(defun episode-and-proposition-name (dual-var)
;````````````````````````````````````````````````
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



(defun get-schema-sections (plan)
;``````````````````````````````````
; Gets list of sections of schema, currently forms tuple (types episodes)
; TODO: Improve reading schemas - store as key-value pairs using class?
;
  (let (sections)
    (dolist (section '(:episodes :types))
      (let* ((lookup (member section plan))
             (rest (member nil (cdr lookup) :test (lambda (x y) (keywordp y)))))
        (setq sections
          (cons (cons (car lookup)
            (if rest (reverse (set-difference (cdr lookup) rest)) (cdr lookup))) sections))))
    sections)
) ; END get-schema-sections



(defun create-say-to-wff (content &key reverse)
;```````````````````````````````````````````````
; Creates and returns a wff consisting of a (me say-to.v you '(...))
; action, or a (you say-to.v me '(...)) action if :reverse t is given.
;
  (if (not reverse)
    `(me say-to.v you (quote ,(modify-response content)))
    `(you say-to.v me (quote ,content)))
) ; END create-say-to-wff



(defun get-episode-vars (plan)
;`````````````````````````````
; Form a list of all episode vars (in proposition form) from a plan.
;
  (let (var vars)
    (cond
      ; Base case - if plan is a symbol, return the symbol if it is an action
      ; var, or nil otherwise.
      ((symbolp plan)
        (if (variable? plan)
          `(,(if (ep-var? plan) (intern (format nil "~a." plan)) plan)) nil))
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
        (var-duals (mapcar (lambda (var)
          (cons (implode (butlast (explode var))) var)) episode-vars))
        (new-var-duals (mapcar (lambda (episode-var)
          (duplicate-variable plan-name episode-var)) episode-vars))
        (result plan))
    (mapcar (lambda (var-dual new-var-dual)
      (setq result
        (subst (car new-var-dual) (car var-dual)
          (subst (cdr new-var-dual) (cdr var-dual) result))))
      var-duals new-var-duals)
  result)
) ; END subst-duplicate-variables



(defun duplicate-variable (plan-name var)
;``````````````````````````````````````````
; Duplicates a variable, inheriting the gist-clauses, ulf, etc. attached to
; it in the schema used (directly or indirectly) to create the current plan.
;
  (let (new-var schema-name)
    ; Create new variable
    (setq new-var (intern (format nil "~a." (gentemp
      (if (prop-var? var)
        (string var)
        (concatenate 'string (string var) "."))))))
    (setq schema-name (get plan-name 'schema-name))
    ; Inherit gist-clauses, semantics, and topic keys
    (setf (gethash new-var (get schema-name 'gist-clauses))
      (gethash var (get schema-name 'gist-clauses)))
    (setf (gethash new-var (get schema-name 'semantics))
      (gethash var (get schema-name 'semantics)))
    (setf (gethash new-var (get schema-name 'topic-keys))
      (gethash var (get schema-name 'topic-keys)))
  ; Return (?a1 . ?a1.) pair
  (cons (implode (butlast (explode new-var))) new-var))
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



(defun read-words () 
;`````````````````````
; This is the input reader when ETA is used with argument live =
; nil (hence also *live* = nil), i.e., with terminal input rather
; than live spoken input.
;
  (finish-output)
  (parse-chars (coerce (read-line) 'list))
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
; (|Twitter| at-loc.p ($ loc ?x ?y ?z))
; (|Toyota| ((past move.v) (from.p-arg ($ loc ?x1 ?y1 ?z1)) (to.p-arg ($ loc ?x2 ?y2 ?z2))))
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
;`````````````````````````````
; This is the perceptions reader when ETA is used with argument live =
; nil (hence also *live* = nil)
;
  (finish-output)
  (read-from-string (read-line))
) ; END get-perceptions-offline



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