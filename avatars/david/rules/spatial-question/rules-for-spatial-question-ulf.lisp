; On github at https://github.com/bkane2/eta-blocksworld 
; Select rules/spatial-question/rules-for-spatial-question-ulf.lisp
; ========================================================================
; "rules-for-spatial-questions.lisp"  -- currently under revision, June 21/19
; from previous version (v6). Add "between" rules. That's a bit tricky,
; because the complement of "between" either can be a plural ("between two
; red blocks") or a full NP conjunction ("between a red block and a blue
; block"), or a conjunction with ellipsis ("between a red and a blue
; block"; "between the Toyota and the SRI block"; "between the Toyota
; and SRI blocks").
;
; One issue is that to allow for names like "Burger King", we currently 
; need to preprocess to change this to "Burger_King". Would an alternative
; be to introduce features 'name-part1', 'name-part2, ..., which apply to
; parts of a multiword name, where single-word names have feature 'name',
; *as well as* feature 'name-part1'? Can we introduce a mini-grammar for
; names whose use is triggered by feature 'name-part1', and that allows
; a word with feature 'name' as a name, and allows appropriate sequences
; of words with features 'name-part1', 'name-part2, ... as names? But we
; can't do that via 'eval-lex-ulf', because this assumed we know for sure
; that the word(s) supplied can be of the specified category -- which we
; can't be sure of, just given features 'name-part1', 'name-part2, ... .
; So the grammar itself would need to check for particular word sequences,
; which would really be very inelegant, and infeasible for realistically
; large sets of names.
;
; Short of switching from single-word features (for pattern matching)
; to features assigned to word sequences (possible in TTT, I suppose),
; I think preprocessing is the best option. This might be done via the 
; gist clause mechanism, as part of "repairing" the user's word sequence
; as produced by the speech recognizer. Repairs are extremely context/
; domain-specific, but then that's also true for gist clause determination
; in general.
; 
;
; TODO: Some examples of references that might be more difficult to handle
; "The furthest one"
; "What about the McDonalds block"
; etc.
;
; "Does the SRI block have a block [on top of][above] it?"
; "Does there exist a block on the SRI block"
; "What is the color of the block ..."
; "Where is the block on the table that is near a blue block"
; "Is the SRI block above the McDonalds block and not the Texaco block?"
; "Is the SRI block above a red block or a blue block?"
;

; ====================================================================

;; Choice packets for ulf derivation from spatial questions by user.
;;
;; The initial set of features are intended to support analysis
;; of the user's spatial relation questions in the Blocks world
;; 
(MAPC 'ATTACHFEAT
'(
;; NOTE: As of 11/25/19, these features are now stored
;; in core/resources/blocksworld-word-data.lisp
))

;; NB: All questions are expected to have a separate question mark at the end;
;;     If necessary, prior input processing of the user input (as supplied -- 
;;     somewhat unreliably -- by the speech recognizer) should separate off
;;     or add a question mark, when the input seems to be in the form of 
;;     a question (typically starting with a be-word, a wh-word, or a prep-
;;     osition plus wh-word)
;;
;; NB: :ulf-recur rules specify 2 reassembly patterns -- one for the successive
;;     parts, and one for putting the ULFs for the parts together with the 
;;     correct bracketing; (:ulf rules specify just the ulf assembly patterns).
;;
;;     The first of these reassembly patterns needs to be a list of any of
;;     the following 3 sorts of components:
;;     - an atom, which will be used as-is in the second reassembly pattern;
;;     - a triple (lex-ulf! <lex-cat> <part number>) which will (later) be
;;       evaluated into a lexical ULF atom or ULF expression;
;;     - a pattern of form (<name of a rule-tree> <itm1> <itm2> ...) where
;;       the <itmj> elements are typically integers indicating parts, but
;;       can also be arbitrary expressions, potentially containing integers
;;       indicating parts.
;;
;;     The second reassembly pattern just specifies how the "pieces" 
;;     (top-level components) in the first reassembly pattern should be 
;;     bracketed to yield the final ULF form (once the components of type
;;     (<name of a rule-tree> <itm1> <itm2> ...) have been recursively
;;     evaluated). It could also introduce additional ULF expressions,
;;     but for readability usually shouldn't, unless material needs to be
;;     added that doesn't fit with the syntactic constraints on types of
;;     components in the first reassembly pattern, as enumerated above.
;;     
;;     The expressions of form (lex-ulf! <lex-cat> <part number>) in the
;;     resulting ULF are evaluated as a final step in the program 
;;     'choose-result-for1', which interprets the directives.

(READRULES '*spatial-question-ulf-tree*
; ```````````````````````````````````````
; Top level tree for spatial questions. Later we can use a more general tree,
; not restricted to the blocks world, and use occurrence of "block" or 
; "table" (& perhaps a spatial relation) to jump to this tree.
;
'(
    ; "is/are"-questions
    1 (be 0 between 0) ; "between" causes issues with prep+NP patterns, so need specific rules
       2 *yn-between-question-ulf-tree* (0 :subtree)
    1 (be np_ 0)
       2 *yn-question-ulf-tree* (0 :subtree)

    ; existential there questions
    1 (be there 0)
       2 *exist-question-ulf-tree* (0 :subtree)

    ; have questions (usually indicates perfect aspect)
    1 (has 0)
       2 *has-question-ulf-tree* (0 :subtree)

    ; modal questions
    1 (modal 0); e.g., can you see the NVidia block ?
       2 *modal-question-ulf-tree* (0 :subtree)

    ; wh+do questions
    1 (wh_ 0 do 0); e.g., what block did I just move ?
       2 *wh-do-question-ulf-tree* (0 :subtree)

    ; how many questions
    1 (how many 0 between 0)
       2 *wh-between-question-ulf-tree* (0 :subtree)
    1 (how many 0)
       2 *how-many-question-ulf-tree* (0 :subtree)

    ; when questions
    1 (when 0)
       2 *when-question-ulf-tree* (0 :subtree)

    ; what color questions
    1 (wh_ color 0)
       2 *color-question-ulf-tree* (0 :subtree)

    ; where questions
    1 (where 0)
       2 *where-question-ulf-tree* (0 :subtree)

    ; wh-questions
    1 (wh_ 0 between 0)
       2 *wh-between-question-ulf-tree* (0 :subtree)
    1 (wh_ 0)
       2 *wh-question-ulf-tree* (0 :subtree)

    ; PP[wh]-questions
    1 (between 2 wh_ 0); e.g., Between which two blocks is the NVidia block ?
       2 *ppwh-between-question-ulf-tree* (0 :subtree)
    1 (prep 2 wh_ 0); e.g., On top of which block is the NVidia block ?
       2 *ppwh-question-ulf-tree* (0 :subtree)

    ; "do"-questions
    1 (do 0)
       2 *do-question-ulf-tree* (0 :subtree)

    ; declarative questions
    1 (np_ 3 be 0 ?); e.g. A red block is next to a blue block ?
       2 (((*spatial-sentence-ulf-tree* 1 2 3 4)) (1 ?)) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 np_ 0 ?); e.g. A red block adjoins a blue block ?
       2 (((*spatial-sentence-ulf-tree* 1 2 3 4 5)) (1 ?)) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 prep 0 ?); e.g. A red block sits between two green blocks ?
       2 (((*spatial-sentence-ulf-tree* 1 2 3 4 5 6)) (1 ?)) (0 :ulf-recur)

    ; fallback rules
    1 (0 block 0 between 0)
       2 *fallback-between-spatial-question-ulf-tree* (0 :subtree)
    1 (0 between 0 block 0)
       2 *fallback-between-spatial-question-ulf-tree* (0 :subtree)
    1 (0 block 0); last resort
       2 *fallback-spatial-question-ulf-tree* (0 :subtree) 
    1 (0 table 0)
       2 *fallback-spatial-question-ulf-tree* (0 :subtree)
    1 (0 pron 0)
       2 *fallback-spatial-question-ulf-tree* (0 :subtree)

)) ; END *spatial-question-ulf-tree*



(READRULES '*n1-ulf-tree* 
; ````````````````````````````
; Parses premodified nouns
;
'(
    ; Straightforward noun, possibly with a corporation and maybe a premodifying adj 
    1 (noun)
       2 (lex-ulf! noun 1) (0 :ulf)
    1 (corp noun); e.g., NVidia block
       2 ((lex-ulf! name 1) (lex-ulf! noun 2)) (0 :ulf)
    1 (adj corp noun); e.g., red NVidia block
       2 ((lex-ulf! adj 1) ((lex-ulf! name 2) (lex-ulf! noun 3))) (0 :ulf)

    ; Superlative adj's, possibly followed by more adjectives and then any postmodifiers 
    1 (sup-adj noun 0); e.g., highest block on the stack
       2 (((lex-ulf! sup-adj 1) (*n1-ulf-tree* 2 3)) (most-n 1 2)) (0 :ulf-recur)
    1 (sup-adj adj noun 0); e.g., highest red block on the table
       2 (((lex-ulf! sup-adj 1) (*n1-ulf-tree* 2 3 4)) 
          (most-n 1 2)) (0 :ulf-recur)
    
    ; Postmodifiers (allow two, i.e., 2 PPs or a PP and a relative clause (either order) 
    1 (2 noun 1 prep det 1 noun 1 prep 2 np-bw 3); e.g., blocks near each other on the table
       2 (((*n1-ulf-tree* 1 2) (*pp-ulf-tree* 3 4 5 6 7) (*pp-ulf-tree* 8 9 10 11 12))
          (n+preds 1 2 3)) (0 :ulf-recur)
    1 (2 noun 1 prep np_ 2 that be 1 prep 2 np-bw 3); e.g., block on the table that is near a red block
       2 (((*n1-ulf-tree* 1 2) (*pp-ulf-tree* 3 4 5 6) that.rel (lex-ulf! v 8)
          (*pp-ulf-tree* 9 10 11 12 13)) (n+preds 1 2 (3 (4 5)))) (0 :ulf-recur)
    1 (2 noun 1 prep np_ 3 conj np_ 3); e.g., red block above the SRI block and the NVidia block
       2 (((*n1-ulf-tree* 1 2) (*pp-ulf-tree* 3 4 5 6 7 8 9)) (n+preds 1 2)) (0 :ulf-recur)
    1 (2 noun 1 between 0 block); e.g., block between a blue and a green block
       2 (((*n1-ulf-tree* 1 2) (*pp-between-ulf-tree* 3 4 5 6)) (n+preds 1 2)) (0 :ulf-recur)
    1 (2 noun 1 prep 2 np-bw 3); e.g., block next_to the farthest blue block
       2 (((*n1-ulf-tree* 1 2) (*pp-ulf-tree* 3 4 5 6 7)) (n+preds 1 2)) (0 :ulf-recur)
    1 (2 noun that be 1 prep np_ 3 conj np_ 3); e.g., block that is above the SRI block and the NVidia block
       2 (((*n1-ulf-tree* 1 2) that.rel (lex-ulf! v 4) (*pp-ulf-tree* 5 6 7 8 9 10 11))
          (n+preds 1 (2 (3 4)))) (0 :ulf-recur)
    1 (2 noun that be 1 between 0 block); e.g., block that is between the NVidia block and SRI block
       2 (((*n1-ulf-tree* 1 2) that.rel (lex-ulf! v 4) (*pp-ulf-tree* 5 6 7 8))
          (n+preds 1 (2 (3 4)))) (0 :ulf-recur)
    1 (2 noun that be 1 prep 2 np-bw 3); e.g., block that is on the table
       2 (((*n1-ulf-tree* 1 2) that.rel (lex-ulf! v 4) (*pp-ulf-tree* 5 6 7 8 9))
          (n+preds 1 (2 (3 4)))) (0 :ulf-recur)

    ; PP after rel-clause might be a pred complement to "be", so we try this last 
    1 (2 noun that be 1 prep det 1 noun 1 prep 2 np-bw 3); e.g., block that is on the table near the red block
       2 (((*n1-ulf-tree* 1 2) that.rel (lex-ulf! v 4) (*pp-ulf-tree* 5 6 7 8 9 10)
          (*pp-ulf-tree* 10 11 12 13)) (n+preds 1 (2 (3 4)) 5)) (0 :ulf-recur)

    ; Ordinary premodifying adj's 
    1 (adj noun 0); e.g., red block that is to_the_left_of a blue block
       2 (((lex-ulf! adj 1) (*n1-ulf-tree* 2 3)) (1 2)) (0 :ulf-recur)
    1 (adj adj noun 0); e.g., second red block
       2 (((lex-ulf! adj 1) (lex-ulf! adj 2) (*n1-ulf-tree* 3 4)) (1 2 3)) (0 :ulf-recur)
    
   ; 2 rel-clauses unlikely, so hold off for now 
)) ; END *n1-ulf-tree*



(READRULES '*np-ulf-tree* 
; ``````````````````````````````
; Parses noun phrase.
;
'(
    ; Cases with a determiner 
    1 (det 2 noun 0); e.g., the nearest block to_the_left_of a red block
       2 (((lex-ulf! det 1) (*n1-ulf-tree* 2 3 4)) (1 2)) (0 :ulf-recur)

    ; Pronoun 
    1 (pron)
       2 (lex-ulf! pro 1) (0 :ulf)

    ; Numerical 
    1 (deg-adv num-adj 1 noun 5); e.g., exactly two red blocks in a stack
       2 (((lex-ulf! adv-a 1) (lex-ulf! adj 2) (*n1-ulf-tree* 3 4 5))
       ((nquan (1 2)) 3)) (0 :ulf-recur)

    ; Reification 
    1 (noun 0); e.g., blocks on the table
       2 (((*n1-ulf-tree* 1 2)) (k 1)) (0 :ulf-recur)
    1 (adj noun 0); e.g., red blocks in_front_of you
       2 (((*n1-ulf-tree* 1 2 3)) (k 1)) (0 :ulf-recur)
    1 (adj adj noun 0); e.g., leftmost red blocks on the table
       2 (((*n1-ulf-tree* 1 2 3 4)) (k 1)) (0 :ulf-recur)

    ; Ellipsis ("the turn before this")
    ; NOTE: we currently assume the user meant "turn" here... this probably isn't true in general
    1 (this)
       2 (((lex-ulf! det 1)) (1 turn.n)) (0 :ulf-recur)
    1 (that)
       2 (((lex-ulf! det 1)) (1 turn.n)) (0 :ulf-recur)

   ; Still need "There are...", "There is ..." sentence forms.
)) ; END *np-ulf-tree*



(readrules '*pp-ulf-tree* 
; ``````````````````````````````
; Parses prepositional phrase
;
'(
    ; Conjunction of two noun phrases 
    1 (prep det 2 noun conj det 2 noun); e.g., above the SRI block and the Nvidia block
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3 4) (*np-ulf-tree* 6 7 8)) (1 (set-of 2 3))) (0 :ulf-recur)
    1 (prep det 2 noun conj 1 corp noun); e.g., above the SRI block and Nvidia block
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3 4) (*np-ulf-tree* 2 6 7 8)) (1 (set-of 2 3))) (0 :ulf-recur)
    
    ; Preposition with a postmodified noun 
    1 (prep det 2 noun that 0); e.g., on the red block that is on the SRI block
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3 4 5 6)) (1 2)) (0 :ulf-recur)
    1 (prep det 2 noun prep 0); e.g., on the red block above the SRI block
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3 4 5 6)) (1 2)) (0 :ulf-recur)

    ; Simple prepositions 
    1 (prep det 2 noun); e.g., on a red block
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3 4)) (1 2)) (0 :ulf-recur)
    1 (prep 2 noun); e.g., on red blocks
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3)) (1 2)) (0 :ulf-recur)
    1 (prep pron); e.g., on it
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2)) (1 2)) (0 :ulf-recur)
    1 (prep this); e.g., before this
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2)) (1 2)) (0 :ulf-recur)
    1 (prep that); e.g., before that
       2 (((lex-ulf! prep 1) (*np-ulf-tree* 2)) (1 2)) (0 :ulf-recur)

    ; Recurse if there's a premodifying adverb-rel 
    1 (deg-adv prep det 3 noun); e.g., directly on a red block
       2 (((lex-ulf! adv-a 1) (*pp-ulf-tree* 2 3 4 5)) (1 2)) (0 :ulf-recur)
    1 (deg-adv prep adj 1 noun); e.g., directly on red blocks
       2 (((lex-ulf! adv-a 1) (*pp-ulf-tree* 2 3 4 5)) (1 2)) (0 :ulf-recur)
    1 (deg-adv prep pron); e.g., directly on it
       2 (((lex-ulf! adv-a 1) (*pp-ulf-tree* 2 3)) (1 2)) (0 :ulf-recur)
    1 (adv-history prep det 3 noun); e.g., recently on a red block
       2 (((lex-ulf! adv-e 1) (*pp-ulf-tree* 2 3 4 5)) (1 2)) (0 :ulf-recur)
    1 (adv-history prep adj 1 noun); e.g., previously on red blocks
       2 (((lex-ulf! adv-e 1) (*pp-ulf-tree* 2 3 4 5)) (1 2)) (0 :ulf-recur)
    1 (adv-history prep pron); e.g., initially on it
       2 (((lex-ulf! adv-e 1) (*pp-ulf-tree* 2 3)) (1 2)) (0 :ulf-recur)

    ; Reified event 
    1 (prep np_ 3 verb 0)
       2 (prep np_ 3 be verb prep np_ 3); e.g., before the NVidia block was put on the SRI block
          3 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3) (lex-ulf! v-pasv 5) (*pp-ulf-tree* 6 7 8))
             (1 (ke (2 (3 4))))) (0 :ulf-recur)
       2 (prep np_ 3 be verb); e.g., before the NVidia block was moved
          3 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3) (lex-ulf! v-pasv 5))
             (1 (ke (2 3)))) (0 :ulf-recur)
       2 (prep np_ 2 verb np_ 3 prep np_ 3); e.g., before I put the NVidia block on the SRI block
          3 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6) (*pp-ulf-tree* 7 8 9))
             (1 (ke (2 (3 4 5))))) (0 :ulf-recur)
       2 (prep np_ 2 verb np_ 3); e.g., before I moved it
          3 (((lex-ulf! prep 1) (*np-ulf-tree* 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6))
             (1 (ke (2 (3 4))))) (0 :ulf-recur)

)) ; END *pp-ulf-tree*



(readrules '*adv-ulf-tree* 
; `````````````````````````````
; Parses adverbial phrase
;
'(
    1 (just); just is an adv-e in the context of historical questions
       2 (lex-ulf! adv-e 1) (0 :ulf); NOTE: might need changing in future
    1 (deg-adv); e.g., directly
       2 (lex-ulf! adv-a 1) (0 :ulf)
    1 (adv-e); e.g., previously
       2 (lex-ulf! adv-e 1) (0 :ulf)
    1 (prep 0 very 0 noun-history); e.g., on the very first turn
       2 (((*pp-ulf-tree* 1 2 4 5)) (adv-e 1)) (0 :ulf-recur)
    1 (prep-history 0 noun-history conj prep-history 0 noun-history); e.g., before the fifth turn and after the second turn
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! cc 4) (*pp-ulf-tree* 5 6 7)) (adv-e (1 2 3))) (0 :ulf-recur)
    1 (deg-adv prep 0 noun-history); e.g., right before the last turn
       2 (((lex-ulf! adv-a 1) (*pp-ulf-tree* 2 3 4)) (adv-e (1 2))) (0 :ulf-recur)
    1 (prep 0 noun-history); e.g., during the first turn
       2 (((*pp-ulf-tree* 1 2 3)) (adv-e 1)) (0 :ulf-recur)
    1 (prep-history-simple 0); e.g., before I moved it
       2 (((*pp-ulf-tree* 1 2)) (adv-e 1)) (0 :ulf-recur)
    1 (det noun-history ago); e.g., two turns ago
       2 (((*np-ulf-tree* 1 2)) (adv-e (sub 1 (ago.p *h)))) (0 :ulf-recur)
    1 (adj-history noun-history); e.g., last turn
       2 (((*n1-ulf-tree* 1 2)) (adv-e (during.p (the.d 1)))) (0 :ulf-recur)
)) ; END *adv-ulf-tree*



(READRULES '*yn-question-ulf-tree* 
; `````````````````````````````````````
; Parses yes-no questions.
;
'(  
    ; Asking about conjunction of blocks
    1 (be np_ 3 and np_ 2 noun 0)
       ; Historical
       2 (be np_ 3 and np_ 2 noun 1 rel-adj adv-e 0 ?); e.g., were the NVidia block and the SRI block directly touching previously ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (*np-ulf-tree* 5 6 7) (lex-ulf! mod-a 8) (lex-ulf! adj 9) (*adv-ulf-tree* 10 11) ?)
             (((set-of 2 3) (1 (4 5) 6)) ?)) (0 :ulf-recur)
       2 (be np_ 3 and np_ 2 noun rel-adj adv-e 0 ?); e.g., were the NVidia block and the SRI block touching on the first turn ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (*np-ulf-tree* 5 6 7) (lex-ulf! adj 8) (*adv-ulf-tree* 9 10) ?)
             (((set-of 2 3) (1 4 5)) ?)) (0 :ulf-recur)
       2 (be np_ 3 and np_ 2 noun 1 prep each other adv-e 0 ?); e.g., were the NVidia block and the SRI block (directly) touching each other ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (*np-ulf-tree* 5 6 7) (*pp-ulf-tree* 8 9 10 11) (*adv-ulf-tree* 12 13) ?)
             (((set-of 2 3) (1 4 5)) ?)) (0 :ulf-recur)
       ; Standard
       2 (be np_ 3 and np_ 2 noun 1 rel-adj ?); e.g., are the NVidia block and the SRI block directly touching ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (*np-ulf-tree* 5 6 7) (lex-ulf! mod-a 8) (lex-ulf! adj 9) ?)
             (((set-of 2 3) (1 (4 5))) ?)) (0 :ulf-recur)
       2 (be np_ 3 and np_ 2 noun rel-adj ?); e.g., are the NVidia block and the SRI block touching ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (*np-ulf-tree* 5 6 7) (lex-ulf! adj 8) ?)
             (((set-of 2 3) (1 4)) ?)) (0 :ulf-recur)
       2 (be np_ 3 and np_ 2 noun 1 prep each other ?); e.g., are the NVidia block and the SRI block (directly) touching each other ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (*np-ulf-tree* 5 6 7) (*pp-ulf-tree* 8 9 10 11) ?)
             (((set-of 2 3) (1 4)) ?)) (0 :ulf-recur)

    ; Asking about a single block
    1 (be det 2 block 0)
       ; Historical
       2 (be det 2 block 1 prep np_ 3 conj np_ 3 adv-e 0 ?); e.g., was the NVidia block above the SRI block and the Texaco block previously ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3 4) (*pp-ulf-tree* 5 6 7 8 9 10 11) (*adv-ulf-tree* 12 13) ?)
             ((2 (1 3 4)) ?)) (0 :ulf-recur)
       2 (be det 2 block 1 prep 2 np-bw 3 adv-e 0 ?); e.g., was the NVidia block on [a red block]/[it]/[red blocks] on the first turn ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3 4) (*pp-ulf-tree* 5 6 7 8 9) (*adv-ulf-tree* 10 11) ?)
             ((2 (1 3 4)) ?)) (0 :ulf-recur)
       2 (be det 2 block adj adv-e 0 ?); e.g., was the NVidia block clear/red/visible before I moved it ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3 4) (lex-ulf! adj 5) (*adv-ulf-tree* 6 7) ?)
             ((2 (1 3 4)) ?)) (0 :ulf-recur)
       ; Standard
       2 (be det 2 block 1 prep np_ 3 conj np_ 3 ?); e.g., is the NVidia block above the SRI block and the Texaco block ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3 4) (*pp-ulf-tree* 5 6 7 8 9 10 11) ?)
             ((2 (1 3)) ?)) (0 :ulf-recur)
       2 (be det 2 block 1 prep 2 np-bw 3 ?); e.g., is the NVidia block on [a red block]/[it]/[red blocks] ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3 4) (*pp-ulf-tree* 5 6 7 8 9) ?)
             ((2 (1 3)) ?)) (0 :ulf-recur)
       2 (be det 2 block adj ?); e.g., is the NVidia block clear/red/visible ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2 3 4) (lex-ulf! adj 5) ?)
             ((2 (1 3)) ?)) (0 :ulf-recur)

    ; Asking about a pronoun
    1 (be pron 0)
       ; Historical
       2 (be pron 1 prep 2 np-bw 3 adv-e 0 ?); e.g., was it on top of [a red block]/[them]/[red blocks] previously ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2) (*pp-ulf-tree* 3 4 5 6 7) (*adv-ulf-tree* 8 9) ?)
             ((2 (1 3 4)) ?)) (0 :ulf-recur)
       2 (be pron adj adv-e 0 ?); e.g., was it clear/red/visible previously ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2) (lex-ulf! adj 3) (*adv-ulf-tree* 4 5) ?)
             ((2 (1 3 4)) ?)) (0 :ulf-recur)
       ; Standard
       2 (be pron 1 prep 2 np-bw 3 ?); e.g., is it on top of [a red block]/[them]/[red blocks] ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2) (*pp-ulf-tree* 3 4 5 6 7) ?)
             ((2 (1 3)) ?)) (0 :ulf-recur)
       2 (be pron adj ?); e.g., is it clear/red/visible ?
          3 (((lex-ulf! v 1) (*np-ulf-tree* 2) (lex-ulf! adj 3) ?)
             ((2 (1 3)) ?)) (0 :ulf-recur)

)) ; END *yn-question-ulf-tree*



(READRULES '*exist-question-ulf-tree* 
; ````````````````````````````````````````
; Parses existential there questions.
;
'(
    ; Historical
    1 (be there 3 noun 1 prep np_ 3 conj np_ 3 adv-e 0 ?); e.g., was there a red block above the SRI block and the NVidia block last turn ?
       2 (((lex-ulf! v 1) there.pro (*np-ulf-tree* 3 4 5 6 7 8 9 10 11) (*adv-ulf-tree* 12 13) ?) 
          ((2 (1 3 4)) ?)) (0 :ulf-recur)
    1 (be there adv-history 3 noun 1 prep 2 np-bw 3 ?); e.g., was there ever a red block on the NVidia block ?
       2 (((lex-ulf! v 1) there.pro (*adv-ulf-tree* 3) (*np-ulf-tree* 4 5 6 7 8 9 10))
          ((2 (1 3 4)) ?)) (0 :ulf-recur)
    1 (be there 3 noun 1 prep 2 np-bw 3 adv-e 0 ?); e.g., was there a red block on a red block previously ?
       2 (((lex-ulf! v 1) there.pro (*np-ulf-tree* 3 4 5 6 7 8 9) (*adv-ulf-tree* 10 11) ?) 
          ((2 (1 3 4)) ?)) (0 :ulf-recur)

    ; Standard
    1 (be there 3 noun 1 prep np_ 3 conj np_ 3 ?); e.g., is there a red block above the SRI block and the NVidia block ?
       2 (((lex-ulf! v 1) there.pro (*np-ulf-tree* 3 4 5 6 7 8 9 10 11) ?) 
          ((2 (1 3)) ?)) (0 :ulf-recur)
    1 (be there 3 noun 1 prep 2 np-bw 3 ?); e.g., is there a red block on [a blue block]/[it]
       2 (((lex-ulf! v 1) there.pro (*np-ulf-tree* 3 4 5 6 7 8 9) ?) 
          ((2 (1 3)) ?)) (0 :ulf-recur)

)) ; END *exist-question-ulf-tree*



(READRULES '*has-question-ulf-tree* 
; `````````````````````````````````````
; Parses has questions (typically indicates perfect aspect, i.e. "has been")
;
'( 
    1 (has 2 np-bw 3 adv_ verb-rel 2 np-bw 3 ?); e.g., has the SRI block ever touched the NVidia block ?
       2 (((*np-ulf-tree* 2 3 4) (*adv-ulf-tree* 5) (lex-ulf! v- 6) (*np-ulf-tree* 7 8 9) ?)
          ((1 (2 ((past perf) (3 4)))) ?)) (0 :ulf-recur)
    1 (has 2 np-bw 3 adv_ been 1 adj ?); e.g., has the SRI block ever been (totally) clear ?
       2 (((*np-ulf-tree* 2 3 4) (*adv-ulf-tree* 5) (lex-ulf! adj 8) ?) ((1 (2 ((past perf) (be.v 3)))) ?)) (0 :ulf-recur)
    1 (has 2 np-bw 3 adv_ been between 0 ?); e.g., has the SRI block ever been between the NVidia block and Twitter block ?
       2 (((*np-ulf-tree* 2 3 4) (*adv-ulf-tree* 5) (*pp-between-ulf-tree* 7 8) ?)
          ((1 (2 ((past perf) (be.v 3)))) ?)) (0 :ulf-recur)
    1 (has 2 np-bw 3 adv_ been 1 prep 2 np-bw 3 ?); e.g., has the SRI block ever been on the NVidia block ?
       2 (((*np-ulf-tree* 2 3 4) (*adv-ulf-tree* 5) (*pp-ulf-tree* 7 8 9 10 11) ?)
          ((1 (2 ((past perf) (be.v 3)))) ?)) (0 :ulf-recur)
    1 (has there adv_ been 2 np-bw 3 between 0 ?); e.g., has there ever been a block between the NVidia block and Twitter block ?
       2 ((there.pro (*adv-ulf-tree* 3) (*np-ulf-tree* 5 6 7 8 9) ?)
          ((2 ((past perf) (be.v 1 3))) ?)) (0 :ulf-recur)
    1 (has there adv_ been 2 np-bw 3 prep 2 np-bw 3 ?); e.g., has there ever been a block on the SRI block ?
       2 ((there.pro (*adv-ulf-tree* 3) (*np-ulf-tree* 5 6 7 8 9 10 11) ?)
          ((2 ((past perf) (be.v 1 3))) ?)) (0 :ulf-recur)

)) ; END *has-question-ulf-tree*


     
(READRULES '*modal-question-ulf-tree* 
; `````````````````````````````````````````
; Parses modal questions (e.g. can you see the NVidia block ?).
; NOTE: currently unsupported.
;
'(
    1 (Sorry\, you are not handling modal questions yet\.) (0 :out)

)) ; END *modal-question-ulf-tree*



(READRULES '*where-question-ulf-tree* 
; `````````````````````````````````````````
; Parses where questions.
;
'(
    ; Historical
    1 (where be det 2 noun adv-e 0 ?); e.g., where was the NVidia block previously ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3 4 5) (*adv-ulf-tree* 6 7) ?)
          ((sub 1 (3 (2 *h 4))) ?)) (0 :ulf-recur)
    1 (where be pron adv-e 0 ?); e.g., where was it before I moved it ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3) (*adv-ulf-tree* 4 5) ?)
          ((sub 1 (3 (2 *h 4))) ?)) (0 :ulf-recur)
    ; Standard
    1 (where be det 2 noun ?); e.g., where is the NVidia block ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3 4 5) ?)
          ((sub 1 (3 (2 *h))) ?)) (0 :ulf-recur)
    1 (where be pron ?); e.g., where is it ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3) ?)
          ((sub 1 (3 (2 *h))) ?)) (0 :ulf-recur)

    ; NOTE: does this make sense?
    1 (where be there 0); interpret like a y/n-question, i.e., drop the "where"
       2 (((*yn-question-ulf-tree* 2 3 4)) 1) (0 :ulf-recur)

)) ; END *where-question-ulf-tree*



(READRULES '*when-question-ulf-tree*
; ````````````````````````````````````
; Parses when questions.
;
'(
    1 (when do pron verb-rel 2 np-bw 3 ?); e.g., when did I move the Twitter block ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3) (lex-ulf! v- 4) (*np-ulf-tree* 5 6 7))
          ((sub 1 (2 3 (4 5 (adv-e *h)))) ?)) (0 :ulf-recur)
    1 (when be 2 np-bw 3 verb-rel ?); e.g., when was the Twitter block moved ?
       2 (((lex-ulf! wh-pred 1) (*np-ulf-tree* 3 4 5) (lex-ulf! v-pasv 6))
          ((sub 1 (2 (3 (adv-e *h)))) ?)) (0 :ulf-recur)
    1 (when be 2 np-bw 3 prep 2 np-bw 3 ?); e.g., when was the SRI block on_top_of the Twitter block ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3 4 5) (*pp-ulf-tree* 6 7 8 9))
          ((sub 1 (3 (2 4 (adv-e *h)))) ?)) (0 :ulf-recur)
    1 (when be 2 np-bw 3 adj ?); e.g., when was the SRI block clear ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3 4 5) (lex-ulf! adj 6))
          ((sub 1 (3 (2 4 (adv-e *h)))) ?)) (0 :ulf-recur)

)) ; END *when-question-ulf-tree*



(READRULES '*color-question-ulf-tree* 
; `````````````````````````````````````````
; Parses what color questions.
;
'(
    ; Historical
    1 (what color noun be 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what color block was to_the_left_of the SRI block previously ?
       2 (((lex-ulf! det 1) (lex-ulf! adj 2) (lex-ulf! noun 3) (lex-ulf! v 4) 
          (*pp-ulf-tree* 5 6 7 8 9) (*adv-ulf-tree* 10 11) ?) (((1 (2 3)) (4 5 6)) ?)) (0 :ulf-recur)
    1 (what color be np_ 0 adv-e 0 ?); e.g., what color was the Nvidia block initially ?
       2 (((lex-ulf! det 1) (lex-ulf! noun 2) (lex-ulf! v 3) (*np-ulf-tree* 4 5) (*adv-ulf-tree* 6 7) ?)
          ((sub ({of}.p (1 2)) (4 (3 *h 5))) ?)) (0 :ulf-recur)
    ; Standard
    1 (what color noun be 1 prep 2 np-bw 3 ?); e.g., what color block is to_the_left_of the SRI block ?
       2 (((lex-ulf! det 1) (lex-ulf! adj 2) (lex-ulf! noun 3) (lex-ulf! v 4) 
          (*pp-ulf-tree* 5 6 7 8 9) ?) (((1 (2 3)) (4 5)) ?)) (0 :ulf-recur)
    1 (what color be np_ 0 ?); e.g., what color is the Nvidia block ?
       2 (((lex-ulf! det 1) (lex-ulf! noun 2) (lex-ulf! v 3) (*np-ulf-tree* 4 5))
          ((sub ({of}.p (1 2)) (4 (3 *h))) ?)) (0 :ulf-recur)

)) ; END *color-question-ulf-tree*



(READRULES '*how-many-question-ulf-tree* 
; ````````````````````````````````````````````
; Parses how many questions.
;
'(
    ; Counting blocks satisfying some preposition (historical)
    1 (how many 1 noun be not prep 2 np-bw 3 conj 2 np-bw 3 adv-e 0 ?); e.g., how many blocks were not on red blocks or blue blocks previously ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10 11 12 13 14) (*adv-ulf-tree* 15 16) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 not 3 4)) ?)) (0 :ulf-recur)
    1 (how many 1 noun be 1 prep 2 np-bw 3 conj 2 np-bw 3 adv-e 0 ?); e.g., how many blocks were there above the SRI block and NVidia block initially ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10 11 12 13 14) (*adv-ulf-tree* 15 16) ?) 
          ((((nquan (how.mod-a many.a)) 1) (2 3 4)) ?)) (0 :ulf-recur)
    1 (how many 1 noun be not prep 2 np-bw 3 adv-e 0 ?); e.g., how many blocks were not on red blocks before ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10) (*adv-ulf-tree* 11 12) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 not 3 4)) ?)) (0 :ulf-recur)
    1 (how many 1 noun be 1 prep 2 np-bw 3 adv-e 0 ?); e.g., how many blocks were there on some red block during the previous turn ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10) (*adv-ulf-tree* 11 12) ?) 
          ((((nquan (how.mod-a many.a)) 1) (2 3 4)) ?)) (0 :ulf-recur)

    ; Counting blocks satisfying some property (historical)
    1 (how many 1 noun be there adv-e 0 ?); how many red blocks were there initially ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) there.pro (*adv-ulf-tree* 7 8) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 there.pro 3)) ?))  (0 :ulf-recur)
    1 (how many 1 noun 3 be not adj adv-e 0 ?); how many blocks (on the table) were not red before this turn ?
       2 (((*n1-ulf-tree* 3 4 5) (lex-ulf! v 6) (lex-ulf! adj 8) (*adv-ulf-tree* 9 10) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 (not.mod-a 3) 4)) ?)) (0 :ulf-recur)
    1 (how many 1 noun 3 be adj adv-e 0 ?); how many blocks (on the table) were red previously ?
       2 (((*n1-ulf-tree* 3 4 5) (lex-ulf! v 6) (lex-ulf! adj 7) (*adv-ulf-tree* 8 9) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 3 4)) ?)) (0 :ulf-recur)
    1 (how many be not adj adv-e 0 ?); how many were not red initially ?
       2 (((lex-ulf! noun blocks) (lex-ulf! v 3) (lex-ulf! adj 5) (*adv-ulf-tree* 6 7) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 (not.mod-a 3) 4)) ?)) (0 :ulf-recur)
    1 (how many be adj adv-e 0 ?); how many were red initially ?
       2 (((lex-ulf! noun blocks) (lex-ulf! v 3) (lex-ulf! adj 4) (*adv-ulf-tree* 5 6) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 3 4)) ?)) (0 :ulf-recur)

    ; Counting blocks satisfying some preposition
    1 (how many 1 noun be not prep 2 np-bw 3 conj 2 np-bw 3 ?); e.g., how many blocks are not on red blocks or blue blocks ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10 11 12 13 14) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 not 3)) ?)) (0 :ulf-recur)
    1 (how many 1 noun be 1 prep 2 np-bw 3 conj 2 np-bw 3 ?); e.g., how many blocks are (there) above the SRI block and NVidia block ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10 11 12 13 14) ?) 
          ((((nquan (how.mod-a many.a)) 1) (2 3)) ?)) (0 :ulf-recur)
    1 (how many 1 noun be not prep 2 np-bw 3 ?); e.g., how many blocks are not on red blocks ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 not 3)) ?)) (0 :ulf-recur)
    1 (how many 1 noun be 1 prep 2 np-bw 3 ?); e.g., how many blocks are (there) on some red block ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*pp-ulf-tree* 7 8 9 10) ?) 
          ((((nquan (how.mod-a many.a)) 1) (2 3)) ?)) (0 :ulf-recur)

    ; Counting blocks satisfying some property
    1 (how many 1 noun be there ?); how many red blocks are there ?
       2 (((*n1-ulf-tree* 3 4) (lex-ulf! v 5) there.pro ?)
          ((((nquan (how.mod-a many.a)) 1) (2 there.pro)) ?))  (0 :ulf-recur)
    1 (how many 1 noun 3 be not adj ?); how many blocks (on the table) are not red ?
       2 (((*n1-ulf-tree* 3 4 5) (lex-ulf! v 6) (lex-ulf! adj 8) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 (not.mod-a 3))) ?)) (0 :ulf-recur)
    1 (how many 1 noun 3 be adj ?); how many blocks (on the table) are red ?
       2 (((*n1-ulf-tree* 3 4 5) (lex-ulf! v 6) (lex-ulf! adj 7) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 3)) ?)) (0 :ulf-recur)
    1 (how many be not adj ?); how many are not red ?
       2 (((lex-ulf! noun blocks) (lex-ulf! v 3) (lex-ulf! adj 5) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 (not.mod-a 3))) ?)) (0 :ulf-recur)
    1 (how many be adj ?); how many are red ?
       2 (((lex-ulf! noun blocks) (lex-ulf! v 3) (lex-ulf! adj 4) ?)
          ((((nquan (how.mod-a many.a)) 1) (2 3)) ?)) (0 :ulf-recur)
    
)) ; END *how-many-question-ulf-tree*



(READRULES '*wh-question-ulf-tree* 
; ``````````````````````````````````````
; Parses wh- questions.
; NOTE: where, how many, and what color questions are parsed separately for organizational reasons.
;
'(
    ; Passive historical
    1 (wh_ 2 be adv_ verb-rel between 0 ?); e.g., what (block) was just placed between the SRI and NVidia blocks ?
       2 (((*np-ulf-tree* 1 2) (*adv-ulf-tree* 4) (lex-ulf! v-pasv 5) (*pp-between-ulf-tree* 6 7) ?) ((1 (2 (3 4))) ?)) (0 :ulf-recur)
    1 (wh_ 2 be adv_ verb-rel 1 prep 2 np-bw 3 ?); e.g., what (block) was just placed on the SRI block ?
       2 (((*np-ulf-tree* 1 2) (*adv-ulf-tree* 4) (lex-ulf! v-pasv 5) (*pp-ulf-tree* 6 7 8 9 10) ?) ((1 (2 (3 4))) ?)) (0 :ulf-recur)
    1 (wh_ 2 be adv_ verb-rel ?); e.g., what (block) was just moved ?
       2 (((*np-ulf-tree* 1 2) (*adv-ulf-tree* 4) (lex-ulf! v-pasv 5) ?) ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh_ 2 be verb-rel between 0 ?); e.g., what (block) was placed between the SRI and NVidia blocks ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v-pasv 4) (*pp-between-ulf-tree* 5 6) ?) ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh_ 2 be verb-rel 1 prep 2 np-bw 3 ?); e.g., what (block) was placed on the SRI block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v-pasv 4) (*pp-ulf-tree* 5 6 7 8 9) ?) ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh_ 2 be verb-rel ?); e.g., what (block) was moved ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v-pasv 4) ?) ((1 2) ?)) (0 :ulf-recur)
    1 (wh_ 2 be verb-rel between 0 adv-e 0 ?); e.g., what (block) was placed between the SRI and NVidia blocks two turns ago ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v-pasv 4) (*pp-between-ulf-tree* 5 6) (*adv-ulf-tree* 7 8) ?) ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh_ 2 be verb-rel 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what (block) was placed on the SRI block two turns ago ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v-pasv 4) (*pp-ulf-tree* 5 6 7 8 9) (*adv-ulf-tree* 10 11) ?) ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh_ 2 be verb-rel adv-e 0 ?); e.g., what (block) was moved two turns ago ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v-pasv 4) (*adv-ulf-tree* 5 6) ?) ((1 (2 3)) ?)) (0 :ulf-recur)

    ; + Not (historical)
    1 (wh-det 1 noun be not 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what red blocks were not on_top_of the NVidia block initially ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (*pp-ulf-tree* 6 7 8 9 10)
          (*adv-ulf-tree* 11 12) ?) ((1 (2 not 3 4)) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be not 1 adj adv-e 0 ?); e.g., what blocks were not (totally) clear during the previous turn ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (lex-ulf! adj 7) (*adv-ulf-tree* 8 9) ?)
          ((1 (2 not 3 4)) ?)) (0 :ulf-recur)
    ; + There (historical)
    1 (wh-det 1 noun be there 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what red blocks were there on blue blocks at the start ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (*pp-ulf-tree* 6 7 8 9 10)
          (*adv-ulf-tree* 11 12) ?) ((1 (2 3 4)) ?)) (0 :ulf-recur)
    ; Pronoun (historical)
    1 (wh-pron be 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what was next to the Texaco block previously ?
       2 ((what.pro (lex-ulf! v 2) (*pp-ulf-tree* 3 4 5 6 7) (*adv-ulf-tree* 8 9) ?) 
          ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh-pron be the sup-adj 2 adv-e 0 ?); e.g., what was the highest red block before I moved the SRI block ?
       2 (((lex-ulf! pro 1) (lex-ulf! v 2) the.d (*n1-ulf-tree* 4 5) (*adv-ulf-tree* 6 7) ?)
          ((1 (2 (= (the.d 4)) 5)) ?)) (0 :ulf-recur)
    1 (wh-pron be the 2 noun prep np_ 3 adv-e 0 ?); e.g., what was the block next_to the farthest blue block initially ?
       2 (((lex-ulf! pro 1) (lex-ulf! v 2) (*np-ulf-tree* 3 4 5 6 7 8) (*adv-ulf-tree* 9 10) ?)
          ((1 (2 (= 3) 4)) ?)) (0 :ulf-recur)
    ; Non-be verb (historical)
    1 (wh_ 2 verb-rel 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what block sat on the SRI block previously ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-ulf-tree* 4 5 6 7 8) (*adv-ulf-tree* 9 10))
          ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh_ 2 adv-history verb-rel 2 np-bw 3 ?); e.g., what block initially faced the SRI block ?
       2 (((*np-ulf-tree* 1 2) (*adv-ulf-tree* 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6 7))
          ((1 (2 (3 4))) ?)) (0 :ulf-recur)
    1 (wh_ 2 verb-rel 2 np-bw 3 adv-e 0 ?); e.g., what block faced the SRI block initially ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4 5 6) (*adv-ulf-tree* 7 8))
          ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh_ 2 verb-rel adv-e 0 ?); e.g., what changed since last turn ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*adv-ulf-tree* 4 5)) ((1 (2 3)) ?)) (0 :ulf-recur)
    ; Standard (historical)
    1 (wh-det 1 noun be 1 prep 2 np_ 3 adv-e 0 ?); e.g., what red blocks were above it initially ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (*pp-ulf-tree* 5 6 7 8 9)
          (*adv-ulf-tree* 10 11) ?) ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be 1 adj adv-e 0 ?); e.g., which blocks were (totally) clear initially ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (lex-ulf! adj 6) (*adv-ulf-tree* 7 8) ?) 
          ((1 (2 3 4)) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be 2 np-bw 3 adv-history prep ?); e.g., what/which block was the NVidia block previously on_top_of ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6 7)
          (lex-ulf! adv-e 8) (lex-ulf! prep 9) ?) ((sub 1 (3 (2 4 (5 *h)))) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be 2 np-bw 3 prep adv-e 0 ?); e.g., what/which block was the NVidia block on_top_of before I moved it ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6 7)
          (lex-ulf! prep 8) (*adv-ulf-tree* 9 10) ?) ((sub 1 (3 (2 (4 *h) 5))) ?)) (0 :ulf-recur)

    ; + Not
    1 (wh-det 1 noun be not 1 prep 2 np-bw 3 ?); e.g., what red blocks are not directly on_top_of the NVidia block ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4)
          (*pp-ulf-tree* 6 7 8 9 10) ?) ((1 (2 not 3)) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be not 1 adj ?); e.g., what blocks are not (totally) clear ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (lex-ulf! adj 7) ?)
          ((1 (2 not 3)) ?)) (0 :ulf-recur)
    ; + There
    1 (wh-det 1 noun be there 1 prep 2 np-bw 3 ?); e.g., what red blocks are there on blue blocks ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4)
          (*pp-ulf-tree* 6 7 8 9 10) ?) ((1 (2 3)) ?)) (0 :ulf-recur)
    ; Pronoun
    1 (wh-pron be 1 prep 2 np-bw 3 ?); e.g., what is next to the Texaco block ?
       2 ((what.pro (lex-ulf! v 2) (*pp-ulf-tree* 3 4 5 6 7) ?) 
          ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh-pron be the sup-adj 2 ?); e.g., what is the highest red block ?
       2 (((lex-ulf! pro 1) (lex-ulf! v 2) the.d (*n1-ulf-tree* 4 5) ?)
          ((1 (2 (= (the.d 4)))) ?)) (0 :ulf-recur)
    1 (wh-pron be the 2 noun prep np_ 3 ?); e.g., what is the block next_to the farthest blue block ?
       2 (((lex-ulf! pro 1) (lex-ulf! v 2) (*np-ulf-tree* 3 4 5 6 7 8) ?)
          ((1 (2 (= 3))) ?)) (0 :ulf-recur)
    ; Non-be verb
    1 (wh_ 2 do not verb-rel 1 prep 2 np-bw 3 ?); e.g., what block doesn't sit on the table ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (lex-ulf! v- 5) (*pp-ulf-tree* 6 7 8 9 10))
          ((1 (2 not (3 4))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do not verb-rel 2 np-bw 3 ?); e.g., what blocks don't touch the SRI block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (lex-ulf! v- 5) (*np-ulf-tree* 6 7 8))
          ((1 (2 not (3 4))) ?)) (0 :ulf-recur)
    1 (wh_ 2 verb-rel 1 prep 2 np-bw 3 ?); e.g., what block sits on the SRI block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-ulf-tree* 4 5 6 7 8))
          ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh_ 2 verb-rel 2 np-bw 3 ?); e.g., what block faces the SRI block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4 5 6))
          ((1 (2 3)) ?)) (0 :ulf-recur)
    ; Standard
    1 (wh-det 1 noun be 1 prep 2 np_ 3 ?); e.g., what red blocks are above it ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4)
          (*pp-ulf-tree* 5 6 7 8 9) ?) ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be 1 adj ?); e.g., which blocks are (totally) clear ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (lex-ulf! adj 6) ?) 
          ((1 (2 3)) ?)) (0 :ulf-recur)
    1 (wh-det 1 noun be 2 np-bw 3 prep ?); e.g., what/which block is the NVidia block on_top_of ?
       2 (((*np-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6 7)
          (lex-ulf! prep 8) ?) ((sub 1 (3 (2 (4 *h)))) ?)) (0 :ulf-recur)

)) ; END *wh-question-ulf-tree*



(READRULES '*ppwh-question-ulf-tree* 
; ````````````````````````````````````````
; Parses PP[wh] questions.
;
'(
    ; Historical
    1 (prep-history wh-det 2 be np_ 3 between 0 ?); e.g., during what turn was the NVidia block between a red block and a blue block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6) (*pp-between-ulf-tree* 7 8) ?)
          ((sub 1 (3 (2 4 (adv-e *h)))) ?)) (0 :ulf-recur)
    1 (prep-history wh-det 2 be np_ 3 prep np_ 3 ?); e.g., during what turn was the NVidia block on the SRI block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6) (*pp-ulf-tree* 7 8 9) ?)
          ((sub 1 (3 (2 4 (adv-e *h)))) ?)) (0 :ulf-recur)
    1 (prep wh-det 2 be np_ 4 adv-e 0 ?); e.g., on what object was the NVidia block previously ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6) (*adv-ulf-tree* 7 8) ?)
          ((sub 1 (3 (2 *h 4))) ?)) (0 :ulf-recur)
    
    ; Historical + do
    1 (prep-history wh-det 2 do pron verb-rel 2 np-bw 3 ?); e.g., during what turn did I move the SRI block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5) (lex-ulf! v- 6) (*np-ulf-tree* 7 8 9) ?)
          ((sub 1 (2 3 (4 5 (adv-e *h)))) ?)) (0 :ulf-recur)
    1 (prep wh-det 2 do pron adv_ verb-rel 2 np-bw 3 ?); e.g., between which blocks did I just move the SRI block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5) (*adv-ulf-tree* 6) (lex-ulf! v- 7) (*np-ulf-tree* 8 9 10) ?)
          ((sub 1 (2 3 (4 (5 6 *h)))) ?)) (0 :ulf-recur)
    1 (prep wh-det 2 do pron verb-rel 2 np-bw 3 ?); e.g., between which blocks did I move the SRI block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5) (lex-ulf! v- 6) (*np-ulf-tree* 7 8 9) ?)
          ((sub 1 (2 3 (4 5 *h))) ?)) (0 :ulf-recur)
    1 (prep wh-det 2 do pron adv_ verb-rel 2 np-bw 3 ?); e.g., on_to what block did I just move the SRI block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5) (*adv-ulf-tree* 6) (lex-ulf! v- 7) (*np-ulf-tree* 8 9 10) ?)
          ((sub 1 (2 3 (4 (5 6 *h)))) ?)) (0 :ulf-recur)
    1 (prep wh-det 2 do pron verb-rel 2 np-bw 3 ?); e.g., on_to what block did I move the SRI block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5) (lex-ulf! v- 6) (*np-ulf-tree* 7 8 9) ?)
          ((sub 1 (2 3 (4 5 *h))) ?)) (0 :ulf-recur)

    ; Standard
    1 (prep wh-det 2 be np_ 4 ?); e.g., on what object is the NVidia block ?
       2 (((*pp-ulf-tree* 1 2 3) (lex-ulf! v 4) (*np-ulf-tree* 5 6) ?) ((sub 1 (3 (2 *h))) ?)) (0 :ulf-recur)

    ; TODO: add further rules, e.g., for "On what blocks are there other blocks ?", or
    ; "On how many blocks is the Target block resting/placed/supported/positioned ?"
)) ; END *ppwh-question-ulf-tree*



(READRULES '*wh-do-question-ulf-tree*
; ````````````````````````````````````````
; Parses wh + do questions.
;
'(
    ; Premodifying adverb
    1 (where do pron adv_ verb-rel 2 np-bw 3 ?); e.g., where did I just move the NVidia block ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3) (*adv-ulf-tree* 4) (lex-ulf! v- 5)
          (*np-ulf-tree* 6 7 8) ?) ((sub 1 (2 3 (4 (5 6 (adv-a *h))))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron adv_ verb-rel between 0 ?); e.g., what (block) did I just put between the SRI block and NVidia block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (*adv-ulf-tree* 5) (lex-ulf! v- 6)
          (*pp-between-ulf-tree* 7 8) ?) ((sub 1 (2 3 (4 (5 *h 6)))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron adv_ verb-rel 1 prep 2 np-bw 3 ?); e.g., what (block) did I just put on the SRI block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (*adv-ulf-tree* 5) (lex-ulf! v- 6)
          (*pp-ulf-tree* 7 8 9 10 11) ?) ((sub 1 (2 3 (4 (5 *h 6)))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron adv_ verb-rel ?); e.g., what (block) did I just move ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (*adv-ulf-tree* 5) (lex-ulf! v- 6) ?)
          ((sub 1 (2 3 (4 (5 *h)))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron adv_ verb-rel between 0 ?); e.g., how many blocks did I just put between the SRI block and NVidia block ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (*adv-ulf-tree* 7) (lex-ulf! v- 8)
          (*pp-between-ulf-tree* 9 10) ?) ((sub (1 2) (3 4 (5 (6 *h 7)))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron adv_ verb-rel 1 prep 2 np-bw 3 ?); e.g., how many blocks did I just put on the SRI block ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (*adv-ulf-tree* 7) (lex-ulf! v- 8)
          (*pp-between-ulf-tree* 9 10 11 12 13) ?) ((sub (1 2) (3 4 (5 (6 *h 7)))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron adv_ verb-rel ?); e.g., how many blocks did I just move ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (*adv-ulf-tree* 7) (lex-ulf! v- 8) ?)
          ((sub (1 2) (3 4 (5 (6 *h)))) ?)) (0 :ulf-recur)

    ; Postmodifying adv-e
    1 (where do pron verb-rel 2 np-bw 3 adv-e 0 ?); e.g., where did I move the NVidia block two turns ago ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3) (lex-ulf! v- 4)
          (*np-ulf-tree* 5 6 7) (*adv-ulf-tree* 8 9) ?) ((sub 1 (2 3 (4 5 (adv-a *h) 6))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron verb-rel between 0 adv-e 0 ?); e.g., what (block) did I put between the SRI block and NVidia block two turns ago ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (lex-ulf! v- 5)
          (*pp-between-ulf-tree* 6 7) (*adv-ulf-tree* 8 9) ?) ((sub 1 (2 3 (4 *h 5 6))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron verb-rel 1 prep 2 np-bw 3 adv-e 0 ?); e.g., what (block) did I put on the SRI block two turns ago ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (lex-ulf! v- 5)
          (*pp-ulf-tree* 6 7 8 9 10) (*adv-ulf-tree* 11 12) ?) ((sub 1 (2 3 (4 *h 5 6))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron verb-rel adv-e 0 ?); e.g., what (block) did I move two turns ago ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (lex-ulf! v- 5) (*adv-ulf-tree* 6 7) ?)
          ((sub 1 (2 3 (4 *h 5))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron verb-rel between 0 adv-e 0 ?); e.g., how many blocks did I put between the SRI block and NVidia block two turns ago ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (lex-ulf! v- 7) (*pp-between-ulf-tree* 8 9)
          (*adv-ulf-tree* 10 11) ?) ((sub (1 2) (3 4 (5 *h 6 7))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron verb-rel 1 prep 2 np-bw 3 adv-e 0 ?); e.g., how many blocks did I put on the SRI block two turns ago ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (lex-ulf! v- 7) (*pp-ulf-tree* 8 9 10 11 12)
          (*adv-ulf-tree* 13 14) ?) ((sub (1 2) (3 4 (5 *h 6 7))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron verb-rel adv-e 0 ?); e.g., how many blocks did I move two turns ago ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (lex-ulf! v- 7) (*adv-ulf-tree* 8 9) ?)
          ((sub (1 2) (3 4 (5 *h 6))) ?)) (0 :ulf-recur)

    ; Standard
    1 (where do pron verb-rel 2 np-bw 3 ?); e.g., where did I move the NVidia block ?
       2 (((lex-ulf! wh-pred 1) (lex-ulf! v 2) (*np-ulf-tree* 3) (lex-ulf! v- 4)
          (*np-ulf-tree* 5 6 7) ?) ((sub 1 (2 3 (4 5 (adv-a *h)))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron verb-rel between 0 ?); e.g., what (block) did I put between the SRI block and NVidia block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (lex-ulf! v- 5)
          (*pp-between-ulf-tree* 6 7) ?) ((sub 1 (2 3 (4 *h 5))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron verb-rel 1 prep 2 np-bw 3 ?); e.g., what (block) did I put on the SRI block ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (lex-ulf! v- 5)
          (*pp-ulf-tree* 6 7 8 9 10) ?) ((sub 1 (2 3 (4 *h 5))) ?)) (0 :ulf-recur)
    1 (wh_ 2 do pron verb-rel ?); e.g., what (block) did I move ?
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 4) (lex-ulf! v- 5) ?)
          ((sub 1 (2 3 (4 *h))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron verb-rel between 0 ?); e.g., how many blocks did I put between the SRI block and NVidia block ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (lex-ulf! v- 7) (*pp-between-ulf-tree* 8 9) ?)
          ((sub (1 2) (3 4 (5 *h 6))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron verb-rel 1 prep 2 np-bw 3 ?); e.g., how many blocks did I put on the SRI block ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (lex-ulf! v- 7) (*pp-ulf-tree* 8 9 10 11 12) ?)
          ((sub (1 2) (3 4 (5 *h 6))) ?)) (0 :ulf-recur)
    1 (how many 1 noun do pron verb-rel ?); e.g., how many blocks did I move ?
       2 ((how_many.d (*n1-ulf-tree* 3 4) (lex-ulf! v 5) (*np-ulf-tree* 6) (lex-ulf! v- 7) ?)
          ((sub (1 2) (3 4 (5 *h))) ?)) (0 :ulf-recur)
    
)) ; END *wh-do-question-ulf-tree*



(READRULES '*do-question-ulf-tree* 
; ```````````````````````````````````````
; Parses do questions.
;
'(
    ; Negation (historical)
    1 (do np_ 2 not verb-rel 1 between 0 adv-e 0 ?); e.g., did anything not sit between the two red blocks initially ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 5) (*pp-between-ulf-tree* 7 8) (*adv-ulf-tree* 9 10) ?)
          ((1 2 (not (3 (adv-a 4) 5))) ?)) (0 :ulf-recur)
    1 (do np_ 2 not verb-rel 2 np-bw 3 adv-e 0 ?); e.g., did any block not support the NVidia block at the first turn ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 5) (*np-ulf-tree* 6 7 8) (*adv-ulf-tree* 9 10) ?)
          ((1 2 (not (3 4 5))) ?)) (0 :ulf-recur)
    1 (do np_ 2 not verb-rel prep 2 np-bw 3 adv-e 0 ?); e.g., did any block not sit on the red NVidia block on the turn before this ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 5) (*pp-ulf-tree* 6 7 8 9) (*adv-ulf-tree* 10 11) ?)
          ((1 2 (not (3 (adv-a 4) 5))) ?)) (0 :ulf-recur)

    ; Standard (historical)
    1 (do np_ 2 verb-rel 1 between 0 adv-e 0 ?); e.g., did anything sit between the two red blocks initially ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 4) (*pp-between-ulf-tree* 6 7) (*adv-ulf-tree* 8 9) ?)
          ((1 2 (3 (adv-a 4) 5)) ?)) (0 :ulf-recur)
    1 (do np_ 2 verb-rel 2 np-bw 3 adv-e 0 ?); e.g., did any block support the NVidia block at the first turn ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 4) (*np-ulf-tree* 5 6 7) (*adv-ulf-tree* 8 9) ?)
          ((1 2 (3 4 5)) ?)) (0 :ulf-recur)
    1 (do np_ 2 verb-rel prep 2 np-bw 3 adv-e 0 ?); e.g., did any block sit on the red NVidia block on the turn before this ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 4) (*pp-ulf-tree* 5 6 7 8) (*adv-ulf-tree* 9 10) ?)
          ((1 2 (3 (adv-a 4) 5)) ?)) (0 :ulf-recur)

    ; Negated
    1 (do np_ 2 not verb-rel 1 between 0 ?); e.g., does anything not sit between the two red blocks ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 5) (*pp-between-ulf-tree* 7 8) ?)
          ((1 2 (not (3 (adv-a 4)))) ?))
    1 (do np_ 2 not verb-rel 2 np-bw 3 ?); e.g., does any block not touch the NVidia block ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 5) (*np-ulf-tree* 6 7 8) ?)
          ((1 2 (not (3 4))) ?)) (0 :ulf-recur)
    1 (do np_ 2 not verb-rel prep 2 np-bw 3 ?); e.g., does any block not sit on the red NVidia block ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 5) (*pp-ulf-tree* 6 7 8 9) ?)
          ((1 2 (not (3 (adv-a 4)))) ?)) (0 :ulf-recur)

    ; Historical premodifier
    1 (do np_ 2 adv-history verb-rel 1 between 0 ?); e.g., did anything recently sit between the two red blocks ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! adv-e 4) (lex-ulf! v- 5) (*pp-between-ulf-tree* 7 8) ?)
          ((1 2 (3 (4 (adv-a 5)))) ?))
    1 (do np_ 2 adv-history verb-rel 2 np-bw 3 ?); e.g., did any block initially touch the NVidia block ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! adv-e 4) (lex-ulf! v- 5) (*np-ulf-tree* 6 7 8) ?)
          ((1 2 (3 (4 5))) ?)) (0 :ulf-recur)
    1 (do np_ 2 adv-history verb-rel prep 2 np-bw 3 ?); e.g., did any block previously sit on the red NVidia block ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! adv-e 4) (lex-ulf! v- 5) (*pp-ulf-tree* 6 7 8 9) ?)
          ((1 2 (3 (4 (adv-a 5)))) ?)) (0 :ulf-recur)

    ; Standard
    1 (do np_ 2 verb-rel 1 between 0 ?); e.g., does anything sit between the two red blocks ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 4) (*pp-between-ulf-tree* 6 7) ?)
          ((1 2 (3 (adv-a 4))) ?)) (0 :ulf-recur)
    1 (do np_ 2 verb-rel 2 np-bw 3 ?); e.g., does any block support the NVidia block ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 4) (*np-ulf-tree* 5 6 7) ?)
          ((1 2 (3 4)) ?)) (0 :ulf-recur)
    1 (do np_ 2 verb-rel prep 2 np-bw 3 ?); e.g., does any block sit on the red NVidia block ?
       2 (((lex-ulf! v 1) (*np-ulf-tree* 2 3) (lex-ulf! v- 4) (*pp-ulf-tree* 5 6 7 8) ?)
          ((1 2 (3 (adv-a 4))) ?)) (0 :ulf-recur)

)) ; END *do-question-ulf-tree*



(READRULES '*spatial-sentence-ulf-tree* 
; ```````````````````````````````````````````
; Parses declarative questions.
;
'(
    ; Historical
    1 (np_ 3 be 1 between 0 adv-e 0); a red block was between the NVidia and Mercedes blocks previously
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-between-ulf-tree* 5 6) (*adv-ulf-tree* 7 8))
          (1 (2 3 4))) (0 :ulf-recur)
    1 (np_ 3 be 1 prep 0 adv-e 0); a red block was on the NVidia block before I moved it
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-ulf-tree* 5 6) (*adv-ulf-tree* 7 8))
          (1 (2 3 4))) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 np_ 0 adv-e 0); a red block supported the NVidia block before
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 5 6) (*adv-ulf-tree* 7 8))
          (1 (2 3 4))) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 between 0 adv-e 0); a red block sat between the NVidia and Mercedes blocks initially
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-between-ulf-tree* 5 6) (*adv-ulf-tree* 7 8))
          (1 (2 3 4))) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 prep 0 adv-e 0); a red block sat on the NVidia block previously
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-ulf-tree* 5 6) (*adv-ulf-tree* 7 8))
          (1 (2 3 4))) (0 :ulf-recur)

    ; Standard
    1 (np_ 3 be 1 between 0); a red block is between the NVidia and Mercedes blocks
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-between-ulf-tree* 5 6))
          (1 (2 3))) (0 :ulf-recur)
    1 (np_ 3 be 1 prep 0); a red block is on the NVidia block
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-ulf-tree* 5 6))
          (1 (2 3))) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 np_ 0); a red block supports the NVidia block
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*np-ulf-tree* 5 6))
          (1 (2 3))) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 between 0); a red block sits between the NVidia and Mercedes blocks
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-between-ulf-tree* 5 6))
          (1 (2 3))) (0 :ulf-recur)
    1 (np_ 3 verb-rel 1 prep 0); a red block sits on the NVidia block
       2 (((*np-ulf-tree* 1 2) (lex-ulf! v 3) (*pp-ulf-tree* 5 6))
          (1 (2 3))) (0 :ulf-recur)

)) ; END *spatial-sentence-ulf-tree*



; TO BE CHECKED FOR APPROPRIATENESS/ACCURACY/COMPLETENESS
(READRULES '*fallback-spatial-question-ulf-tree*
; ````````````````````````````````````````````````````
; These rules should be accessed as last resort by *spatial-question-ulf-tree*
; For the most part, these rules just allow for ignoring some words here and
; there, but there are also some reformulations (e,g., "support" relations)
; NOTE: These don't check for potential historical questions currently
;
'(
    1 (4 where 2 det 2 block 2)
       2 (((*wh-question-ulf-tree* where is 4 5 6 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (4 where 2 pron 2)
       2 (((*wh-question-ulf-tree* where is 4 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (4 where be there 0)
       2 (((*fallback-spatial-question-ulf-tree* 3 4 5)) 1) (0 :ulf-recur)
    1 (4 wh-det 2 noun be 2 prep 2 noun 2 ?);
       2 (((*wh-question-ulf-tree* 2 3 4 5 7 8 9 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-det 1 color be det 1 block 2)
       2 (((*wh-question-ulf-tree* 2 4 5 6 7 8 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-det 1 color 1 block be 1 prep 3 noun 2)
       2 (((*wh-question-ulf-tree* what color block is 8 9 10 11 ?)) 
          (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-pron be 2 sup-adj 2 noun 2)
       2 (((*wh-question-ulf-tree* 2 3 the 5 6 7 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-det 2 noun be the sup-adj 2); e.g., which red block is the highest up ?
       2 (((*wh-question-ulf-tree* which is the 7 3 4 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (8 sup-adj 2 noun 5); desperation rule for a superlative
       2 (((*wh-question-ulf-tree* which is the 2 3 4 ?)) (poss-ques 1)) (0 :ulf-recur) 
    1 (4 wh-pron be the 2 noun 1 be 1 prep 3 det 2 noun 5)
       2 (((*wh-question-ulf-tree* what is 4 5 6 that is 9 10 11 12 13 14 ?))
       (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-det 2 noun be 1 prep 3 det 2 noun 5)
       2 (((*wh-question-ulf-tree* which 3 4 is 6 7 8 9 10 11 ?))
          (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-det 2 noun be supporting det 2 noun 2); transform to on-relation
       2 (((*wh-question-ulf-tree* on 2 3 4 is 7 8 9 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (4 wh-det 2 noun be 1 supported by det 2 noun 2); transform to on-relation
       2 (((*wh-question-ulf-tree* 2 3 4 5 on 9 10 11 ?)) (poss-ques 1)) (0 :ulf-recur) 
    1 (2 be 1 det 2 noun 2 prep 3 det 3 noun 2)
       2 (((*yn-question-ulf-tree* 2 4 5 6 8 9 10 11 12 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (2 be pron 2 prep 3 det 3 noun 2) 
       2 (((*yn-question-ulf-tree* 2 3 5 6 7 8 9 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (2 be 1 det 2 noun 2 prep 3 pron 2)
       2 (((*yn-question-ulf-tree* 2 4 5 6 8 9 10 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (2 be det 2 noun 1 adj 2)
       2 (((*yn-question-ulf-tree* 2 3 4 5 7 ?)) (poss-ques 1)) (0 :ulf-recur)
    1 (0 det 2 block and 2 block 0); e.g., are the SRI block and NVidia block touching ?
       2 (((*yn-question-ulf-tree* 1 2 3 4 5 2 6 7 8)) (poss-ques 1)) (0 :ulf-recur)
    ; More can/should be added
    1 (0 det 2 block 0)
       2 (I am asking about some 3 4 \, but you didn\'t catch what it was\.) (0 :out)
    1 (0 det table 0)
       2 (I referred to the table\, but you didn\'t catch what I said\.) (0 :out)
    1 (0 pron 0)
       2 (You didn\'t catch what I am referring to\.) (0 :out)
    ; variants of begging-off responses should be added, with non-zero latency,
    ; so that the user will see a variety of such responses
 ))  
 

;  ; borrowed stuff, for reference:
;   2 *yn-question-ulf-tree* (0 :subtree)
;  1 (modal 0)      ; e.g., "Can you see the NVidia block ?
;   2 *modal-question-ulf-tree* (0 :subtree)
;  1 (wh_ 0)
;   2 *wh-question-ulf-tree* (0 :subtree)
;  1 (prep 2 wh_ 0) ; e.g., "On top of which block is the NVidia block ?"
;   2 *ppwh-question-ulf-tree* (0 :subtree)
