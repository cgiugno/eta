(MAPC 'ATTACHFEAT
  '()
)

(READRULES '*reaction-to-statement*
; Here we match any non-question statements which the user might ask, and respond to them appropriately.
; If some topic has enough possible gist clauses where it gets messy to put them all here, we can branch off
; to a subtree defined in the rule file for that subtopic (e.g. with *general-reaction*).
; NOTE: questions in declarative form might be tricky, and seem to happen frequently enough in the transcripts.
; Will have to figure out whether to make those questions beforehand through some preprocessing step (I think
; this makes more sense) or deal with them here somehow.
'(
  1 (I am sorry that your daughter couldn\'t come today \.)
    2 (That\'s okay\. We\'ll have to discuss it with her some other time\.) (0 :out)
  ; Chemotherapy
  1 (I think you need chemotherapy \.)
    ;; 2 *discuss-chemotherapy* (0 :schema)
    2 (*have-subdialogue* ((What would be the side effects of chemotherapy ?)
                           ((What are the side effects of chemotherapy ?)))) (100 :schema+args)
  1 (I do not think you need chemotherapy \.)
    2 (Ah\, okay\.) (0 :out)
  ; Medicine
  1 (I think you should take med-narcotic \.)
    2 (*have-subdialogue* ((Could you tell me about some of the side effects of that ?)
                           ((Can you tell me about the side effects ?)))) (100 :schema+args)
  1 (0)
    2 *general-reaction* (0 :subtree)
))



