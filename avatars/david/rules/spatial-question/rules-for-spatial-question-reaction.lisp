(MAPC 'ATTACHFEAT
'(
  (history-question was were did has)
  (recent-times previous current preceding last)
  (prep-history-at at on in during)
))

(READRULES '*reaction-to-spatial-question-input*
'(
  ; If a historical question was asked, invoke historical question schema
  ; UNLESS the historical question involves the previous turn, which the
  ; BW specialist system is capable of answering using "high-fidelity visual memory"
  1 (0 history-question 0)
    2 (0 just verb 0)
      3 *reactions-to-spatial-question* (0 :schema+ulf)
    2 (0 prep-history-at 1 recent-times noun-history 0)
      3 *reactions-to-spatial-question* (0 :schema+ulf)
    2 (0)
      3 *reactions-to-historical-question* (0 :schema+ulf)
  ; TODO: spatial explanation requests
  ;; 1 (spatial-explanation-request 0)
  ;;   2 *reactions-to-spatial-explanation-request* (0 :schema+ulf)
  ; Otherwise, invoke basic spatial question QA schema
  1 (0)
    2 *reactions-to-spatial-question* (0 :schema+ulf)
))