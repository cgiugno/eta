(mapc 'attachfeat
  '()
)

(READRULES '*reaction-to-question*
'(
  1 (how are you on medicine ?)
    2 *medicine-schema* (100 :schema)
  1 (what is your prognosis ?)
    2 *prognosis-schema* (100 :schema)

))
