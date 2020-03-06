(MAPC 'ATTACHFEAT
  '()
)

(READRULES '*reaction-to-question*
; Here we match any important questions which may branch off into a sub-dialogue, i.e. initiate a sub-schema.
; A separate tree is used for matching any less relevant questions, which prompt Eta to give an answer but then
; return to the central track of the conversation.
;
; General format of the below rules:
; 1 (pattern)
;   2 (*have-subdialogue* ((Answer to output \.)
;                          ((Gist clause one \.) (Gist clause two \.)))) (100 :schema+args)
'(
  ; Questions about pain
  1 (Can you tell me about your pain ?)
    2 (*have-subdialogue* ((The pain was pretty much under control for a while\, but in the past week it has been more difficult.
                            It used to be in my back and left side of my chest\, but now it\'s in my shoulder blade too\, and on
                            the other side from where it started\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
    2 (*have-subdialogue* ((The pain doesn\'t really go into other areas\. It\'s dull and constant\, and aches a lot\. It usually
                            hurts to take deep breathes\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
    2 (I did have some pain during swallowing after radiation as well\, but that\'s improved a bit\.) (0 :out)
  1 (How do you rate your pain ?)
    2 (*have-subdialogue* ((The pain is about a seven out of ten\. With medication\, it goes down to about a five\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
  1 (Where is the pain located ?)
    2 (*have-subdialogue* ((The pain is primarily in the left side of my chest\, and in the middle of my back\. Recently\,
                            it also moved to the right side of my chest\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
  1 (Does it hurt to 2 ?)
    2 (*have-subdialogue* ((It hurts whenever I take a deep breath\. It used to hurt to swallow during radiation\, but that
                            isn\'t as bad now\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
  ; Questions about how you got your diagnosis
  1 (How did you get your diagnosis ?)
    2 (*have-subdialogue* ((Well\, I first went to my regular doctor\, but he didn\'t find anything and just thought I had a bad
                            cold\. But after the pain and cough got really bad\, I went to the ER\. They thought it was just pneumonia
                            and gave me antibiotics and Lortab for pain\. When they told me how much I weighed\, I realized that I
                            had lost fifteen pounds over the course of the past six months\. A couple of days ago after I went to the
                            ER\, I made appointments to go for a bunch of tests\. That took a couple of weeks\. I had to get a bone scan\,
                            and I was sent to the lung doctor for a biopsy\. After all that\, the lung doctor told me I had lung cancer\.)
                           ((I got my diagnosis after visiting a lung doctor \.)))) (100 :schema+args)
  ; Questions about radiation treatment
  1 (Did you get radiation treatment ?)
    2 (*have-subdialogue* ((I did get radiation treatment\, for about five weeks\. I finished about six weeks ago\.)
                           ((I had radiation treatment for five weeks \.)))) (100 :schema+args)
  1 (Did you get any hair loss or redness during radiation treatment ?)
    2 (*have-subdialogue* ((I had a little bit of hair loss and redness at the site\, but it\'s mostly gone now\.)
                           ((I had some hair loss and redness at the site of radiation \.)))) (100 :schema+args)
  ; Questions about anyone here with you
  1 (Is anyone here with you ?)
    2 (*have-subdialogue* ((I\'m staying with my daughter now\. She took the day off to come in today\,
                            but someone called in sick where she works\. So\, she had to go in\. She
                            really wanted to be here\.)
                           ((I am here alone \.)))) (100 :schema+args)
  ; Questions about chemotherapy
  1 (Did your doctor mention chemotherapy ?)
    2 (*have-subdialogue* ((My doctor mentioned something about chemotherapy\, but said that I should wait to see how things go
                            after the radiation is done\. Why? Do you think I need chemotherapy?)
                           ((Do I need chemotherapy ?)))) (100 :schema+args)
  ; Questions about sleep
  1 (Have you been sleeping okay ?)
    2 (*have-subdialogue* ((I\'ve been having a bit of trouble\. I keep waking up at night\. Most nights I have to take my pain medication
                            before falling back to sleep again\.)
                           ((I have not been sleeping well \.)))) (100 :schema+args)
  ; Questions about energy
  1 (How is your energy ?)
    2 (*have-subdialogue* ((I\'ve been fatigued most days for the past month\. If I\'m just doing something normal\, I\'m fine\, but I don\'t
                            have as much energy as I used to\.)
                           ((I have been fatigued \.)))) (100 :schema+args)
  1 (Have you had trouble concentrating ?)
    2 (*have-subdialogue* ((Only sometimes\.)
                           ((I have had trouble concentrating \.)))) (100 :schema+args)
  
  ; Questions about weight loss
  1 (Have you lost weight ?)
    2 (*have-subdialogue* ((Yes\, I have lost weight\.)
                           ((I\’ve gained some weight\.)))) (100 :schema+args)

  ; Questions about substance use
  1 (Do you drink alcohol ?)
    2 (*have-subdialogue* ((I might have a few drinks a couple of times a week\.)
                           ((Not all the time \.)))) (100 :schema+args)
  1 (How often do you drink ?)
    2 (*have-subdialogue* ((I\’ve cut down since I have gotten ill. I don\’t think I have a drinking problem now \.)
                           ((I just don\’t have the taste for alcohol since I have gotten sick \.)))) (100 :schema+args)
  1 (Do you do drugs ?)
    2 (*have-subdialogue* ((I do not do drugs \.)
                           ((I tried some marijuana when I was young\, but I stopped in my twenties \.)))) (100 :schema+args)

  ; Questions about mental health
  1 (Are you depressed ?)
    2 (*have-subdialogue* ((I try not to think about dying.\.)
                           ((I try to keep carrying on\, but sometimes I do feel down \.)))) (100 :schema+args)
  1 (Have you thought of suicide ?)
    2 (*have-subdialogue* ((I never think about hurting myself\.)
                           ((I try not to be worried or depressed\. I don't think about harming myself\.)))) (100 :schema+args)

  ; Questions about medicine
  1 (Do you have allergies to any medicine ?)
    2 (*have-subdialogue* ((No\, you don\'t believe so\.)
                           ((I don\'t have allergies to any medicine \.)))) (100 :schema+args)
  1 (What medicine are you taking ?)
    2 (*have-subdialogue* ((I\'m just taking the Lortab for pain right now\.)
                           ((I am only taking Lortab to treat my pain \.)))) (100 :schema+args)
  1 (Are you taking pain-med ?)
    2 (*have-subdialogue* ((I\'m just taking the Lortab for pain right now\.)
                           ((I am only taking Lortab to treat my pain \.)))) (100 :schema+args)
  1 (Are you taking pain-med-other ?)
    2 (*have-subdialogue* ((No\, I\'m not taking any of those\. Just the Lortab\.)
                           ((I am only taking Lortab to treat my pain \.)))) (100 :schema+args)
  1 (Are you taking blood-pressure-med ?)
    2 (*have-subdialogue* ((I\'m taking Cozar for blood pressure\.)
                           ((I am taking Cozar to help with blood pressure \.)))) (100 :schema+args)
  1 (Is the pain medication working at all ?)
    2 (*have-subdialogue* ((A little bit\, but not much\.)
                           ((Why isn\'t the pain medication working anymore ?)))) (100 :schema+args)
  1 (Is the pain medication working ?)
    2 (*have-subdialogue* ((No\, it\'s not really working anymore\. So I\'ve been taking the pills
                            every three hours instead of every four\.)
                           ((Why isn\'t the pain medication working anymore ?)))) (100 :schema+args)

  ; The following two will need modification
  1 (How are you on medicine ?)
    2 (*have-subdialogue* ((I need a refill \.)
                           ((I would like a refill of medicine \.)))) (100 :schema+args)
  1 (What is your prognosis ?)
    2 (*have-subdialogue* ((Can you tell me what kind of time we\'re looking at ?)
                           ((What is my prognosis ?)))) (100 :schema+args)

  1 (0)
    2 *reaction-to-question-minor* (0 :subtree)
))


(READRULES '*reaction-to-question-minor*
; Here we match any question gist clauses which are considered "minor", i.e. off-topic or otherwise
; not expected to branch off into a further sub-dialogue.
'(
  ; Questions about daughter
  1 (Where does your daughter work ?)
    2 (She works as a school nurse in the county school system\.) (0 :out)
  1 (How old is your daughter ?)
    2 (She\'s thirty four\. Turning thirty five in a few months\.) (0 :out)
  ; Questions about family (TODO)
))
