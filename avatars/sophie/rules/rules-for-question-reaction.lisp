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
;                          ((Gist clause one \.) (Gist clause two \.)))) (0 :schema+args)
'(
  ; Questions about pain
  1 (Can you tell me about your pain ?)
    2 (*have-subdialogue* ((The pain was pretty much under control for a while\, but in the past week it has been more difficult.
                            It used to be in my back and left side of my chest\, but now it\'s in my shoulder blade too\, and on
                            the other side from where it started\.)
                           ((My pain has recently been getting worse \.)))) (3 :schema+args)
    2 (*have-subdialogue* ((The pain doesn\'t really go into other areas\. It\'s dull and constant\, and aches a lot\. It usually
                            hurts to take deep breathes\.)
                           ((My pain has recently been getting worse \.)))) (3 :schema+args)
    2 (I did have some pain during swallowing after radiation as well\, but that\'s improved a bit\.) (0 :out)
  1 (How do you rate your pain ?)
    2 (*have-subdialogue* ((The pain is about a seven out of ten\. With medication\, it goes down to about a five\.)
                           ((My pain has recently been getting worse \.)))) (0 :schema+args)
  1 (Where is the pain located ?)
    2 (*have-subdialogue* ((The pain is primarily in the left side of my chest\, and in the middle of my back\. Recently\,
                            it also moved to the right side of my chest\.)
                           ((My pain has recently been getting worse \.)))) (0 :schema+args)
  1 (Does it hurt to 2 ?)
    2 (*have-subdialogue* ((It hurts whenever I take a deep breath\. It used to hurt to swallow during radiation\, but that
                            isn\'t as bad now\.)
                           ((My pain has recently been getting worse \.)))) (0 :schema+args)
  1 (Did your pain come back ?)
    2 (*have-subdialogue* ((My pain came back a couple weeks after I finished radiation\. It\'s been getting worse recently\.)
                           ((My pain came back a couple weeks after I finished radiation \.)))) (0 :schema+args)
  ; Questions about how you got your diagnosis
  1 (How did you get your diagnosis ?)
    2 (*have-subdialogue* ((Well\, I first went to my regular doctor\, but he didn\'t find anything and just thought I had a bad
                            cold\. But after the pain and cough got really bad\, I went to the ER\. They thought it was just pneumonia
                            and gave me antibiotics and Lortab for pain\. When they told me how much I weighed\, I realized that I
                            had lost fifteen pounds over the course of the past six months\. A couple of days ago after I went to the
                            ER\, I made appointments to go for a bunch of tests\. That took a couple of weeks\. I had to get a bone scan\,
                            and I was sent to the lung doctor for a biopsy\. After all that\, the lung doctor told me I had lung cancer\.)
                           ((I got my diagnosis after visiting a lung doctor \.)))) (0 :schema+args)
  1 (What symptoms do you have ?)
    2 (*have-subdialogue* ((I have constipation\, though I\'ve had that for a year\. My appetite is worse\. I don\'t eat much anymore\.)
                           ((I have constipation \.) (My appetite is worse \.)))) (0 :schema+args)
  1 (Have you changed weight ?)
    2 (*have-subdialogue* ((I have lost some weight \.)
                           ((I have lost weight \.)))) (0 :schema+args)
  1 (How much weight have you lost ?)
    2 (*have-subdialogue* ((I\'ve lost about twenty five pounds since they started\. I gained seven back in radiation\, but I\'m still down a bit\.)
                           ((I have lost twenty five pounds \.) (I gained some weight back during radiation \.)))) (0 :schema+args)
  1 (Have you changed appetite ?)
    2 (*have-subdialogue* ((I have lost my appetite a bit\. The food looks good\, but I can only take a couple bites and then I\'m done\.)
                           ((I have lost my appetite \.)))) (0 :schema+args)
  1 (Do you have the symptom of 2 ?)
    2 (*have-subdialogue* ((No\, I haven\'t had problems with that \.)
                           ((I do not have the symptom you mentioned \.)))) (0 :schema+args)
  1 (Do you know what the tests say ?)
    2 (*have-subdialogue* ((I don\'t really understand the test results\. Can you explain them?)
                           ((I do not understand the test results \.) (What do my test results mean ?)))) (0 :schema+args)
  ; Questions about radiation treatment
  1 (Did you get radiation treatment ?)
    2 (*have-subdialogue* ((I did get radiation treatment\, for about five weeks\. I finished about six weeks ago\.)
                           ((I had radiation treatment for five weeks \.)))) (0 :schema+args)
  1 (Did you get any hair loss or redness during radiation treatment ?)
    2 (*have-subdialogue* ((I had a little bit of hair loss and redness at the site\, but it\'s mostly gone now\.)
                           ((I had some hair loss and redness at the site of radiation \.)))) (0 :schema+args)
  1 (Did the pain respond to radiation treatment ?)
    2 (*have-subdialogue* ((I was feeling a little better after radiation\, for a few weeks\.)
                           ((I was feeling a little better after radiation \.)))) (0 :schema+args)
  ; Questions about appointment
  1 (Did you drive here ?)
    2 (*have-subdialogue* ((I drove here today\. I\'ve just been taking my time with it\.)
                           ((I drove here today \.)))) (0 :schema+args)
  1 (Is anyone here with you ?)
    2 (*have-subdialogue* ((I\'m staying with my daughter now\. She took the day off to come in today\,
                            but someone called in sick where she works\. So\, she had to go in\. She
                            really wanted to be here\.)
                           ((I am here alone \.)))) (0 :schema+args)
  ; Questions about chemotherapy
  1 (Did your doctor mention chemotherapy ?)
    2 (*have-subdialogue* ((My doctor mentioned something about chemotherapy\, but said that I should wait to see how things go
                            after the radiation is done\. Why? Do you think I need chemotherapy?)
                           ((Do I need chemotherapy ?)))) (0 :schema+args)
  ; Questions about sleep
  1 (Have you been sleeping okay ?)
    2 (*have-subdialogue* ((I\'ve been having a bit of trouble\. I keep waking up at night\. Most nights I have to take my pain medication
                            before falling back to sleep again\.)
                           ((I have not been sleeping well \.)))) (0 :schema+args)
  1 (How often are you waking up at night ?)
    2 (*have-subdialogue* ((I haven\'t really been keeping track\. Maybe about four or five times in a night\.)
                           ((I have been waking up about four or five times a night \.)))) (0 :schema+args)
  1 (Do you sleep during the day ?)
    2 (*have-subdialogue* ((I usually take a nap during the day\. About one hour to two hours \.)
                           ((I usually take a nap during the day \.)))) (0 :schema+args)
  1 (What is on your mind when you try to sleep ?)
    2 (*have-subdialogue* ((When I actually sleep\, I don\'t really have anything on my mind\. When I have trouble sleeping I usually can\'t think
                           of anything except for the pain\.)
                           ((The pain is on my mind when I try to sleep \.)))) (0 :schema+args)
  ; Questions about energy
  1 (How is your energy ?)
    2 (*have-subdialogue* ((I\'ve been fatigued most days for the past month\. If I\'m just doing something normal\, I\'m fine\, but I don\'t
                            have as much energy as I used to\.)
                           ((I have been fatigued \.)))) (0 :schema+args)
  1 (Have you had trouble concentrating ?)
    2 (*have-subdialogue* ((Only sometimes\.)
                           ((I have had trouble concentrating \.)))) (0 :schema+args)
  1 (how is your mental health ?)
    2 (*have-subdialogue* ((Well\, I do try to keep carrying on\, but sometimes I just feel down\.) 
                           ((I feel mildly depressed \.)))) (0 :schema+args)

  ; Questions about medicine
  1 (Do you have allergies to any medicine ?)
    2 (*have-subdialogue* ((No\, you don\'t believe so\.)
                           ((I don\'t have allergies to any medicine \.)))) (0 :schema+args)
  1 (What medicine are you taking ?)
    2 (*have-subdialogue* ((I\'m just taking the Lortab for pain right now\.)
                           ((I am only taking Lortab to treat my pain \.)))) (0 :schema+args)
  1 (Are you taking pain-med ?)
    2 (*have-subdialogue* ((I think so\. I\'m taking Lortab for pain right now\.)
                           ((I am only taking Lortab to treat my pain \.)))) (0 :schema+args)
  1 (How often are you taking medication ?)
    2 (*have-subdialogue* ((I\'m taking two pills of Lortab every three hours now for the past couple of weeks\,
                            since this pain has come back.)
                           ((I am taking two pills of Lortab every three hours \.)))) (0 :schema+args)
  1 (Does taking medication more frequently help ?)
    2 (*have-subdialogue* ((Taking the Lortab more often seems to help a little\. I\'m already taking it every three hours\.)
                           ((Taking the Lortab more frequently helps \.) (I am taking Lortab every three hours \.)))) (0 :schema+args)
  1 (Are you taking pain-med-other ?)
    2 (*have-subdialogue* ((No\, I\'m not taking any of those\. Just the Lortab\.)
                           ((I am only taking Lortab to treat my pain \.)))) (0 :schema+args)
  1 (Are you taking blood-pressure-med ?)
    2 (*have-subdialogue* ((I\'ve been taking blood pressure medicine for about ten years\.)
                           ((I am taking blood pressure medicine \.)))) (0 :schema+args)
  1 (Are you taking med-narcotic ?)
    2 (*have-subdialogue* ((No\, I\'m not taking any of those\. Just the Lortab\.)
                           ((I am only taking Lortab to treat my pain \.)))) (0 :schema+args)
  1 (Are you taking blood-pressure-med ?)
    2 (*have-subdialogue* ((I\'m taking Cozar for blood pressure\.)
                           ((I am taking Cozar to help with blood pressure \.)))) (0 :schema+args)
  1 (Is the pain medication working at all ?)
    2 (*have-subdialogue* ((The Lortab is working a little bit\, but not much\.)
                           ((Why isn\'t the pain medication working anymore ?)))) (0 :schema+args)
  1 (Is the pain medication working ?)
    2 (*have-subdialogue* ((No\, it\'s not really working anymore\. So I\'ve been taking the pills
                            every three hours instead of every four\.)
                           ((Why isn\'t the pain medication working anymore ?)))) (0 :schema+args)
  1 (Do you want stronger pain medication ?)
    2 (*have-subdialogue* ((I think I could use a stronger pain medication\. Something to help make me more comfortable\.
                            What are the side effects?)
                           ((I want a stronger pain medication \.) (What are the side effects of stronger pain medication ?)))) (100 :schema+args)
    2 (You think having the stronger pain medication would help\.) (0 :out)
  1 (Do you need more medicine ?)
    2 (*have-subdialogue* ((I need a refill of my Lortab \.)
                           ((I would like a refill of medicine \.)))) (0 :schema+args)
  ; Comfort care
  1 (Have you considered comfort care ?)
    2 (*have-subdialogue* ((I haven\'t thought about it\, but it sounds like what I really need\. A way to maintain quality of life
                            during the time I have left\. Can you tell me about it?)
                           ((How does comfort care work ?)))) (0 :schema+args)
  ;Questions about medical history
  1 (What is your history with alcohol ?)
    2 (*have-subdialogue* ((There was a point in my life that I drank fairly often\. But I cut back on my own\, and I don\'t really drink a lot now\.
                                  Truth be told\, ever since the diagnosis\, I\'ve kind of lost my taste for alcohol\.)
                           ((I have a history of alcohol abuse \.) (I do not drink often now \.)))) (100 :schema+args)
    2 (At one time\, you suppose you used to have a drink or two maybe three or four days a week\. You\'ve cut back since then though\. 
          Now\, since the cancer diagnosis\, you only have a couple drinks a week \.) (0 :out)
  1 (What is your history with smoking ?)
    2 (*have-subdialogue* ((I was a pretty heavy smoker once\. I started my senior year in high school\, back when it was all the rage\, then smoked about a pack a day up until about six months ago\. Now I don\'t smoke at all\.)
                           ((I have a history of smoking \.) (I quit smoking six months ago \.)))) (100 :schema+args)
    2 (You\'ve been a heavy smoker since high school\, but you kicked the habit six months ago\.) (0 :out)
  1 (What is your history with med-narcotic ?)
    2 (*have-subdialogue* ((I took some pain medication for a fractured ankle about fifteen or so years ago\, but I don\'t believe it was a narcotic\. 
                              Besides that\, my doctor prescribed me Lortab about three weeks ago\.)
                           ((I do not have a history of narcotic abuse \.) (I am only taking Lortab to treat my pain \.)))) (0 :schema+args)
  1 (Does your family have a history of mental illness ?)
    2 (*have-subdialogue* ((I don\'t think so\. As far as I know\, no one in my family has experienced that sort of thing\.)  ((My family does not have a history of mental illness \.)))) (3 :schema+args)
    2 (*have-subdialogue* ((No\, no one in my family has any history of mental illness\.)  (0 :out)
  1 (How did your parents die ?)
    2 (*have-subdialogue* ((My parents passed on a few years ago\. Both at seventy three\, if you would believe it\. 
                               My mother died from complications with her diabetes\. She was a smoker\, something she passed onto me\.
                               My father\, though\, he died when his prostate cancer went into his bones\. Near the end of his life\, he was in a lot of pain\, which I guess is why I worry about my medications\.)
                           ((My mother died of complications from her diabetes \.) (My father died of prostate cancer\.)))) (0 :schema+args)
  
                           
  1 (0)
    2 *reaction-to-question-minor* (0 :subtree)
))


(READRULES '*reaction-to-question-minor*
; Here we match any question gist clauses which are considered "minor", i.e. off-topic or otherwise
; not expected to branch off into a further sub-dialogue.
'(
  ; Questions about daughter
  1 (Where does your daughter work ?)
    2 (She works as a school nurse in the county school system\. She\'s very diligent\. They gave her an award last year\, but I\'m blanking on the name of it\.) (0 :out)
  1 (Where does your son work ?)
    2 (He\'s in construction management\, out over in Utica\. He supervised the team that built the new firehouse there last year\.) (0 :out)
  1 (How old is your daughter ?)
    2 (She\'s thirty four\. Turning thirty five in a few months\.) (0 :out)
  1 (How old is your son ?)
    2 (He\'ll be celebrating his fortieth this year\, and is not happy about it\.) (0 :out)
  1 (Do you have any grandchildren ?)
    2 (Yes\, one grandson\. He\'s starting middle school this year and is absolutely thrilled about it\.) (0 :out)
  1 (Do you have any children ?)
    2 (Yes\, you\'re staying with my daughter and her husband here in Rochester\, but you have a son out in Utica as well\.) (0 :out)
  1 (Are you married ?)
    2 (You were for about twenty years\. But as we got older and the kids went off to college\, things just didn\'t work out as well as we thought they would\. We separated about ten years ago and divorced two years later\.) (0 :out)
  
  ; Questions about family (TODO)
))
