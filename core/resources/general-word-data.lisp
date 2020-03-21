; June 8/19
;=================================================================
; These are the general word data used by a ETA1 (initial version
; derived as a variant of doolittle). [This file is derived from
; data.lisp, but excluding the doolittle rule tree.] They consist of
;
; 1. A list of contractions and their corresponding expansions; 
;    e.g., (SHE'S SHE IS) (DON'T DO NOT), etc. The expanded versions 
;    are inserted on the property lists of the contractions, under 
;    indicator TWOWORDS. The TWOWORDS property is used in an obvious 
;    way to preprocess user inputs, replacing contractions by their 
;    expanded versions.
;
; 2. The next set of items supplies contracted negations of auxiliary 
;    words, under indicator NEG. E.g., (IS ISN'T), (DO DON'T), etc. 
;    These are used in an obvious way to condense outputs just 
;    before they are printed. E.g., WHY DO NOT YOU ... is condensed 
;    to WHY DON'T YOU ... by noticing that DO has a non-NIL value 
;    for NEG, and is followed by NOT.
;
; 3. Next is a list of substitutions to be made in computing the dual
;    of an output, before actual printing. The substition is stored 
;    under the SUBST property of the word. The DUALS function makes 
;    each of the two given words the SUBST property of the other.
;
; 4. The next set provides a very extensive set of features (tags) 
;    that seem useful in formulating decomposition rules. (Not all 
;    of them are as yet used.)
;

  (MAPC (LAMBDA (TRIPLE) (SETF (GET (CAR TRIPLE) 'TWOWORDS) (CDR TRIPLE)))
	'((DON\'T DO NOT) (DONT DO NOT) (DOESN\'T DOES NOT) (DIDN\'T DID NOT)
	  (WON\'T WILL NOT) (WONT WILL NOT) 
	  (CAN\'T CAN NOT) (CANT CAN NOT) (CANNOT CAN NOT)
	  (COULDN\'T COULD NOT) (WOULDN\'T WOULD NOT) (SHOULDN\'T SHOULD NOT)
	  (AREN\'T ARE NOT) (ISN\'T IS NOT) (WASN\'T WAS NOT)
	  (WEREN\'T WERE NOT) (MIGHTN\'T MIGHT NOT) (HAVEN\'T HAVE NOT)
	  (HASN\'T HAS NOT) (HADN\'T HAD NOT)
	  (I\'M I AM) (I\'LL I WILL) (I\'D I WOULD)
	  (YOU\'RE YOU ARE) (YOU\'LL YOU WILL) (YOU\'D YOU WOULD)
	  (HE\'S HE IS) (HE\'LL HE WILL) (HE\'D HE WOULD)
	  (SHE\'S SHE IS) (SHE\'LL SHE WILL) (SHE\'D SHE WOULD)
	  (IT\'S IT IS) (IT\'LL IT WILL) 
	  (WE\'RE WE ARE) (WE\'LL WE WILL) (WE\'D WE WOULD)
	  (THEY\'RE THEY ARE) (THEY\'LL THEY WILL) (THEY\'D THEY WOULD)
	  (I\'VE I HAVE) (YOU\'VE YOU HAVE) (WE\'VE WE HAVE) 
	  (THEY\'VE THEY HAVE) (GONNA GOING TO) (WANNA WANT TO) (THAT\'S THAT IS)
	  ))

  (MAPC (LAMBDA (PAIR) (SETF (GET (CAR PAIR) 'NEG) (CADR PAIR)))
	'((DO DON\'T) (DID DIDN\'T) (DOES DOESN\'T) (WILL WON\'T) (CAN CAN\'T)
	  (COULD COULDN\'T) (WOULD WOULDN\'T) (SHOULD SHOULDN\'T)
	  (ARE AREN\'T) (IS ISN\'T) (WAS WASN\'T) (WERE WEREN\'T)
	  (HAVE HAVEN\'T) (HAS HASN\'T) (HAD HADN\'T)
	  ))


  (DUALS 'I 'YOU)
  (SETF (GET 'ME 'SUBST) 'YOU)
  (SETF (GET 'YOU2 'SUBST) 'ME)		; objective case!
  (DUALS 'MY 'YOUR)
  (DUALS 'MINE 'YOURS)
  (DUALS 'MYSELF 'YOURSELF)
  (SETF (GET 'AM 'SUBST) 'ARE)
  (SETF (GET 'ARE2 'SUBST) 'AM)		; second person! (after YOU)
  (SETF (GET 'WAS2 'SUBST) 'WERE)	; after I?
  (SETF (GET 'WERE2 'SUBST) 'WAS)	; after YOU?


					; Features:

  (MAPC 'ATTACHFEAT
	'((FINISH FINISHED DONE QUIT STOP TERMINATE)
          (FOREIGN FRANCAIS DEUTSCH ITALIANO ESPANOL
		   FRANCAIS? DEUTSCH? ITALIANO? ESPANOL?)
          (END-PUNC - ? ! \. \: \;)
          (THEME-KEY PET-KEY CHAT1-OPENING CHAT1-ROCHESTER CHAT1-MOVIES
                             ; we could add other keys, as a flag for
                             ; adhering to a particular theme (i.e., capture
          )                  ; by a particular cluster of rules)
	  (INDEX-PRON I YOU ME US MINE YOURS OURS)
	  (QUANT-PRON SOMEONE EVERYONE ANYONE SOMETHING EVERYTHING
		      EVERYBODY NOBODY SOMEBODY ANYBODY ANYTHING NOTHING)
	  (REFL-PRON ONESELF MYSELF YOURSELF HIMSELF HERSELF ITSELF
		     OURSELVES YOURSELVES THEMSELVES)
	  (ANA-PRON HE SHE IT THEY HIM HER THEM HERS THEIRS)
	  (ANAPHOR ANA-PRON HIS HER ITS THEIR)
	  (PRON INDEX-PRON QUANT-PRON REFL-PRON ANA-PRON WH-PRON)
          (WH-DET WHICH WHAT WHOSE)
          (WH-PRON WHO WHOM WHAT WHICH)
		(INDEX-DET THAT THOSE THESE THIS)
	  (DET THE A AN MY YOUR HIS HER ITS OUR THEIR ALL EVERY EACH ANY INDEX-DET
				 SOME MANY ONE TWO THREE ANOTHER OTHER WH-DET
               TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN ELEVEN TWELVE)
	  (NP_ PRON DET)                ;; the beginning of a noun phrase
	  (MODAL CAN WILL SHALL COULD WOULD SHOULD MIGHT MAY OUGHT)
	  (HAVE HAS HAD)
	  (BE AM ARE IS WAS WERE)
	  (DO DOES DID)
	  (AUX MODAL HAVE BE DO)
	  (FREQ OFTEN FREQUENTLY MANY FEW LOTS)
	  (TIMEADV TODAY YESTERDAY TOMORROW OFTEN SOMETIMES SELDOM
		   NEVER RARELY ALWAYS CONSTANTLY NOW)
	  (DEG-ADV NOT JUST VERY ONLY EXACTLY PRECISELY; name changed from 'WH-ADV' 6/17/19
             IMMEDIATELY RIGHT SLIGHTLY DIRECTLY FLUSH UP); "directly on", "flush against"
	  (INITADV TIMEADV DEG-ADV)
	  (CONJ BUT AND OR)
	  (WH_ WH-DET WH-PRON WHY HOW WHEN WHERE);; begin'g of wh-ques
	  (QUANT POSQUANT NEGQUANT)
	  (POSQUANT ALWAYS ALL EVERYONE EVERYTHING EVERY EVERYBODY
		    CONSTANTLY)
	  (NEGQUANT NEVER NOTHING NOONE NO-ONE NOBODY)
	  (ADV POSADV NEGADV INITADV S-ADV)
	  (POSADV ALWAYS SOMETIMES CERTAINLY COURSE ABSOLUTELY SURE OK
		  OCCASIONALLY MOSTLY YESTERDAY TOMORROW FREQUENTLY USUALLY
		  CONSTANTLY PROBABLY REALLY TRULY OBVIOUSLY NATURALLY |OK,|
		  ALSO SO)
	  (NEGADV SELDOM RARELY NEVER ALMOST)
          (S-ADV PERHAPS)
	  (PERHAPS MAYBE POSSIBLY)
          (SUPPOSE GUESS IMAGINE HOPE) ; aimed at things like "I suppose so"
          (TENTATIVE PERHAPS SUPPOSE)
          (DOUBT UNLIKELY HARDLY NOT); aimed at things like "I doubt it",
                                     ; "I don't think so", "That's unlikely"
	  (SELF I MY MYSELF ME)
	  (FATHER DAD)
	  (MOTHER MOM MOMMY)
	  (CHILD CHILDREN KID KIDS)
	  (SON SONS) 
	  (DAUGHTER DAUGHTERS)
	  (SPOUSE HUSBAND WIFE)
	  (GRANDFATHER GRANDDAD GRANDPA)
	  (GRANDMOTHER GRANNY GRANDMA) 
	  (FAMILY1 FATHER MOTHER SON DAUGHTER SPOUSE
		   CHILD SON DAUGHTER)
	  (FAMILY2 GRANDMOTHER GRANDFATHER COUSIN NIECE NEPHEW UNCLE AUNT
		   MOTHER-IN-LAW FATHER-IN-LAW)
	  (FAMILY FAMILY1 FAMILY2)
	
	  (MONEY CASH ASSETS FINANCIAL)
	  (NOMONEY DEBT BROKE)
	  (MONEYTHEME MONEY NOMONEY BANK ACCOUNT MORTGAGE
		      PAYMENTS ENDS FORTUNE)
	  (TROUBLE TROUBLES DIFFICULT DIFFICULTY DIFFICULTIES 
		   TOUGH HARD HARDSHIP CHALLENGING PROBLEM PROBLEMS
                   STRUGGLE STRUGGLING) 
	  (NOMORE RID LOST GONE LEFT AWAY OUT)
	  (NEG NO NOT NOPE NAH HARDLY LITTLE SCARCELY)
		(POS YES YEAH YUP SURE DEFINITELY CERTAINLY ABSOLUTELY INDEED AGREED)

	  (MARRIAGETHEME MARRIAGE SPOUSE DIVORCE)
	  (DIVORCE DIVORCED SEPARATION SEPARATED)
	  (LOVELIFE LOVE LOVER SEX)
	  (LOVER MAN WOMAN GIRLFRIEND BOYFRIEND AFFAIR MISTRESS)
	  (SCHOOLTHEME SCHOOLWORK TEACHER)
	  (SCHOOLWORK SCHOOL CLASS CLASSES COURSE COURSES ASSIGNMENT ESSAY
		      ESSAYS HOMEWORK ASSIGNMENTS EXAM EXAMS TEST TESTS GRADES MARKS
		      STUDY STUDIES)
	  (TEACHER TEACHERS INSTRUCTOR INSTRUCTORS PROFESSOR PROFESSORS
		   LECTURER LECTURERS COLLEAGUE COLLEAGUES)
	  (WORKTHEME WORK-BOSS SALARY OFFICE CO-WORKERS)
	  (WORK-BOSS WORK BOSS)
	  (WORK WORKING WORKED JOB JOBS)
	  (BOSS EMPLOYER SUPERVISOR CHAIRMAN)
	  (SALARY RAISE PAY)
	  (SOCIALLIFE FRIEND FUN)
	  (FRIEND FRIENDS BUDDY BUDDIES PAL PALS ACQUAINTANCE
		  ACQUAINTANCES FRIENDSHIP FRIENDSHIPS)
	  (FUN SOCIAL PARTY PARTIES DANCE DANCES DANCING
	       MOVIE  CONCERT VISIT VISITING INVITE INVITED) ;SHOW
          (PET-TYPE DOG DOGS CAT CATS CANARY BUDGY BUDGIE BUDGERIGAR PARROT
               GOLDFISH GERBIL HAMSTER GUINEA-PIG GUINEAPIG GUINEA PIG IGUANA 
               TURTLE HORSE PONY)
          (PETTHEME PET-KEY PET-TYPE PETS PET)
               
	  (BELIEVE THINK KNOW SUPPOSE SUSPECT PRESUME GUESS)
	  (REMEMBER RECALL)
	  (PERCEIVE NOTICE NOTE SEE INFER CONCLUDE REALIZE)
	  (CONC BELIEVE REMEMBER PERCEIVE)
	  (BADSTATE UNHAPPY SAD WORRIED TIRED DEPRESSED APPALLED
		    TERRIBLE AWFUL LONELY DISGUSTED UPSET BORED DISMAYED
		    DISTRESSED SHAMBLES WRECKED ROCKS DEAD FALLING APART
		    DISINTEGRATING PIECES LOUSY)
	  (BADHEALTH SICK WRECK CANCER ILL ILLNESS ILLNESSES INSANE
		     SICKNESS PAIN PAINS ACHE ACHES CRAZY HURT HURTS INJURY 
                     INJURED SMOKE SMOKING ALCOHOLIC ALCOHOLISM TIRED FATIGUED)
          (SICKNESS SICKNESSES COLD FLU HEADACHE HEADACHES TOOTHACHE CANCER 
                    DIABETES DIABETIC DIABETICS RABIES DISEASE)
	  (GOODHEALTH HEALTHY WELL BETTER HEALTH)
	  (HEALTH-THEME GOODHEALTH BADHEALTH)
	  (BADQUALITY STUPID MEAN NASTY SILLY IDIOTIC VICIOUS DISGUSTING DORKY
		      BORING UGLY SMELLY WEIRD DUMB OLD POOR DIFFICULT LAZY YUCKY
		      DIFFICULT EVIL WICKED MALICIOUS DENSE INSANE CRAZY NUTS
		      PHONEY TERRIBLE AWFUL DREADFUL MISERABLE HELLISH STINK
              STINKS SUCK SUCKS GROSS GREASY)
	  (DIFFICULT TOUGH IMPOSSIBLE HARD CHALLENGING DEMANDING STRESSFUL)
	  (MUCH TOO MANY)
	  (HATE HATES HATED)
	  (DISLIKE DISLIKES)
	  (BADATTITUDE HATE DISLIKE AFRAID DESPISE)
	  (YELL YELLING YELLED YELLS SCREAM SCREAMING SCREAMED SCREAMS) 
	  (BOTHER BOTHERS BOTHERING BUG BUGS BUGGING ANNOYS HARRASS HARRASSING 
                  HARASSES NAG NAGS NAGGING PESTER PESTERS PESTERING)
	  (FIGHT FIGHTS FIGHTING FOUGHT ARGUE ARGUING ARGUES 
		 QUARREL QUARRELING QUARRELED)
	  (BADREL BADATTITUDE BOTHER HATE YELL FIGHT)
	  (KILL KILLED)
	  (MURDER MURDERED)
	  (BEAT BEATS BEATING)
	  (SHOOT SHOT)
	  (STAB STABBED)
	  (BATTER BATTERED BATTERS)

          (BADOCCUR DIED DISAPPEARED BURNED CRASHED BROKE COLLAPSED)
          (BADEVENT DEATH DISAPPEARANCE ACCIDENT)
	
	  (INJURE BEAT SHOOT STAB BATTER)
	  (VIOLENCE KILL MURDER INJURE)
	  (CHEAT CHEATING CHEATS CHEATED)
	  (LIE LYING LIED)
	  (FOOL FOOLS FOOLED FOOLING)
	  (DECEIVE DECEIVED DECEIVING CHEAT LIE FOOL)
	  (ERR GOOF GOOFED BLEW BLOWN MISTAKE MISTAKES BLUNDER BLUNDERS)
	  (STUPIDTHING IDIOT MORON JERK FOOL ASS WEIRDO PERVERT DORK DUMMY DUM
		       DUMMIE DUM-DUM NERD BOZO YOYO PEABRAIN DINGBAT DING-DONG 
		       DING-DING FREAK SLOB GOOBER NUMBSKULL DIMWIT NITWIT LOSER
		       SLEAZEBAG CRETIN AIRHEAD TURKEY TWIT DWEEB)
	  (SCARYTHING KILLER MONSTER EXAM OLD DEATH SICKNESS AGE SNAKES)
	  (BADTHING STUPIDTHING SCARYTHING CRIME)
	  (CRIME VIOLENCE ROB ROBBED ROBBERY MUGGING THEFT THIEF
		 BURGLAR BURGLARY STEAL STOLE RAPE RAPED EXTORTION PIMP
		 EXTORTING EXTORTED EXTORTS RIP-OFF MAFIA MOB MOBSTER)
	  (LIQUOR BOOZE BEER DRINK DRINKS DRINKING DRANK DRUNK ALCOHOL
		  ALCOHOLIC)
	  (DRUGS DRUG LIQUOR ADDICT JUNKIE HOOKED ADDICTED HEROIN COCAIN
		 COCAINE MAINLINE DOPE ECSTASY CRACK)
	  (BADPROP BADSTATE BADQUALITY STUPIDTHING)
	  (BADPRED BADPROP BADREL BADEVENT VIOLENCE DECEIVE BADTHING)
	  (GOODSTATE HAPPY WELL FINE PLEASED DELIGHTED CHEERFUL GLAD	
		     SATISFIED CONTENTED JOY JOYFUL)
	  (GOODQUALITY SMART CLEVER BRIGHT NICE GOOD FRIENDLY PRETTY COOL SEXY
		       HANDSOME GOOD-LOOKING KIND INTELLIGENT HELPFUL LOVELY 
		       ENJOYABLE DELIGHTFUL ENTERTAINING FUN FUNNY WONDERFUL
		       NORMAL SANE BEAUTIFUL SHARP PERFECT DELICIOUS KNOWLEDGEABLE SKILLED)
	  (LOVE LOVES)
	  (LIKE LIKES)
	  (ADMIRE ADMIRES) 
	  (UNDERSTAND UNDERSTANDS)
          (LISTEN LISTENS HEAR HEARS)
	  (RESPECT RESPECTS)
	  (APPRECIATE APPRECIATES)
	  (CARE CARES)
	  (ENJOY ENJOYS ENJOYING ENJOYED)
	  (GOODATTITUDE LOVE LIKE ADMIRE UNDERSTAND RESPECT STAND 
             APPRECIATE CARE ENJOY)
	  (HELP HELPS HELPED ASSIST ASSISTS ASSISTED)
          (RELAXATION RELAX RELAXES RELAXING RELAXED CALM CALMS CALMING SOOTH
             SOOTHES SOOTHINGTHERAPY THERAPEUTIC)
          (VERY QUITE EXTREMELY EXCEEDINGLY EXCEPTIONALLY EXTRAORDINARY TREMENDOUSLY
             IMMENSELY HUGELY UNCOMMONLY PARTICULARLY HIGHLY REMARKABLY TRULY REALLY)
          (HELPFUL HELPING HELP OBLIGING ACCOMMODATING SYMPATHETIC USEFUL)
          (TIRED SLEEPY EXHAUSTED WEARY FATIGUED)
	  (SUPPORT SUPPORTS SUPPORTED)
          (QUICK EFFICIENT FAST)
	  (GOODTHING GENIUS BEAUTY SEX FUN PLEASURE)
	  (GOODACTION HELP SUPPORT) 
	  (GOODPROP GOODSTATE GOODQUALITY)
	  (GOODREL GOODATTITUDE GOODACTION)
	  (GOODPRED GOODPROP GOODREL GOODTHING)
	  (INFORM INFORMS INFORMED TELL TELLS TOLD TELLING
		  ASSURE ASSURES ASSURES ASSURING)
	  (ASSERT ASSERTS ASSERTED ASSERTING SAY SAYS SAID SAYING 
		  CLAIM CLAIMS CLAIMED STATES STATED STATING)
	  (COMMUN INFORM ASSERT)
	  (WANT NEED NEEDS WISH REQUIRE REQUIRES)
	  (V AUX CONC BADREL VIOLENCE GOODREL COMMUN WANT)
	  ))


   (SETQ *TRACERULES* NIL)


