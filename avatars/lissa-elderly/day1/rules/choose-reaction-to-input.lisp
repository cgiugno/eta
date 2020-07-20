;; "choose-reaction-to-input.lisp" (for single gist clause inputs)
;; ===============================================================
;; File for choosing a reaction to a feature-augmented gist
;; clause extracted from a user's answer to a question. In
;; general, the gist clause is expected to provide sufficient
;; information to allow choice of a comment (if warranted) from
;; Lissa, based on choice trees for specific answers, thematic
;; answers, and potentially a final question in the gist
;; clause list.
;;
;; This packet is for single clauses -- see 
;;     "choose-reactions-to-input.lisp"
;; (note the plural) for a packet that makes a choice of a schema
;; depending on multiple gist clauses extracted from the user 
;; input -- but realizing the steps of the schema again depend
;; on the choice packet in this file, and the choice trees 
;; referenced here.
;; 
;; The gist clause input is expected to be of the (at least
;; approximate) form currently constructed by the choice trees
;; for extracting gist clauses from user input, specifically
;; the inputs responding to the questions (as gist clauses)
;;
;;           (where are you from ?)
;;           (tell me more about your hometown ?)
;;           (how did you like the weather in your hometown ?)
;;           (How did you end up in Rochester ?)

(MAPC 'ATTACHFEAT ; needed for detecting alternatives in the
                  ; watching-or-reading question
'(
  (spare-time-activity sports read reading watch watching play
    playing hike hiking explore exploring walk walking walks hobby
    hobbies painting); others? "make", "build" seem too general
))


(READRULES '*reaction-to-input*
; Choose between reaction to a question and an assertion
; Only one gist clause is expected here
'(
  1 (0 wh_ 3 you 0)
    2 *reaction-to-question* (0 :subtree)
  1 (0 wh_ 3 your 0)
    2 *reaction-to-question* (0 :subtree)
  1 (0 aux your 0)
    2 *reaction-to-question* (0 :subtree)
  1 (0 aux you 0)
    2 *reaction-to-question* (0 :subtree)
  1 (0 right-really 4 ?)
    2 *reaction-to-question* (0 :subtree)
  1 (0); by default, it's an assertion
    2 *reaction-to-assertion* (0 :subtree)
))


(READRULES '*reaction-to-assertion*
; Very rough initial attempt.
; Actually, it seems we could readily provide reactions
; directly here, instead of delegating to specialized
; choice trees. However, it seems we have better oversight
; by using separate choice trees, specified in a file that
; also contains the specialized features for the topic at
; issue.
'(
  ; getting-to-know
  1 (0 My name is 0)
    2 *reaction-to-name-input* (0 :subtree)
  1 (0 I had 4 for breakfast 0)
    2 *reaction-to-breakfast-today-input* (0 :subtree)
  1 (0 ice cream flavor I like 0) 
    2 *reaction-to-favorite-icecream-input* (0 :subtree)
	1 (0 I do not 3 favorite ice cream flavor 0)
    2 *reaction-to-favorite-icecream-input* (0 :subtree)
  1 (0 food I like 0)
    2 *reaction-to-favorite-food-input* (0 :subtree)
  1 (0 do not have a favorite food 0)
    2 *reaction-to-favorite-food-input* (0 :subtree)
  1 (0 to get here 0)
    2 *reaction-to-how-you-got-here-input* (0 :subtree)

  ; where-are-you-from
  1 (0 I am from 0) 
    2 *reaction-to-hometown-input* (0 :subtree)
  1 (0 the weather in my hometown 0)
    2 *reaction-to-hometown-weather-input* (0 :subtree)
  1 (0 I do not like the weather 0)
    2 *reaction-to-hometown-weather-input* (0 :subtree)
  1 (0 I like the weather 0)
    2 *reaction-to-hometown-weather-input* (0 :subtree)
  1 (0 where I grew up is 0)
    2 *reaction-to-describe-hometown-input* (0 :subtree)
  1 (0 I ended up in Rochester 0)
    2 *reaction-to-endup-in-rochester-input* (0 :subtree)

  ; activities
  1 (0 my hobby is 0) 
    2 *reaction-to-hobbies-input* (0 :subtree)
  1 (0 do not like 2 hobby 0)
    2 *reaction-to-hobbies-input* (0 :subtree)   
  1 (0 I do not like reading 0)
    2 *reaction-to-like-to-read-input* (0 :subtree)
  1 (0 I like reading 0)
    2 *reaction-to-like-to-read-input* (0 :subtree)
  1 (0 I like to read 0)
    2 *reaction-to-things-like-to-read-input* (0 :subtree)
  1 (0 I spend my days 0)
    2 *reaction-to-spend-your-days-input* (0 :subtree)
  1 (0 I like to 5 in neighborhood 0)
    2 *reaction-to-things-in-neighborhood-input* (0 :subtree)
  1 (0 I do not like to 5 in neighborhood 0)
    2 *reaction-to-things-in-neighborhood-input* (0 :subtree)
))

 
(READRULES '*reaction-to-unexpected*
'(
  1 (0 thank you 0)
    2 *user-thanks-schema* (0 :schema)
))

