
# Eta Dialogue Manager (Ver 2 - 01/06/2020):

## How to run

Edit `config.lisp` and set the global parameters to the desired values (see the comment at the top of the file
for information about the config options). `*avatar*` should be set to the name of one of the available avatars (in all lowercase),
each one of which specifies a set of schemas and pattern transduction rules to guide the conversation. These options
are discussed further below.

Start SBCL in the top level directory and enter `(load "start.lisp")`. The dialogue will then begin using the top-level
schema of the chosen avatar.

Eta has two modes: text mode, and live mode. These can be changed in `config.lisp`. Use `(defparameter *mode* nil)` for
text mode, and `(defparameter *mode* t)` for live mode.

In text mode, simply enter the query into the command line when the system prompts you to do so. The system will
output the gist clause that was extracted, and the corresponding ulf if one was extracted. In the case of the David
avatar used in the blocks world system, no meaningful reaction will be given by the dialogue agent in text mode, since
answering spatial questions requires connecting with the blocks world system in live mode.

In live mode, the system will await for an input to be set in `io/input.lisp` as `(setq *next-input* "Input here")`,
where the value of \*next-input\* is a string. This is intended to be used in conjunction with an ASR program. If the
input was a spatial question, the system will output the extracted ulf to `io/ulf.lisp` as `(setq *next-ulf* '(((PRES BE.V) THERE.PRO ...) ?))`, where the value of \*next-ulf\* is some list.

**NOTE**: the value of \*next-ulf\* might potentially have an additional *poss-ques* wrapper around it, e.g. `'(POSS-QUES (((PRES BE.V) THERE.PRO ...) ?))`.

Eta's outputs are logged in plaintext in `io/output.txt`, with each output on a newline and preceeded by `#:`. This is intended
to be used in conjunction with a TTS program.

The following live mode features are (currently) specific to the david avatar used in the blocks world system:

In the case of a spatial question, the system will await a response in `answer.lisp` as
`(setq *next-answer* "Answer here")`, where the value of \*next-answer\* is a string. If the input
was not a spatial question, the system will skip this step and form a reaction as normal.

If `*perceptive-mode*` is enabled, after hearing a spatial question the system will await block coordinates in `perceptions.lisp`. The
format of this is a list of coordinate propositions, e.g. `(setq *next-coords* '((|SRI | at-coords.p 1 1 1) (|Texaco| at-coords.p 1 3 1) (|Twitter| at-coords.p 1 1 2)))`.

If `*responsive-mode*` is enabled, the spatial QA system will create natural language answers to questions. Otherwise, it
will skip this step due to not having loaded the required dependencies. This mode can safely be disabled for the other avatars.

## Supported avatars

### David

TBC

### Sophie

TBC

## Code overview

TBC



