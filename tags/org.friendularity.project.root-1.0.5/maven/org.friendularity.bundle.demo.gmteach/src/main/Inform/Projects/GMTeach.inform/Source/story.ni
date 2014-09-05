"GMTEACH1" by GLUE-AI


[---- DOCUMENTATION ----

Section: What it does.

Tuneplaying is a simple extension (version 2 updated for compatibility with 6G60) that does the following things. First, we can create words for new music-performances, such as shazam. Once shazam has been added to a game according to the guidelines below, the player will be able to use the commands "shazam", "play shazam", "shazam the X", and "play shazam on the X" (where X is some object in the game).

By default, however, the performances we add to a game are unlearned, and thus can't be used by the player. We can declare a performance as learned when we create it (in which case the player will be able to use it from the beginning of the game), or we can create a mechanism (for instance, as part of the action of reading a performance-book) that includes the line "now shazam is learned".

The extension also implements the out of world command "performances". This will list the performances the player has learned, together with a brief description of each. If no performances have been learned, the "performances" command will say so.

The new actions defined in Tuneplaying are playing, playing it on, zapping (which handles commands of the form "shazam the X"), and tunechecking.

Section: Using Tuneplaying.

To add our own performances to a game, we need to do a few things.

First, we create Table of Performances (continued). The Table of Performances (continued) should have the following headings: Performance, Output, Directedoutput, Description.

Put new performance words into the Performance column. The other three columns should contain double-quoted text. The Output column should contain the default text that will print when the player types the name of the performance as a bare command ("shazam") or uses the "play" verb ("play shazam"). The Directedoutput column should contain the default text that will print when the player plays the performance at an object in the game using the format "shazam the X" or "play shazam at X". In most cases these defaults won't be needed, because the handling of performance-playing will be under the control of instead rules; but writing catch-all default messages is good coding style. The Description column should contain a brief description of the performance, which will be printed (after the performance has been learned) in response to the "performances" command.

After entering a new performance in the Table of Performances (continued), declare the name of the performance as a music-performance. A music-performance is a kind of backdrop, which is convenient as we want it to be present in the room no matter where the player is. Because Inform can't put kinds in locations, we have to make each of our music-performances everywhere, like this:

shazam is a music-performance. It is everywhere.

Alternatively, we might want to define a region in which the shazam performance will operate, and confine the backdrop to that region. If we do this, however, the performance will not be in scope when the player is not in that region, which will cause a performance the player has learned to erroneously appear unlearned when the player tries to play it in the wrong place. A better solution is to edit the Output or Directedoutput message of the performance in a way that depends on the player's location.

This is easily done using "To say" code. In the Output column, the text should read something like "[shazam-play]". We could then write code like this:

To say shazam-play:
	if the player is in the Arboretum...

Because music-performances are unlearned by default, the next step in coding is to specify the manner in which the new performances can be learned.

Finally, we write our own Instead rules for playing specific performances, either without an object ("shazam") or on specific objects ("shazam the X").

We can also, if desired, substitute our own messages for the extension's error messages no-performances-learned, not-a-performance, unknown-performance, unlearned-performance, and cant-see-any-such:

To say not-a-performance:
	say "What are you gibbering about now?"

If a game uses some other message than "You can't see any such thing" in response to references to things that are invisible or nonexistent, it's a good idea to substitute the new message for cant-see-any-such.

Example: ** The Sorcerer's Apprentice - Some new performances that can be learned from a dusty old book.
]



Section 5.1 - Includes

Include Questions by Michael Callaghan.


[Include Room Description Control by Emily Short.]
[Include Simple Followers by Emily Short.]
[Include Simple Graphical Window by Emily Short.]
[Include Single Paragraph Description by Emily Short.]
[*Include Status Line Removal by Emily Short.]
[Include Tailored Room Description by Emily Short.
Include Transit System by Emily Short.]

[SAFE]

Include Commonly Unimplemented by Aaron Reed.
Include Config File by Aaron Reed.
Include Console by Aaron Reed.
Include Dynamic Rooms by Aaron Reed.
Include Extended Grammar by Aaron Reed.
Include Poor Man's Mistype by Aaron Reed.
Include Procedural Randomness by Aaron Reed.
Include Remembering by Aaron Reed.
Include Neutral Library Messages by Aaron Reed.
Include Keyword Interface by Aaron Reed.
Include Numbered Disambiguation Choices by Aaron Reed.
Include Lines of Communication by Aaron Reed.
Include Small Kindnesses by Aaron Reed.
Include Numerical Inflection by Aaron Reed.
Include Spin by Aaron Reed.
Include Greek To Me by Adam Thornton.
Include Runic Characters by Adam Thornton.
Include Reversed Persuasion Correction by Juhana Leinonen.
Include Hidden Items by Krister Fundin.
Include WhoWhat by David Cornelson.

Include Computers by Emily Short.
Include Deluxe Doors by Emily Short.
Include Locksmith by Emily Short.
Include Debugging by Al Golden.

Include Approaches by Emily Short.
Include Assorted Text Generation by Emily Short.

Include Fully Spatial by Quantum Games.
Include Touchy Feely by Quantum Games.
Include Directional Facing by Tim Pittman.
Include Extended Banner by Stephen Granade.

Include Hiding Under by Eric Eve.
[Include NPC Implicit Actions by Eric Eve.]


[Include Protagonists by Kevin Norris.  ][but conflicts with ..]
[wierd but works Include Adventure Book by Edward Griffiths.]


[
* Include Intelligent Hinting by Aaron Reed.
* Include Conversation Framework for Sand-dancer by Aaron Reed.
* Include Player Experience Upgrade by Aaron Reed.
* Include Smarter Parser by Aaron Reed.
*Include Basic Help by David Cornelson.
]

[*Include Liquid Handling by Al Golden.]

[*Include Automated Drawers by Emily Short.]

[C Include Secret Doors by Andrew Owen.]
[
Include Exit Lister by Andre Kosmos.
Include Modified Inventory by Al Golden.
Include Modified Lock-Unlock by Al Golden.
Include Record-Replay by Al Golden.]
[*Include Supplemental Actions by Al Golden.]
[Include Conversation Builder by Chris Conley.
Include Threaded Conversation by Chris Conley.]
[*Include Past Tense Commands by Dan Efran.]
[
Include Basic IF Help by Andrew Plotkin.
Include Unicode Parser by Andrew Plotkin.
Include Bitwise Operators by Bart Massey.
Include Mistype by Cedric Knight.
Include Configurable Creative Commons License by Creative Commons.

Include Benchmarking by Dannii Willis.
Include Simple Unit Tests by Dannii Willis.
Include Ultra Undo by Dannii Willis.
Include Repeat Boxes by Dave Robinson.
Include Snippetage by Dave Robinson.
Include Custom Library Messages by David Fisher.
Include Default Messages by David Fisher.
Include Armed by David Ratliff.
Include Glulx Boxed Quotation by Eliuk Blau.

Include Basic Hyperlinks by Emily Short.
Include Dishes by Emily Short.
Include Empty Transfer by Emily Short.
Include Facing by Emily Short.
Include Glulx Entry Points by Emily Short.
Include Glulx Text Effects by Emily Short.
Include Introductions by Emily Short.
Include Location Images by Emily Short.
Include Measured Liquid by Emily Short.
Include Modern Conveniences by Emily Short.
Include Modified Exit by Emily Short.
Include Mood Variations by Emily Short.
Include Ordinary Room Description by Emily Short.
Include Postures by Emily Short.
Include Power Sources by Emily Short.
Include Property Checking by Emily Short.
Include Recorded Endings by Emily Short
Include Tutorial Mode by Emily Short.
Include Adaptive Hints by Eric Eve.
Include Alternatives by Eric Eve.
Include Bulk Limiter by Eric Eve.
Include Conversation Framework by Eric Eve.
Include Conversation Nodes by Eric Eve.
Include Conversation Package by Eric Eve.
Include Conversation Responses by Eric Eve.
Include Conversation Rules by Eric Eve.
Include Conversation Suggestions by Eric Eve.
Include Conversational Defaults by Eric Eve.
Include Epistemology by Eric Eve.
Include Exit Lister by Eric Eve.


Include Implicit Actions by Eric Eve.
Include Limited Implicit Actions by Eric Eve.
Include List Control by Eric Eve.
Include List Controller by Eric Eve.
Include Text Capture by Eric Eve.
Include Underside by Eric Eve.
Include Variable Time Control by Eric Eve.
Include French by Eric Forgeot.
Include Extended Debugging by Erik Temple.
Include Glimmr Animation Fader - Black by Erik Temple.
Include Glimmr Automap by Erik Temple.
Include Glimmr Automap Tileset by Erik Temple.
Include Glimmr Bitmap Font by Erik Temple.
Include Glimmr Canvas Animation by Erik Temple.
Include Glimmr Canvas Editor by Erik Temple.
Include Glimmr Canvas-Based Drawing by Erik Temple.
Include Glimmr Debugging Console by Erik Temple.
Include Glimmr Drawing Commands by Erik Temple.
Include Glimmr Form Fields by Erik Temple.
Include Glimmr Graphic Hyperlinks by Erik Temple.
Include Glimmr Image Font by Erik Temple.
Include Glimmr Simple Graphics Window by Erik Temple.
Include Glulx Drawing Commands by Erik Temple.
Include Glulx Input Loops by Erik Temple.
Include Glulx Status Window Control by Erik Temple.
Include Graphical Window Animation by Erik Temple.
Include Graphical Window Sprites by Erik Temple.
Include HTML Color Names for Glulx Text Effects by Erik Temple.
Include Inline Hyperlinks by Erik Temple.
Include Real-Time Delays by Erik Temple.
Include Text Window Input-Output Control by Erik Temple.
Include Undo Output Control by Erik Temple.
Include Room Description Headings by Erwin Genstein.
Include Second Gender by Felix Larsson.
Include Swedish by Felix Larsson.
Include GNU General Public License v3 by Free Software Foundation.
Include Exit Lister by Gavin Lambert.
Include Extended Timers by Gavin Lambert.
Include Telephones by George Tryfonas.
Include Weather by Ish McGravin.
Include Metagame and Fixes by Jason Catena.
Include Puzzle Boxes by Jason Catena.
Include Graphic Links by Jeff Sheets.
Include Action Queuing by Jesse McGrew.
Include Conditional Undo by Jesse McGrew.
Include Dynamic Objects by Jesse McGrew.
Include Dynamic Tables by Jesse McGrew.
Include Hypothetical Questions by Jesse McGrew.]
[*Include One-Shot Text by Jesse McGrew.]
[Include String Buffers by Jesse McGrew.
Include Notepad by Jim Aikin.
Include Tuneplaying by Jim Aikin.
Include Conditional Backdrops by John Clemens.
Include Consolidated Multiple Actions by John Clemens.
Include Scheduled Activities by John Clemens.
Include Pausing the game by John Goettle.
Include Advanced Help Menu by John W Kennedy.
Include Considerate Holdall by Jon Ingold.
Include Disambiguation Control by Jon Ingold.
Include Far Away by Jon Ingold.
Include Flexible Windows by Jon Ingold.
Include Interactive Parsing by Jon Ingold.
Include Inventory Window by Jon Ingold.
Include Landmark Events by Jon Ingold.
Include line break workaround by Jon Ingold.]
[*Include Multi-examine by Jon Ingold.]
[Include Restrictions by Jon Ingold.
Include Text Variations by Jon Ingold.
Include Title Page by Jon Ingold.
Include Written Inventory by Jon Ingold.
Include Bulky Items by Juhana Leinonen.
Include Debug Files by Juhana Leinonen.
Include Object Response Tests by Juhana Leinonen.
Include Regional Travel by Juhana Leinonen.]
[*Include Interactive Poetic Interludes by Kazuki Mishima.]
[Include Numbers by Krister Fundin.
Include Hyperlink Interface by Leonardo Boselli.
Include German Basic Help Menu by Lukas Strahner.
Include German Basic Screen Effects by Lukas Strahner.
Include German Epistemology by Lukas Strahner.
Include German Menus by Lukas Strahner.
Include Automap by Mark Tilford.
Include Simple Chat by Mark Tilford.
Include Simple CYOA by Mark Tilford.
Include MIT X11 License by Massachusetts Institute of Technology.
Include Intestazione di Pagina by Massimo Stella.
Include Italian by Massimo Stella.
Include Limitatore di Ingombro by Massimo Stella.
Include Menus by Massimo Stella.
Include Modelli di Conversazione by Massimo Stella.
Include Multiple Sounds by Massimo Stella.
Include Nasconder Sotto by Massimo Stella.
Include Quips Interattive by Massimo Stella.
Include Telefoni by Massimo Stella.
Include Keywords for Conversation by Matt Wigdahl.
Include Exit Descriptions by Matthew Fletcher.
Include Exit Descriptions SP by Matthew Fletcher.
Include Interactive Poetry by Michael Bacon.
Include Fixed Point Maths by Michael Callaghan.
Include Patrollers by Michael Callaghan.
Include Questions by Michael Callaghan.
Include Simple Debugger by Michael Hilborn.
Include Assumed Conversers by Michael Martin.
Include Quip-Based Conversation by Michael Martin.
Include Reactable Quips by Michael Martin.
Include Unicode Interrogation by Michael Martin.
Include Achievements by Mikael Segercrantz.
Include Atmospheric Effects by Mikael Segercrantz.
Include Instead of Going by Mikael Segercrantz.
Include Located Sounds by Mikael Segercrantz.
Include Multiple Exits by Mikael Segercrantz.
Include Randomness by Mikael Segercrantz.
Include Shipboard Directions by Mikael Segercrantz.
Include Trinity Inventory by Mikael Segercrantz.
Include Weather Effects by Mikael Segercrantz.
Include Conditional Backdrops by Mike Ciul.
Include Lost Items by Mike Ciul.
Include Scope Caching by Mike Ciul.
Include Planner by Nate Cull.
Include Basic Plans by Nate Cull.
Include Unknown Word Error by Mike Ciul.]



[Include Unknown Word Error by Neil Cerutti.
Include Directional Facing by Poster.
Include Automated Testing by Roger Carbol.
Include After Not Doing Something by Ron Newcomb.
Include Automated Verb Phrases by Ron Newcomb.
Include Command Prompt on Cue by Ron Newcomb.
Include Custom Library Messages by Ron Newcomb.
]


[*Include Default Messages by Ron Newcomb.]
[
Include Dialogue Punctuation by Ron Newcomb.
Include Editable Stored Actions by Ron Newcomb.
* Include Grouped Messages As Dialogue by Ron Newcomb.
Include Ignored Rules by Ron Newcomb.
Include Mentioned In Room Description by Ron Newcomb.
Include Original Parser by Ron Newcomb.
Include Output Filtering by Ron Newcomb.
Include Passable Relations by Ron Newcomb.
Include Permission to Visit by Ron Newcomb.
Include Phrases for Adaptive Pacing by Ron Newcomb.
Include Phrases for Tables with Topics by Ron Newcomb.
Include Player-Character Requires Persuasion by Ron Newcomb.
Include Problem-Solving Characters by Ron Newcomb.
Include Pronouns by Ron Newcomb.
Include Real Date and Time by Ron Newcomb.
Include Repeat Through a Rulebook by Ron Newcomb.
Include Repeat Through Actions by Ron Newcomb.
Include Rewrite the Command Line by Ron Newcomb.
Include Scope Control by Ron Newcomb.
Include Unsuccessful PC Attempt by Ron Newcomb.
Include Dice-Lock by S John Ross.
Include Helpers for GBA Frotz by S John Ross.
Include Shipboard Directions by Samuel Byron.
Include Basic Real Time by Sarah Morayati.
Include Plugs and Sockets by Sean Turner.
Include Spanish by Sebastian Arg.
Include AI Moving by Sebastian Rahn.
Include Room & Dimension by Sebastian Rahn.
Include Basic Characters by ShadowChaser.
Include Senses by ShadowChaser.
Include StartEnd MenuPages by ShadowChaser.
Include Useful Functions by ShadowChaser.

Include Footnotes by Stephen Granade.
Include German by Team GerX.
Include Native Time Control by Tim Pittman.
Include Undescribed Objects by Tim Pittman.
Include Effetti Visivi di Base by Tristano Ajmone.
Include Permadeath by Victor Gijsbers.
Include Basic Help Menu by Wade Clarke.
Include Menus by Wade Clarke.
Include Event Chains by William S Martin.
]

Section 1 - Definition, Some Performances, and Table

A music-performance is a kind of backdrop. A music-performance can be learned or unlearned. A music-performance is usually unlearned.

xyzzy is a music-performance. It is everywhere.

Table of Performances
Performance		Output			Directedoutput	Description
xyzzy		"You play the xyzzy performance, but nothing happens."	"You play the xyzzy performance on [the second noun], but nothing happens."		"The xyzzy performance does nothing much."

Section 2 - Trapping Non-Allowed References to Performance Objects & Performance-Playing Verb

Instead of doing anything when the noun is a music-performance (this is the pretend the performance is invisible rule):
	if the current action is playing something:
		continue the action;
	otherwise if the current action is playing something on something:
		continue the action;
	otherwise if the current action is zapping:
		continue the action;
	otherwise:
		say "[cant-see-any-such]".
	
[This rule handles the case where the player types 'play X'. When the intended action is "play", we want a different error message, not "You can't see any such thing," since one can't see performances. However, if word 1 is "play" and word 2 is the name of a performance, then we're getting the parser error because something later in the input made no sense, so in this case we DO want "You can't see any such thing."]
	
Rule for printing a parser error when the latest parser error is can't see any such thing error (this is the new can't see any such thing rule):
	let T be indexed text;
	let T be the player's command;
	if word number 1 in T is "play":
		let W be word number 2 in T;
		repeat with N running from 1 to the number of rows in the Table of Performances:
			let S be the performance in row N of the Table of Performances;
			let SS be the printed name of S;
			if W is SS:
				say "[cant-see-any-such]";
				stop the action;
		say "[unknown-performance]";
	otherwise:
		say "[cant-see-any-such]".

[This rule handles the case where the player tries to play a performance (learned or unlearned) on an object that is not in scope or does not exist. Without this rule, Inform would reply, "That's not a verb I recognize," which would be misleading if the performance were known. So we need to trap the first word in the input and look at it. If it's a performance, we need to output a different error message.]
		
Rule for printing a parser error when the latest parser error is not a verb I recognise error (this is the new not a verb I recognise rule):
	let T be indexed text;
	let T be the player's command;
	let W be word number 1 in T;
	repeat with N running from 1 to the number of rows in the Table of Performances:
		let S be the [name of the] performance in row N of the Table of Performances;
		let SS be the printed name of S;
		if W is SS:
			say "[cant-see-any-such]";
			stop the action;
	continue the action.
		
Section 3 - New Actions

Zapping is an action applying to two things. Understand "[music-performance] [something]" as zapping.

Carry out zapping:
	try playing the noun on the second noun instead.

Playing is an action applying to one thing. Understand "[music-performance]" and "play [something]" as playing.

Check playing:
	let found-it be a truth state;
	let found-it be false;
	let its-learned be a truth state;
	let its-learned be false;
	repeat with N running from 1 to the number of rows in the Table of Performances:
		if the performance in row N of the Table of Performances is the noun:
			now found-it is true;
			if the noun is learned:
				now its-learned is true;
	if found-it is true:
		if its-learned is false:
			say "[unlearned-performance]";
			stop the action;
		otherwise:
			continue the action;
	otherwise:
		say "[not-a-performance]";
		stop the action.
		
Carry out playing:
	repeat with N running from 1 to the number of rows in the Table of Performances:
		if the performance in row N of the Table of Performances is the noun:
			say "[output in row N of the Table of Performances][line break]".

Playing it on is an action applying to two things. Understand "play [something] on [something]" as playing it on.

Check playing it on:
	let found-it be a truth state;
	let found-it be false;
	let its-learned be a truth state;
	let its-learned be false;
	repeat with N running from 1 to the number of rows in the Table of Performances:
		if the performance in row N of the Table of Performances is the noun:
			now found-it is true;
			if the noun is learned:
				now its-learned is true;
	if found-it is true:
		if its-learned is false:
			say "[unlearned-performance]";
			stop the action;
		otherwise:
			continue the action;
	otherwise:
		say "[not-a-performance]";
		stop the action.
		
Carry out playing it on:
	repeat with N running from 1 to the number of rows in the Table of Performances:
		if the performance in row N of the Table of Performances is the noun:
			say "[directedoutput in row N of the Table of Performances][line break]".
			
Section 4 - Supplying Information

Tunechecking is an action out of world applying to nothing. Understand "performances" as tunechecking.

Carry out tunechecking:
	let found-one be a truth state;
	let found-one be false;
	repeat with N running from 1 to the number of rows in the Table of Performances:
		if the performance in row N of the Table of Performances is learned:
			now found-one is true;
	if found-one is false:
		say "[no-performances-learned]";
	otherwise:
		say "You are the master of the following performances:[line break]";
		repeat with N running from 1 to the number of rows in the Table of Performances:
			if the performance in row N of the Table of Performances is learned:
				say "[performance in row N of the Table of Performances]:   ";
				say "[description in row N of the Table of Performances][line break]".

Section 5 - Error Messages

To say no-performances-learned:
	say "You have not learned any performances."

To say cant-see-any-such:
	say "You can't see any such thing."
	
To say unlearned-performance:
	say "I'm not quite sure what you meant by that."

To say not-a-performance:
	say "[The noun] [if the noun is plural-named]are[otherwise]is[end if] not a performance."
		
To say unknown-performance:
	say "I don't know that performance."


Section 6 - The Inital Scene

Desk is a room. "You are at the far end of a musical scale that you cant quite remember. A recent boy-band rise to fame prevents all further hopes of remembering any performance  other than their smash hit that is strangely reminiscent of the flight of the bumblebees.  A large Grumpy music teacher to the east emits a faint green light.  Type 'test me' to win fast! "

A large Grumpy music teacher is a person.
[A large Grumpy music teacher is at desk.]


A large hand is a door. The large hand is locked and closed.

The large hand is scenery. The description is "It is the same green hand that has Inscribed in on the blackboard of the the words remember the scala of Sinbads Theme  to find your way forward.'"

Instead of doing anything other than examining the large hand:
	say "There is an aura of music about the large grumpity hand that prevents you doing this.  Perhaps name the notes of the musical bar: say egbdf"

Small stage is a room. "A flight of steps leads down into the darkness."

The large hand is east of the desk and west of the small stage.

StoryIntro is a scene.
StoryIntro begins when play begins.
StoryIntro ends when the large hand is open.

When StoryIntro begins:
	follow the set open sesame rule.

Every turn when StoryIntro is happening (this is the set open sesame rule):
	now current question is "";
	now current prompt is "Q>";
	now punctuation removal is true;
	ask an open question, in text mode.

A text question rule (this is the open sesame rule):
	if StoryIntro is happening:
		if the current answer is "egbdf":
			say "With an eerie musical scale, the hand falls to the side hearing songA";
			now the large hand is open;
			now the large hand is unopenable;
			exit;
		otherwise:
			parse.

Test me with "x hand / push hand / say egbdf / e / d / Peter / 34 / M / 3 / Y".

Section 7 - QUIZ AREA

Dance Floor is a room. Dance Floor is down from the small stage. The description is "The Dance Floor is just the dance floor below the stage and west of the rest of the pub.[If unvisited] An alien presence invades your mind and deep in your bones you hear a voice echo 'Welcome brave soul. You have passed the first hurdle. Before you proceed, you must answer the following questions.'"


Instead of doing anything in the Dance Floor when FillingOutLessonForms is happening:
	say "You are under a strange compulsion and unable to move."

Data is a kind of value. The data are name, age, sex, character, finalise and complete.

FillingOutLessonForms is a scene. FillingOutLessonForms begins when the location is the Dance Floor.
FillingOutLessonForms ends when LessonForms is complete.
LessonForms is data that varies.

When FillingOutLessonForms begins:
	now LessonForms is name.

Every turn during FillingOutLessonForms:
	if LessonForms is name:
		now current question is "What is your name?";
		now current prompt is "Enter your name >";
		now punctuation removal is false;
		ask a closed question, in text mode;
	if LessonForms is age:
		now current question is "How old are you?";
		ask a closed question, in number mode;
	if LessonForms is sex:
		now current question is "Do you want to play as a male, female or neutral character";
		ask a closed question, in gender mode;
	if LessonForms is character:
		now current question is "Which of the following is your preferred character type:";
		now current question menu is {"Drummer", "Keyboardist", "Lipsyncer", "Other Faker"};
		ask a closed question, in menu mode;
	if LessonForms is finalise:
		say "You have chosen:[line break]Name: [player's name][line break]Age: [Player's age][line break]Sex: [Player's sex][line break]Character: [Player's character][paragraph break]";
		now current question is "Are you happy with your responses?";
		ask a closed question, in yes/no mode.

The player's name is indexed text that varies.
The player's age is a number that varies.
The player's sex is a gender that varies.
The player's character is text that varies.

A text question rule (this is the gather name rule):
	if FillingOutLessonForms is happening and LessonForms is name:
		if the number of words in the current answer is greater than 1:
			say "I think we will simply refer to you as [word number 1 in the current answer][line break]";
			now the player's name is word number 1 in the current answer;
		otherwise:
			now the player's name is the current answer;
			say "Welcome to this venture, [player's name][line break]";
		now LessonForms is age;
		exit.

A number question rule (this is the gather age rule):
	if FillingOutLessonForms is happening and LessonForms is age:
		if the number understood is less than 5:
			say "It seems improbable that one of such tender years would be venturing so far.[line break]";
			retry;
		if the number understood is greater than 111:
			say "Let's try to be realistic about this.";
			retry;
		now the player's age is the number understood;
		say "Thank you: You are [the player's age] for the remainder of your venture into the unknown[line break]";
		now LessonForms is sex;
		exit.

A gender question rule (this is the gather gender rule):
	if FillingOutLessonForms is happening and LessonForms is sex:
		now the player's sex is the gender understood;
		say "Thank you. You are [the player's sex] gender[line break]";
		now LessonForms is character;
		exit.

A menu question rule (this is the gather character rule):
	if FillingOutLessonForms is happening and LessonForms is character:
		let temp be the number understood;
		now the player's character is entry temp in the current question menu;
		say "Thank you, you are playing as a [player's character][line break]";
		now LessonForms is finalise;
		exit.

A yes/no question rule (this is the confirm answers rule):
	if FillingOutLessonForms is happening and LessonForms is finalise:
		if the decision understood is Yes:
			say "Thank you. Venture into the unknown brave [the player's character][line break]";
			now LessonForms is complete;
			exit;
		otherwise:
			say "OK. Let's start again, shall we[line break]";
			now LessonForms is name;
			exit.

Section 7.1 - SCRIPT CONTEXT

[[[BEGIN SCRIPT CONTEXT SOURCE]]]

A scriptcontext is a kind of person. A scriptcontext has an external file called the text file. A scriptcontext can be blank or ready. A scriptcontext is usually blank. A scriptcontext has a text called the heading.

A currently erased scriptcontext is an object that varies.

To erase (pad - a scriptcontext): 
	now the currently erased scriptcontext is the pad; 
	write "[heading of the currently erased scriptcontext][paragraph break]" to the text file of the pad; 
	now the pad is blank.
To write in (pad - a scriptcontext): 
	write "[line break][player's command][line break]" to the text file of the pad; 
	now the pad is ready.
To ammend in (pad - a scriptcontext): 
	append "[line break][player's command][line break]" to the text file of the pad; 
	now the pad is ready.
To read (pad - a scriptcontext): 
	say "You read:[text of the text file of the pad]".
To ask in (pad - a scriptcontext): 
	write "[line break][player's command][line break]" to the text file of the pad; 
	say "result:[text of the text file of the pad]";
	now the pad is ready.
	
[When play begins: 
	repeat with pad running through scriptcontexts: 
		erase the pad. ]

Instead of examining a ready scriptcontext (called the pad): 
	read the pad.

Instead of examining a blank scriptcontext (called the pad): 
	say "There is nothing of note in [the pad]."
	
Target scriptcontext is an object that varies. The target scriptcontext is usually nothing.
Understand "write in [something preferably held]" as writing in. Writing in is an action applying to one thing.
Check writing in: 
	if the noun is not a scriptcontext, say "It would be better to write in a notebook." instead.
Carry out writing in: 
	now the command prompt is ">>"; 
	now the target scriptcontext is the noun.
Report writing in: 
	say "You open [the noun] and prepare to write in it."
	
After reading a command when target scriptcontext is a scriptcontext: 
	now the command prompt is ">"; 
	write in target scriptcontext; 
	now target scriptcontext is ready; 
	say "You finish writing and fold your notebook away."; 
	now the target scriptcontext is nothing; 
	reject the player's command.
Understand "erase [something preferably held]" as erasing. Erasing is an action applying to one carried thing.
Check erasing: 
	if the noun is not a scriptcontext, say "It's hard to see how." instead.
Carry out erasing: 
	erase the noun.
Report erasing: 
	say "You scrub out all the entries in [the noun]."

The player carries a scriptcontext called your beanscript. 
The heading of your beanscript is "bsh".
The file of Player's Scripts is called "bsfbeanscriptbsf". 
The text file of your beanscript is the file of Player's Scripts.

Section 7.2 - SCRIPT CONTEXT DEMO
[[
The Vestry is a room. "[Havers] hangs back by the door: the forensics expert is not finished with a preliminary examination of the body. From here you can't see much, except that the expert has peeled back and laid to one side a liturgical vestment that someone at the church used to cover the corpse until the police came. What was once a cream silk with festive Easter embroidery is now stained with blood-colored handprints."
]]

Grumpy Music Teacher is a man in the Desk. The description is "He looks glumly back. There's still a purple-ish bruise on his cheekbone from the disaster Thursday afternoon." [ Teacher is scenery.]

Teacher is carrying a scriptcontext called Teacher's notebook. The file of Teacher's Observations is called "estimates". The text file of Teacher's notebook is the file of Teacher's Observations. The heading of Teacher's notebook is "Sun. AM".


The time of day is 9:11 AM.	

Section 7.3 - QUIZ DEMO


The pub is a room. "The pub is crowded for the weekly quiz night. The barman is ready to call out the questions. Get one wrong and you will be barred from the pub for life."

Dance Floor is west from the pub.

After looking for the first time:
	follow the pub rule.

Table of quiz questions
Qn	Options	Correct
"In which year was the Battle of Hastings? (hint 2)"	{"1256", "1066", "1589", "1790"}	2
"What type of animal is Basil Brush? (hint 4)"	{"Cat", "Dog", "Rabbit", "Fox"}	4
"What is the square root of 126736? (hint 1)"	{"356", "289", "421", "321"}	1
"How many books are there in the Old Testament? (hint 2)"	{"23", "39", "37", "41"}	2
"What is a Cantaloupe? (hint 3)"	{"Animal", "Star", "Fruit", "Vegetable"}	3

The expected answer is a number that varies.

Every turn when the location is the pub (this is the pub rule):
	choose a random row in the table of quiz questions;
	now current question is the Qn entry;
	now current question menu is the Options entry;
	now expected answer is the correct entry;
	blank out the whole row;
	ask a closed question, in menu mode.

A menu question rule (this is the pub answer rule):
	if the number understood is the expected answer:
		if the table of quiz questions is not empty:
			say "Well done. You have survived to the next round.";
		otherwise:
			end the story finally saying "Congratulations you have won!" ;
	otherwise:
		end the story finally saying "You have been barred from the pub for getting a question wrong." .


Section 7.4 - PERFORMANCES DEMO

The big colorfull midi device is in the Desk. The description is "Big and colorfull with  bright green, red and yellow." Understand "bright", "red","green", and "yellow" as the midi device.

A thing can be consultable. A thing is usually not consultable.

The dusty old book is in the Desk. "A musician seems to have left his book of music performances lying out in plain view!" The description is "It's a very dusty old book. You recall seeing a musician look up various song in the book, including frotz, nitfol, and gnusto." The dusty old book is consultable.


Researching it in is an action applying to two things. Understand "research [something] in [something]" and "learn [something] from [something]" and "practice [something] from [something]" and "look up [something] in [something]" as researching it in.
[
The check rule for researching it in will reject everything, because we're going to use instead rules for researching.
]
Check researching it in:
	if the second noun is not consultable:
		say "[The second noun] [if the second noun is plural-named]are[otherwise]is[end if] not useful for researching things.";
	otherwise if the noun is not a music-performance:
		say "You leaf through the book, but find nothing on that subject.";
	otherwise:
		say "Hmm ... that page of the book seems to have been torn out."
	
Carry out researching it in:
	say "This message should never print."

Instead of consulting the book about something:
	say "So many words ... it's all just a blur."

[The consulting action, which is part of the Inform library, can be used only with topics, not with objects. Since we're making the performances objects, this action won't work with them. Thus the new action, researching it in.

When the researching it in action is being invoked, its instead rule will supersede the default instead rule contained in the extension, which prohibits any reference at all to the performance objects, other than the actions involved in playing the performances.
]

Instead of researching a music-performance in the book:
	say "Hmm ... that page of the book seems to have been torn out."

Instead of researching frotz in the book:
	if frotz is learned:
		say "You've already learned the frotz performance from the book.";
	otherwise:
		now frotz is learned;
		say "You find the page in the book that describes the frotz performance, and commit the performance to memory."

Instead of researching nifty in the book:
	if nifty is learned:
		say "You've already learned the nifty performance from the book.";
	otherwise:
		now nifty is learned;
		say "You find the page in the book that describes the nifty performance, and commit the performance to memory."
	
Instead of researching nitfol in the book:
	if nitfol is learned:
		say "You've already learned the nitfol performance from the book.";
	otherwise:
		now nitfol is learned;
		say "You find the page in the book that describes the nitfol performance, and commit the performance to memory."


Table of Performances (continued)
Performance		Output			Directedoutput	Description
nifty		"You play the nifty performance, everyone thinks you are lit up."	"You play nifty on [the second noun], and it lights up!"		"The nifty performance causes objects to light up."
frotz		"You play the frotz performance, but nothing happens."	"You play frotz on [the second noun], and you hear frotzy music."		"The frotz performance causes spherical objects to quiver."
gnusto		"You play the gnusto performance, but nothing happens."	"You play gnusto on [the second noun], but nothing happens to gnusto."	"The gnusto performance causes great gusts of wind."
nitfol		"You play the nitfol performance, but nothing happens."	"You play nitfol (song) on [the second noun], but nothing happens."		"The nitfol performance does something completely different."

nifty is a music-performance. It is everywhere.
frotz is a music-performance. It is everywhere.
gnusto is a music-performance. It is everywhere.
nitfol is a music-performance. It is everywhere.

nifty is learned.

Instead of playing frotz on the midi device:
	if frotz is not learned:
		say "[unlearned-performance]";
	otherwise if the midi device is held:
		say "The midi device quivers vigorously for a moment.";
	otherwise:
		say "The device bounces several times and then comes to rest."
	
Instead of playing nifty on the midi device:
	if nifty is not learned:
		say "[unlearned-performance]";
	otherwise if the midi device is held:
		say "The midi device quivers vigorously for a moment.";
	otherwise:
		say "The device lights several times and then comes to rest."
	
Test learning with "performances / look up nitfol in book / performances / nitfol / nitfol the device".

Test playing with "play shazam on device / play frotz on device / look up frotz in book / play frotz on device / take device / frotz device".

Test errors with "x frotz / take nitfol / frotz book / gnusto book / look up gnusto in book / frotz desk".

Test meep with "test learning / test playing / test errors".
		





