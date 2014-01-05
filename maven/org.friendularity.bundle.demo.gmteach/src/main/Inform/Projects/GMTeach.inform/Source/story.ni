
"GMTEACH" by GLUE-AI


Include questions by Michael Callaghan.

Desk is a room. "You are at the far end of a musical scale that you cant quite remember. A recent boy-band rise to fame prevents all further hopes of remembering any tune  other than their smash hit.  'Sometimes'.  It is strangely reminiscent of the flight of the bumblebees.  A large Grumpy music teacher to the east emits a faint green light.  Type 'test me' to win fast! "

A large hand is a door. The large hand is locked and closed.

The large hand is scenery. The description is "It is the same green hand that has Inscribed in on the blackboard of the the words ‘Remember the scala of Sinbad[']s Theme  to find your way forward.'"

Instead of doing anything other than examining the large hand:
	say "There is an aura of magic about the large grumpity hand that prevents you doing this.  Perhaps name the notes of the musical bar: say egbdf"

Small stage is a room. "A flight of steps leads down into the darkness."

The large hand is east of the desk and west of the small stage.

Introduction is a scene.
Introduction begins when play begins.
Introduction ends when the large hand is open.

When introduction begins:
	follow the set open sesame rule.

Every turn when introduction is happening (this is the set open sesame rule):
	now current question is "";
	now current prompt is ">";
	now punctuation removal is true;
	ask an open question, in text mode.

A text question rule (this is the open sesame rule):
	if introduction is happening:
		if the current answer is "egbdf":
			say "With an eerie musical scale, the hand falls to the side hearing songA";
			now the large hand is open;
			now the large hand is unopenable;
			exit;
		otherwise:
			parse.

Room of requirements is a room. Room of requirements is down from the small stage. The description is "The room smells musty.[If unvisited] An alien presence invades your mind and deep in your bones you hear a voice echo 'Welcome brave soul. You have passed the first hurdle. Before you proceed, you must answer the following questions.'"

Instead of doing anything in the room of requirements when Classroom is happening:
	say "You are under a strange compulsion and unable to move."

Data is a kind of value. The data are name, age, sex, character, finalise and complete.

Classroom is a scene. Classroom begins when the location is the room of requirements.
Classroom ends when lessonstage is complete.
Lessonstage is data that varies.

When Classroom begins:
	now lessonstage is name.

Every turn during Classroom:
	if lessonstage is name:
		now current question is "What is your name?";
		now current prompt is "Enter your name >";
		now punctuation removal is false;
		ask a closed question, in text mode;
	if lessonstage is age:
		now current question is "How old are you?";
		ask a closed question, in number mode;
	if lessonstage is sex:
		now current question is "Do you want to play as a male, female or neutral character";
		ask a closed question, in gender mode;
	if lessonstage is character:
		now current question is "Which of the following is your preferred character type:";
		now current question menu is {"Drummer", "Keyboardist", "Lipsyncer", "Other Faker"};
		ask a closed question, in menu mode;
	if lessonstage is finalise:
		say "You have chosen:[line break]Name: [player's name][line break]Age: [Player's age][line break]Sex: [Player's sex][line break]Character: [Player's character][paragraph break]";
		now current question is "Are you happy with your responses?";
		ask a closed question, in yes/no mode.

The player's name is indexed text that varies.
The player's age is a number that varies.
The player's sex is a gender that varies.
The player's character is text that varies.

A text question rule (this is the gather name rule):
	if Classroom is happening and lessonstage is name:
		if the number of words in the current answer is greater than 1:
			say "I think we will simply refer to you as [word number 1 in the current answer][line break]";
			now the player's name is word number 1 in the current answer;
		otherwise:
			now the player's name is the current answer;
			say "Welcome to this venture, [player's name][line break]";
		now lessonstage is age;
		exit.

A number question rule (this is the gather age rule):
	if Classroom is happening and lessonstage is age:
		if the number understood is less than 5:
			say "It seems improbable that one of such tender years would be venturing so far.[line break]";
			retry;
		if the number understood is greater than 111:
			say "Let's try to be realistic about this.";
			retry;
		now the player's age is the number understood;
		say "Thank you: You are [the player's age] for the remainder of your venture into the unknown[line break]";
		now lessonstage is sex;
		exit.

A gender question rule (this is the gather gender rule):
	if Classroom is happening and lessonstage is sex:
		now the player's sex is the gender understood;
		say "Thank you. You are [the player's sex] gender[line break]";
		now lessonstage is character;
		exit.

A menu question rule (this is the gather character rule):
	if Classroom is happening and lessonstage is character:
		let temp be the number understood;
		now the player's character is entry temp in the current question menu;
		say "Thank you, you are playing as a [player's character][line break]";
		now lessonstage is finalise;
		exit.

A yes/no question rule (this is the confirm answers rule):
	if Classroom is happening and lessonstage is finalise:
		if the decision understood is Yes:
			say "Thank you. Venture into the unknown brave [the player's character][line break]";
			now lessonstage is complete;
			exit;
		otherwise:
			say "OK. Let's start again, shall we[line break]";
			now lessonstage is name;
			exit.

Test me with "x hand / push hand/ say egbdf / e / d / Peter / 34 / M / 3 / Y".

[[[BEGIN SCRIPT CONTEXT SOURCE]]]
A scriptcontext is a kind of person. A scriptcontext has an external file called the text file. A scriptcontext can be fresh or used. A scriptcontext is usually fresh. A scriptcontext has a text called the heading.
The currently erased scriptcontext is an object that varies.

To erase (pad - a scriptcontext): 
	now the currently erased scriptcontext is the pad; 
	write "[heading of the currently erased scriptcontext][paragraph break]" to the text file of the pad; 
	now the pad is fresh.
To write in (pad - a scriptcontext): 
	write "[line break][player's command][line break]" to the text file of the pad; 
	now the pad is used.
To ammend in (pad - a scriptcontext): 
	append "[line break][player's command][line break]" to the text file of the pad; 
	now the pad is used.
To read (pad - a scriptcontext): 
	say "You read:[text of the text file of the pad]".
To ask in (pad - a scriptcontext): 
	write "[line break][player's command][line break]" to the text file of the pad; 
	say "result:[text of the text file of the pad]";
	now the pad is used.
	
[When play begins: 
	repeat with pad running through scriptcontexts: 
		erase the pad. ]

Instead of examining a used scriptcontext (called the pad): 
	read the pad.

Instead of examining a fresh scriptcontext (called the pad): 
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
	now target scriptcontext is used; 
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


The Vestry is a room. "[Havers] hangs back by the door: the forensics expert is not finished with a preliminary examination of the body. From here you can't see much, except that the expert has peeled back and laid to one side a liturgical vestment that someone at the church used to cover the corpse until the police came. What was once a cream silk with festive Easter embroidery is now stained with blood-colored handprints."
Detective Havers is a woman in the Vestry. The description is "She looks glumly back. There's still a purple-ish bruise on her cheekbone from the disaster Thursday afternoon." Havers is scenery.

Havers is carrying a scriptcontext called Barbara's notebook. The file of Barbara's Observations is called "barbara". The text file of Barbara's notebook is the file of Barbara's Observations. The heading of Barbara's notebook is "Sun. AM".


The time of day is 9:11 AM.	

[[[END SCRIPT CONTEXT SOURCE]]]

