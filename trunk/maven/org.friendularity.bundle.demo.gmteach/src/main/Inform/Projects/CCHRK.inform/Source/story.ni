"CCHRK" by Glue-ai

Include questions by Michael Callaghan.

The pub is a room. "The pub is crowded for the weekly quiz night. The barman is ready to call out the questions. Get one wrong and you will be barred from the pub for life."

After looking for the first time:
	follow the pub rule.

Table of quiz questions
Qn  	Options  	Correct  
"In which year was the Battle of Hastings"  	{"1256", "1066", "1589", "1790"}  	2  
"What type of animal is Basil Brush"  	{"Cat", "Dog", "Rabbit", "Fox"}  	4  
"What is the square root of 126736?"  	{"356", "289", "421", "321"}  	1  
"How many books are there in the Old Testament"  	{"23", "39", "37", "41"}  	2  
"What is a Cantaloupe?"  	{"Animal", "Star", "Fruit", "Vegetable"}  	3  

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