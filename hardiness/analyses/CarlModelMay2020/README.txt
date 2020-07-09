Converting Carl's May Model into R:

There are 4 essential .R files for this to run. 

1. helpfulFunctions.R
2. IFstatements.R
3. magicNumbers.R
4. main.R

helpfulFunctions - There are a few functions defined in this script. The most notable - finalLTEpredictions. This function
returns a dataframe which contains the final predicted LTE values for each day. Reference main.R for how the function is called. 
All possible versions of the function call are included near the bottom of main.R. 

IFstatements.R - Contains all of the IF statements. The IF statements horrendously reference each other (see flowchart to see the interconnections)

magicNumbers.R - This is where all of the numbers that act on values come from. Each IF statement had anywhere between 10 and 17 of these values. 
Mostly created because I didn't trust myself to visually parse all of those numbers correctly & to make debugging less painful.

main.R - Where all of the functions are called and where data is opened. The meat and potatoes of the script is from line 136 - 150. Everything before
these lines is required to be ran before calling the functions.

This is a brief description that will be supplemented in about a week or so.