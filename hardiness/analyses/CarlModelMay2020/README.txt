Converting Carl's May Model into R:

There are 4 essential .R files for this to run. 
If verifying the resemblance of Carl's May 2020 model and the model after it was moved into R reference: bcvin/hardiness/dashboard/verifyingModel.R

1. helpfulFunctions.R
2. IFstatements.R
3. magicNumbers.R
4. main.R

helpfulFunctions - There are a few functions defined in this script. The most notable - finalLTEpredictions. This function
returns a dataframe which contains the final predicted LTE values for each day. Reference main.R for how the function is called. 
All possible versions of the function call are included near the bottom of main.R. 

IFstatements.R - Contains all of the IF statements. The IF statements reference are very intertwined (see flowchart to see the interconnectivity).

magicNumbers.R - This is where all of the coefficients that rescaled intermediate values or values that were used as thresholds. Each IF statement had anywhere between 10 and 17 of these values. 
Mostly created because I didn't trust myself to visually parse all of those numbers correctly & to make debugging less painful.

main.R - Where all of the functions are called and where data is opened/manipulated. The meat and potatoes of the script is from line 136 - 150. Everything before
these lines is required to be ran before calling the functions.
	
Started July 2020 by Adam
adamfong888@gmail.com