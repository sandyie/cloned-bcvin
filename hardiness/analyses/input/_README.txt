hardiness README
Started 28 May 2019
By Lizzie
Eddited by Faith Jones 29/11/2019

These files all come from:
Bud Hardiness Chardonnay Model FINAL FINAL-v16.xlsx TAB Chardonnay Predicted LTE (3)
Received from Carl Bogdanoff unless otherwise specified. 

climhist_19812010.csv	from the K-N columns (labelled 1981-2010 Historical Data (Penticton WS))

envcanada_penticton.csv is the 'Env Canada Penticton Weather' tab from 2012-2018 PENTICTON WEATHER.xlsx and there were a bunch of multibyte strings to fix:
à -> datqual_a
^ -> incmpdat
Ê -> prelimqual
[empty] -> empty
ÁC -> C


The following csv files:
	budhardiness2012to13
	budhardiness2013to2013
	budhardiness2014to15
	budhardiness2015to16
	budhardiness2016to17
	budhardiness2017to18
	budhardiness2018to19
... all come from 'Bud Hardiness Chardonnay Model FINAL FINAL-v16.xlsx' under the `Hardiness Tables 2012-18' tab, then under:
2012 - 2013 Winter Grape Bud Hardiness Okanagan Valley BC
I redid the headers for 2014-2015 onward (so that they were just one line).
Dates for 2016-2017 hardiness are missing, so I guessed the dates from columns Q-AB (row 178) were the correct dates. 


Also includied is the CSV file
	2012-2018_PENTICTON_WEATHER_EM.csv
this is needed for the missing temperatures to be filled correctly. Carl said to Lizzie:
"Missing Penticton weather data was usually filled in with Env Canada Summerland weather data.  You might already have this, 
	but if not, attached is the weather data I used. (ATTACHED: 2012-2018 PENTICTON WEATHER EM.xslx)"
The correct sheet in this excel spreadsheet is Pent Max Min Mean 1, 2 & 3 da



 
QUESTIONS for Carl ...
