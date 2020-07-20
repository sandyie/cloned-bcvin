#Script & tips to compare with Carl's model
#No source files because it's best to have all of the dependent files open to reference them.
#USEFUL: calculate_day(). The R model uses daynum values between 1-176. These correspond to different days in the year.
#you can use calculate_day(day, month) to see what the value is to make it easier on yourself 


####I'm using this block to compare to my outputs of the same period of time 
#finalLTEpredictions_test(-10.54, -10.59, -10.59, "2012to13")
#to check the values I calculated, just run the mainDashboard.R script in hardiness/dashboard to check the non "_test" version of the predictions
finalLTEpredictions_test(-13.6, -13.6, -13.6, "2013to14") 
finalLTEpredictions_test(-9.77, -10.18, -10.18, "2014to15")
finalLTEpredictions_test(-12.84, -12.84, -12.84, "2015to16")
finalLTEpredictions_test(-11.84, -11.99, -11.99, "2016to17")
finalLTEpredictions_test(-13.25, -13.25, -13.25, "2017to18")
finalLTEpredictions_test(-13.84, -13.84, -13.84, "2018to19")

predLTE_test_combined_2013to14 #these should be very similar to Carl's model. The way I tested was by using 
predLTE_test_combined_2014to15
predLTE_test_combined_2015to16
predLTE_test_combined_2016to17
predLTE_test_combined_2017to18
predLTE_test_combined_2018to19

df_testing <- data.frame(day = c(21, 8, 7, 7, 1, 11), #October 20th, December 8th, Jan 7th, Feb 7th, March 1st
                         month = c(10, 12, 1, 2, 3, 4))

sectionStarts <- calculate_day_v(df_testing$day, df_testing$month) #can use this to query the start of each section in Carl's model. He added a nice grid to help visualize

#Can check each column if you want but all of them add to the third column "predLTE3$predLTEfinal." In Carl's model, this is the far right pink column labeled Final Predicted LTE in each year block. 
#tdiff and twoDayAvg column are good to check to see that the scraped data and the data in the git repo are identical. 
#scale column are the "Initial IF statements" in Carl's model added together. Also worthwhile to see that boundary values may differ in the R version and Excel version 

predLTE_test_combined_2013to14[sectionStarts ,]
predLTE_test_combined_2014to15[sectionStarts ,]
predLTE_test_combined_2015to16[sectionStarts ,]
predLTE_test_combined_2016to17[sectionStarts ,]
predLTE_test_combined_2017to18[sectionStarts ,]
predLTE_test_combined_2018to19[sectionStarts ,]
