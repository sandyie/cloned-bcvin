###magic numbers (magicNum) from Excel sheet for the 12 columns (CL to CV) IF statements. Many are broken into 6 different portions as Carl had divided his IF
###statements. Refer to Chard Model Instructions May 2020, 11(a - f).
#used in parse_vals
regex1 <- "C[A-Z][0-9][0-9]+"
regex2 <- "C[A-Z][0-9][0-9][0-9]+"
regex3 <- "[\\(|\\)]"
regex4 <- "[A-Z]+"
regex5 <- "[<|>|\\*]"
regex6 <- "\\:"
regex7 <- "\\="
regex8 <- "\\+"

#used in parse_cells
regex9 <- "C[A-Z]+"

parse_cells <- function(IF_statement, StringForNamingObject){
  assign(
    x = paste0("cells", StringForNamingObject),
    value = str_extract_all(IF_statement, paste0(regex9)),
    envir = .GlobalEnv
    
  )
}

parse_vals <- function(IF_statement, StringForNamingObject){
  assign(
    x = paste0("magicNum", StringForNamingObject),
    value = gsub(paste(regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, sep = "|"), '', IF_statement) %>%   
      strsplit(. , ",") %>%
      as.data.frame(col.names = "value") %>% 
      type_convert(),
    envir = .GlobalEnv
  )
  
}

#all IF statements are copy / pasted from Carl's excel sheet. IF1_1 is the first column of if statements (CL) in the first section as defined by Carl
#These are repeated for each season 
IF1_1 <- "IF(CH50<-7,3,IF(CH50<-6,2.6,IF(CH50<-5,2.2,IF(CH50<-4,1.6,IF(CH50<-3,1.2,IF(CH50<-2,1.15,IF(CH50<-1,1.1,IF(CH50<0,1,0))))))))"   
IF1_2 <- "IF(CH71<-7,2.8,IF(CH71<-6,2.4,IF(CH71<-5,2,IF(CH71<-4,1.6,IF(CH71<-3,1.45,IF(CH71<-2,1.3,IF(CH71<-1,1.1,IF(CH71<0,1,0))))))))"
IF1_3 <- "IF(CH112<-4,-1.6,IF(CH112<-3,-1.5,IF(CH112<-2,-1.4,IF(CH112<-1,-1.3,IF(CH112<0,-1.2,0)))))"
IF1_4 <- "IF(CH140<-9,-3,IF(CH140<-7,-2,IF(CH140<-4,-1.7,IF(CH140<-3,-1.6,IF(CH140<-2,-1.5,IF(CH140<-1,-1.25,IF(CH140<0,-1,0)))))))"
IF1_5 <- "IF(CH175<-9,-2,IF(CH175<-7,-1.8,IF(CH175<-5,-1.5,IF(CH175<-4,-1.3,IF(CH175<-3,-1,IF(CH175<-2,-0.5,IF(CH175<-1,0.5,0)))))))"
IF1_6 <- "IF(CH205<-9,-1,IF(CH205<-7,-0.5,IF(CH205<-5,-0.1,IF(CH205<-4,0.2,IF(CH205<-3,0.8,IF(CH205<-2,1,IF(CH205<-1,1.05,0)))))))"

IF2_1 <- "IF(CH28>6,0.2,IF(CH28>5,0.4,IF(CH28>4,0.6,IF(CH28>3,0.75,IF(CH28>2,0.9,IF(CH28>1,0.95,IF(CH28>0,1,0)))))))"
IF2_2 <- "IF(CH56>6,0.25,IF(CH56>5,0.45,IF(CH56>4,0.6,IF(CH56>3,0.7,IF(CH56>2,0.9,IF(CH56>1,0.95,IF(CH56>0,1,0)))))))"
IF2_3 <- "IF(CH104>5,1,IF(CH104>4,0.5,IF(CH104>3,0,IF(CH104>2,-0.5,IF(CH104>1,-1,IF(CH104>0,-1.1,0))))))"
IF2_4 <- "IF(CH134>5,1.3,IF(CH134>4,1.1,IF(CH134>3,1,IF(CH134>2,0.6,IF(CH134>1,0.2,IF(CH134>0,-0.5,0))))))"
IF2_5 <- "IF(CH165>6,3.2,IF(CH165>4,2,IF(CH165>3,1.6,IF(CH165>2,1.4,IF(CH165>1,1.2,IF(CH165>0,1.1,IF(CH165>-1,1.05,0)))))))"
IF2_6 <- "IF(CH188>7,4,IF(CH188>6,3,IF(CH188>4,2.4,IF(CH188>3,1.8,IF(CH188>2,1.4,IF(CH188>1,1.2,IF(CH188>0,1.15,IF(CH188>-1,1.1,0))))))))" 

IF3_1 <- "IF((CQ25<-24),(((CI26+CJ26)*CG26*0.1)+CK25),(((CI26+CJ26)*CG26)+CK25))"
IF3_2 <- "IF((CQ55<-24),(((CI56+CJ56)*CG56*0.1)+CK55),(((CI56+CJ56)*CG56)+CK55))"
IF3_7th <- "IF(CK103<-24.5,-24.5,(CK103+CL103))" ########################################################################################don't forget to add this one to the loop manually
IF3_3 <- "IF(AND((CI105+CJ105)<0,CK104<=-24.5),(((CI105+CJ105)*CG105*0.2)+CK104),(((CI105+CJ105)*CG105)+CK104))"
IF3_4 <- "IF(AND((CI134+CJ134)<0,CK133<=-24.5),(((CI134+CJ134)*CG134*0.3)+CK133),(((CI134+CJ134)*CG134)+CK133))"
IF3_5 <- "IF(AND((CI165+CJ165)<0,CK164<=-24.5),(((CI165+CJ165)*CG165*0.3)+CK164),(((CI165+CJ165)*CG165)+CK164))"
IF3_6 <- "IF(AND((CI188+CJ188)<0,CK187<=-24.5),(((CI188+CJ188)*CG188*0.3)+CK187),(((CI188+CJ188)*CG188)+CK187))"

IF4_1 <- "IF(AND(CQ25<(CF26-3),CH26<-5),CG26*-0.1,IF(AND(CQ25<(CF26-3),CH26<-3),CG26*-0.2,IF(AND(CQ25<(CF26-3),CH26<-1),CG26*-0.3,0)))"
IF4_2 <- "IF(AND(CQ55<(CF56-2),CH56<-5),CG56*-0.1,IF(AND(CQ55<(CF56-2),CH56<-3),CG56*-0.2,IF(AND(CQ55<(CF56-2),CH56<-1),CG56*-0.3,0)))" 
IF4_3 <- "IF(AND(CQ103<(CF104-2),CH104<-5),CG104*0.1,IF(AND(CQ103<(CF104-2),CH104<-3),CG104*0.2,IF(AND(CQ103<(CF104-2),CH104<-1),CG104*0.3,0)))"
IF4_4 <- "IF(AND(CQ133<(CF134-2),CH134<-5),CG134*0.1,IF(AND(CQ133<(CF134-2),CH134<-3),CG134*0.2,IF(AND(CQ133<(CF134-2),CH134<-1),CG134*0.3,0)))"
IF4_5 <- "IF(AND(CQ164<(CF165-2),CH165<-5),CG165*0.1,IF(AND(CQ164<(CF165-2),CH165<-3),CG165*0.2,IF(AND(CQ164<(CF165-2),CH165<-1),CG165*0.3,0)))"
# IF4_6 ONLY 5 Sections for this column

IF5_1 <- "IF(AND(CQ25<(CF26-3),CH26>5),CG26*-0.3,IF(AND(CQ25<(CF26-3),CH26>3),CG26*-0.2,IF(AND(CQ25<(CF26-3),CH26>1),CG26*-0.1,0)))"
IF5_2 <- "IF(AND(CQ55<(CF56-2),CH56>5),CG56*-0.3,IF(AND(CQ55<(CF56-2),CH56>3),CG56*-0.2,IF(AND(CQ55<(CF56-2),CH56>1),CG56*-0.1,0)))"
IF5_3 <- "IF(AND(CQ103<(CF104-2),CH104>5),CG104*0.3,IF(AND(CQ103<(CF104-2),CH104>3),CG104*0.2,IF(AND(CQ103<(CF104-2),CH104>1),CG104*0.1,0)))"
IF5_4 <- "IF(AND(CQ133<(CF134-2),CH134>5),CG134*0.3,IF(AND(CQ133<(CF134-2),CH134>3),CG134*0.2,IF(AND(CQ133<(CF134-2),CH134>1),CG134*0.1,0)))"
IF5_5 <- "IF(AND(CQ164<(CF165-2),CH165>5),CG165*0.3,IF(AND(CQ164<(CF165-2),CH165>3),CG165*0.2,IF(AND(CQ164<(CF165-2),CH165>1),CG165*0.1,0)))"
# IF5_6 ONLY 5 sections for this column

IF6_1 <- "IF(AND(CQ25>(CF26+3),CH26>5),CG26*0.3,IF(AND(CQ25>(CF26+3),CH26>3),CG26*0.2,IF(AND(CQ25>(CF26+3),CH26>1),CG26*0.1,0)))"
IF6_2 <- "IF(AND(CQ55>(CF56+2),CH56>5),CG56*0.3,IF(AND(CQ55>(CF56+2),CH56>3),CG56*0.2,IF(AND(CQ55>(CF56+2),CH56>1),CG56*0.1,0)))"
IF6_3 <- "IF(AND(CQ103>(CF104+2),CH104>5),CG104*-0.3,IF(AND(CQ103>(CF104+2),CH104>3),CG104*-0.2,IF(AND(CQ103>(CF104+2),CH104>1),CG104*-0.1,0)))"
IF6_4 <- "IF(AND(CQ133>(CF134+2),CH134>5),CG134*-0.3,IF(AND(CQ133>(CF134+2),CH134>3),CG134*-0.2,IF(AND(CQ133>(CF134+2),CH134>1),CG134*-0.1,0)))"
IF6_5 <- "IF(AND(CQ164>(CF165+2),CH165>5),CG165*-0.3,IF(AND(CQ164>(CF165+2),CH165>3),CG165*-0.2,IF(AND(CQ164>(CF165+2),CH165>1),CG165*-0.1,0)))"
# IF6_6 ONLY 5 sections for this column

IF7_1 <- "IF(AND(CQ25>(CF26+3),CH26<-5),CG26*0.1,IF(AND(CQ25>(CF26+3),CH26<-3),CG26*0.2,IF(AND(CQ25>(CF26+3),CH26<-1),CG26*0.3,0)))"
IF7_2 <- "IF(AND(CQ55>(CF56+2),CH56<-5),CG56*0.1,IF(AND(CQ55>(CF56+2),CH56<-3),CG56*0.2,IF(AND(CQ55>(CF56+2),CH56<-1),CG56*0.3,0)))"
IF7_3 <- "IF(AND(CQ103>(CF104+2),CH104<-5),CG104*-0.1,IF(AND(CQ103>(CF104+2),CH104<-3),CG104*-0.2,IF(AND(CQ103>(CF104+2),CH104<-1),CG104*-0.3,0)))"
IF7_4 <- "IF(AND(CQ133>(CF134+2),CH134<-5),CG134*-0.1,IF(AND(CQ133>(CF134+2),CH134<-3),CG134*-0.2,IF(AND(CQ133>(CF134+2),CH134<-1),CG134*-0.3,0)))"
IF7_5 <- "IF(AND(CQ164>(CF165+2),CH165<-5),CG165*-0.1,IF(AND(CQ164>(CF165+2),CH165<-3),CG165*-0.2,IF(AND(CQ164>(CF165+2),CH165<-1),CG165*-0.3,0)))"
# IF7_6 ONLY 5 sections for this column 

IF8_1 <- "IF(AND(CW25<-21,CH26<0),(SUM(CL26:CP26)*0.6),(SUM(CL26:CP26)))"
IF8_2 <- "IF(AND(CW55<-21,CH56<0),(SUM(CL56:CP56)*0.7),(SUM(CL56:CP56)))"
IF8_3 <- "IF(AND(CW103<-23,CH104<0),(SUM(CL104:CP104)*0.7),(SUM(CL104:CP104)))"
IF8_4 <- "IF(AND(CW133<-23,CH134<0),(SUM(CL134:CP134)*0.7),(SUM(CL134:CP134)))"
IF8_5 <- "IF(AND(CW164<-23,CH165<-2),(SUM(CL165:CP165)*0.6),(SUM(CL165:CP165)))"
IF8_6 <- "IF(AND(CW187<-23,CH188<-2),(SUM(CL188:CP188)*0.6),(SUM(CL188:CP188)))"

# IF9_1 Nothing in 1st period for this column 
IF9_2 <- "IF((CW55<-23.5),(SUM(CL56:CP56)*0.3),IF((CW55<-22.5),(SUM(CL56:CP56)*0.7),CR56))"
IF9_3 <- "IF(AND(CW103<-25,CH104<0),(SUM(CL104:CP104)*0.3),(CR104))"
IF9_4 <- "IF(AND(CW133<-25,CH134<0),(SUM(CL134:CP134)*0.3),(CR134))"
IF9_5 <- "IF(AND(CW164<-24,CH165<0),(SUM(CL165:CP165)*0.4),(CR165))"
IF9_6 <- "IF(AND(CW187<-24,CH188<0),(SUM(CL188:CP188)*0.4),(CR188))"

# IF10_1 Nothing in 1st period for this column
IF10_2 <- "IF(AND(CW55>(CF56+1.5),CH56>0),CS56*1.5,CS56*1)"
# IF10_3 Nothing in 3rd period for this column 
IF10_4 <- "IF(AND(CW133<-24.5,CH134>0),(SUM(CL134:CP134)*4),(CS134))"
IF10_5 <- "IF(AND(CW164<-23,CH165>-2),(SUM(CL165:CP165)*1.7),(CS165))"
IF10_6 <- "IF(AND(CW187<-23,CH188>-2),(SUM(CL188:CP188)*1.7),(CS188))"

# IF11_1 nothing in 1st period
# IF11_2 nothing in 2nd period
# IF11_3 nothing in 3rd period
# IF11_4 nothing in 4th period
# IF11_5 nothing in 5th period
IF11_6 <- "=IF(AND((CF188-2)>CW187,(CH188>0)),(CT188*1.6),CT188)"

# IF12_1 nothing in the 1st period
# IF12_2 nothing in the 2nd period
# IF12_3 nothing in the 3rd period
# IF12_4 nothing in the 4th period
# IF12_5 nothing in the 5th period
IF12_6 <- "IF(AND(CG188>0.3,CW187<-12,CH188>1),(CU188*1.2),CU188)"

for(i in 1:12){ #running the parser for all IF statements
  for(j in 1:6) {
    if((i == 4 || i == 5|| i == 6 || i == 7) && j ==6){
      next
    }else if(i == 9 && j == 1){
      next
    }else if(i == 10 && (j == 1 || j == 3)){
      next
    }else if((i == 11 || i == 12) && j != 6){
      next
    }
    parse_vals(
      get(paste0("IF", i, "_", j)),
      paste0(i, "_", j)
    )
    parse_cells(
      get(paste0("IF", i, "_", j)),
      paste0(i, "_", j)
    )
  }
} #outputs IF_df_1_X where X <- 1:6

parse_vals(IF3_7th, "3_dec7th") #didn't fit the pattern because it was an odd one out
parse_cells(IF3_7th, "3_dec7th")

num <- 0
datalist = list()

for(i in 1:12){ #creating a DF to list all of unique cell columns so I can find out their origin in the excel sheet 
  for(j in 1:6) {
    if((i == 4 || i == 5|| i == 6 || i == 7) && j ==6){
      next
    }else if(i == 9 && j == 1){
      next
    }else if(i == 10 && (j == 1 || j == 3)){
      next
    }else if((i == 11 || i == 12) && j != 6){
      next
    }
    num <- num + 1
    dat <- data.frame(x = unique(unlist(get(paste0("cells", i, "_", j)))))
    dat$i <- i
    dat$j <- j
    datalist[[num]] <- dat
    
  }
} 

cells_df <- do.call(rbind, datalist)

cells_df_unique <- unique(cells_df$x) #all of the column names referenced, CV was never referenced but it exists & has an if statement
cells_df_unique_ref <- c("tdiff", "Pred LTE 2", "IF1", "IF2", "est LTE/day", "Pred LTE 1 / IF3", "Estimate LTE / START", "Pred LTE / STOP", "Daily LTE Change", "IF7", "IF8", "IF9", "IF10","IF11" ) #only useful for the flowchart
refs <- data.frame("col" = cells_df_unique,"title" = cells_df_unique_ref) #makes the process in excel easier so you don't have to scroll through the infinite sheet to find a name
