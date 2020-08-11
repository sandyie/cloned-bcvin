#very reliant on the magicNumbers.R script to have ran all the way through. cells_df is drawn from that script 

library(DiagrammeR)

#each cluster represents an IF statement and its dependents 
cluster1 <- unique(cells_df$i[cells_df$x == 'CH'])
cluster2 <- unique(cells_df$i[cells_df$x== 'CQ'])
cluster3 <- unique(cells_df$i[cells_df$x == 'CI'])
cluster4 <- unique(cells_df$i[cells_df$x == 'CJ'])
cluster5 <- unique(cells_df$i[cells_df$x == 'CG'])
cluster6 <- unique(cells_df$i[cells_df$x == 'CK'])
cluster7 <- unique(cells_df$i[cells_df$x == 'CF'])
cluster8 <- unique(cells_df$i[cells_df$x == 'CW'])
cluster9 <- unique(cells_df$i[cells_df$x == 'CL'])
cluster10 <- unique(cells_df$i[cells_df$x == 'CP'])
cluster11 <- unique(cells_df$i[cells_df$x == 'CR'])
cluster12 <- unique(cells_df$i[cells_df$x == 'CS'])
cluster13 <- unique(cells_df$i[cells_df$x == 'CT'])
cluster14 <- unique(cells_df$i[cells_df$x == 'CU'])

#this flowchart is very helpful in seeing the dependencies of the if statements
#which are ovals & red and what variables they are dependent on. 
#Also highlights how important each IF statement is. Looking at "IFX" in the rectangles allows you to see how many IF statements (the red ovals) are dependent on each specific IFX statement
grViz("
     
digraph {
      graph[layout = neato, overlap = false, nodesep = .5, ranksep = .25, color = crimson, splines = ortho]
      
      node[shape = rectangle, width = 2, fontname = Helvetica, fixedsize = true]
      
      edge [color = grey, arrowhead = vee, arrowtail = none]
      
      CH [label = '@@1-1']
      CQ [label = '@@1-2', color = orange]
      CI [label = '@@1-3']
      CJ [label = '@@1-4']
      CG [label = '@@1-5']
      CK [label = '@@1-6', color = purple]
      CF [label = '@@1-7', color = green]
      CW [label = '@@1-8', color = green]
      CL [label = '@@1-9']
      CP [label = '@@1-10']
      CR [label = '@@1-11']
      CS [label = '@@1-12']
      CT [label = '@@1-13']
      CU [label = '@@1-14']
      
      node[shape = oval, color = red, fixedsize = false]
      1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12;
      
      
      CH -> {@@15}
      
      
      
      CQ -> {@@2}
      
      
     
      CI -> {@@3}
      
      
    
      CJ -> {@@4}
      
      
    
      CG -> {@@5}
       
      
     
      CK -> {@@6}
       
      
      
      CF -> {@@7} [color = black]
       
      
     
      CW -> {@@8} [color = black]
       
      
     
      CL -> {@@9}
       
      
      
      CP -> {@@10}
       
      
     
      CR -> {@@11}
       
      
    
      CS -> {@@12}
       
      
      
      CT -> {@@13}
       
      
      
      CU -> {@@14}
       
      
      4, 5, 6, 7 -> CQ [color = orange]
      CQ, CI, CJ, CG -> CK [color = purple]
}
      
      [1]: refs$title[1:14]
      [2]: paste(cluster2[1:length(cluster2)], collapse = ',')
      [3]: paste(cluster3[1:length(cluster3)], collapse = ',')
      [4]: paste(cluster4[1:length(cluster4)], collapse = ',')
      [5]: paste(cluster5[1:length(cluster5)], collapse = ',')
      [6]: paste(cluster6[1:length(cluster6)], collapse = ',')
      [7]: paste(cluster7[1:length(cluster7)], collapse = ',')
      [8]: paste(cluster8[1:length(cluster8)], collapse = ',')
      [9]: paste(cluster9[1:length(cluster9)], collapse = ',')
      [10]: paste(cluster10[1:length(cluster10)], collapse = ',')
      [11]: paste(cluster11[1:length(cluster11)], collapse = ',')
      [12]: paste(cluster12[1:length(cluster12)], collapse = ',')
      [13]: paste(cluster13[1:length(cluster13)], collapse = ',')
      [14]: paste(cluster14[1:length(cluster14)], collapse = ',')
      [15]: paste(cluster1[1:length(cluster1)], collapse = ',')
          
      ")

grViz("
     
digraph {
      graph[layout = dot, rankdir = LR, nodesep = .5, ranksep = .25, color = crimson, splines = ortho]
      
      node[shape = rectangle, width = 2, fontname = Helvetica, fixedsize = true]
      
      edge [color = grey, arrowhead = vee, arrowtail = none]
      
      CH [label = '@@1-1']
      CQ [label = '@@1-2', color = orange]
      CI [label = '@@1-3']
      CJ [label = '@@1-4']
      CG [label = '@@1-5']
      CK [label = '@@1-6', color = purple]
      CF [label = '@@1-7', color = green]
      CW [label = '@@1-8', color = green]
      CL [label = '@@1-9']
      CP [label = '@@1-10']
      CR [label = '@@1-11']
      CS [label = '@@1-12']
      CT [label = '@@1-13']
      CU [label = '@@1-14']
      
      node[shape = oval, color = red, fixedsize = false]
      1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12;
      
      
      CH -> {@@15}
   
      CQ -> {@@2}

      CI -> {@@3}
  
      CJ -> {@@4}
  
      CG -> {@@5}
    
      CK -> {@@6}
    
      CF -> {@@7} [color = black]
  
      CW -> {@@8} [color = black]
   
      CL -> {@@9}
   
      CP -> {@@10}
   
      CR -> {@@11}
  
      CS -> {@@12}
    
      CT -> {@@13}

      CU -> {@@14}
       
      
      4, 5, 6, 7 -> CQ [color = orange]
      CQ, CI, CJ, CG -> CK [color = purple]
}
      
      [1]: refs$title[1:14]
      [2]: paste(cluster2[1:length(cluster2)], collapse = ',')
      [3]: paste(cluster3[1:length(cluster3)], collapse = ',')
      [4]: paste(cluster4[1:length(cluster4)], collapse = ',')
      [5]: paste(cluster5[1:length(cluster5)], collapse = ',')
      [6]: paste(cluster6[1:length(cluster6)], collapse = ',')
      [7]: paste(cluster7[1:length(cluster7)], collapse = ',')
      [8]: paste(cluster8[1:length(cluster8)], collapse = ',')
      [9]: paste(cluster9[1:length(cluster9)], collapse = ',')
      [10]: paste(cluster10[1:length(cluster10)], collapse = ',')
      [11]: paste(cluster11[1:length(cluster11)], collapse = ',')
      [12]: paste(cluster12[1:length(cluster12)], collapse = ',')
      [13]: paste(cluster13[1:length(cluster13)], collapse = ',')
      [14]: paste(cluster14[1:length(cluster14)], collapse = ',')
      [15]: paste(cluster1[1:length(cluster1)], collapse = ',')
          
      ")
