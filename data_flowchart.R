library(DiagrammeR)


grViz("
digraph {

graph[layout = dot, rankdir = LR, splines = line]

node[ height = 6]

a [shape = folder, height = 5, width = 18, fixedsize = true, label = '@@1', fontname = Helvetica, fontsize = 100, style = filled, fillcolor = linen]

# node def for 
node [fontsize = 100, style = filled, fillcolor = orange, fontname = Helvetica, color = black]

b [label = '@@2-1']
c [label = '@@2-2']
d [label = '@@2-3']
e [label = '@@2-4']
f [label = '@@2-5']

node [shape = oval, fontsize = 100, fontname = Helvetica, fillcolor = yellow, label = '@@3']
81; 82; 83; 84; 85

node [shape = diamond, fontsize = 100, fontname = Helvetica, fillcolor = green, height = .5, label = 'Final Climate Layer']
final

# node definitions for data years (all values that aren't defined)
node [shape = point, fillcolor = black, label = '', fixedsize = true, width = .75]



edge [color = black, arrowsize = 10, arrowhead = vee, penwidth = 8, minlen = 5]
a -> b [taillabel = 'ClimateBC outputs data for \n 5 different model members.',  fontsize = 100, fontname = Helvetica, style = dashed, fontcolor = grey]
a -> {c d e f}

edge [color = black, arrowhead = none, penwidth = 3, minlen = 8]
b -> g [headlabel = 'Each member contains \n 20 years of data. \n Each dot represents one year.', fontsize = 100, fontname = Helvetica,len = .25 , fontcolor = grey]
b -> {h i j k l m n o p q r s t u v w x y z} 
c -> {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20} 
d -> {21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40} 
e -> {41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60}
f -> {61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80}


g -> 81 [taillabel = 'Data from all members \n are used to calculate \n mean & variation \n for the final layer.', fontsize= 100, fontname = Helvetica, fontcolor = grey]
{h i j k l m n o p q r s t u v w x y z} -> 81 
{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20} -> 82 
{21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40} -> 83 
{41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60} -> 84 
{61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80} -> 85 

edge [arrowhead = vee, arrowsize = 10, penwidth = 8]
{82 83 84 85} -> final 
81 -> final 
}

[1]: 'GCM: CanESM2 RCP 8.5'
[2]: paste0('Member: r', 1:5, '1i1p1')
[3]: '20 Year Avg'


")







