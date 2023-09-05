
DiagrammeR::grViz("
digraph circo {
graph [layout = circo]
node [shape = circle,
      style = filled, nodesep = 1, fixedsize = false, width = 2]
a [label = 'Uso Sensato de los Insecticidas',  fillcolor =  '#767676', color = 'white', fontcolor = 'white', fontsize = 14]

b [label = 'Que Aplicar',  fillcolor =  '#ECB22E', color = 'white', fontcolor = 'white',  fontsize = 20]
c [label = 'Cuando Aplicar',  fillcolor =  '#E01E5A', color = 'white', fontcolor = 'white', fontsize = 20]

d [label = 'Con que Aplicar',  fillcolor =  '#0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
e [label = 'Como Aplicar',  fillcolor =  '#36C5F0', color = 'white', fontcolor = 'white', fontsize = 20]
f [label = 'Donde Aplicar',  fillcolor =  '#FF5A5F', color = 'white', fontcolor = 'white', fontsize = 20]


edge [color = grey]


a -> {b c d e f}
}")


DiagrammeR::grViz("digraph {
                  # graph definitions
  graph [layout = dot, rankdir = TB]
  
  # node definitions
  node [shape = rectangle, style = filled, color = grey, 
  nodesep = 1, 
  fixedsize = true, 
  width = 8] 
  
  # edge definition
  edge [color = grey, arrowhead = normal, arrowtail = dot]
  
  ##### fuentes de información
  
  inf [label = 'Fuentes de Información',  fillcolor =  '#DB4437', color = 'white', fontcolor = 'white']
  
  
  ##### datasets
  
  step1 [label = 'Describe & Analiza la situación de las ETVS',  fillcolor =  '#0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
  
  step2 [label = 'Estratifica la ETV basado en variables relevantes',  fillcolor =  ' #0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
  
  step3 [label = 'Determinar si hay un papel para el control de vectores en cada estrato',  fillcolor =  ' #0F9D58', color = 'white', fontcolor = 'white',fontsize = 20]

  step4 [label = 'Si hay una función para el control de vectores, determine qué método(s) es(son) adecuado(s)',  fillcolor =  ' #0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
 
 # 
 step5 [label = 'Anopheles',  fillcolor =  'orange', color = 'white', fontcolor = 'black', fontsize = 20]
 

 # final dataset
 
 step6 [label = 'Culicidae_mx_dataset',  fillcolor =  'DodgerBlue', color = 'white', fontcolor = 'white', fontsize = 20]
 
 ###
 step7 [label = 'Aedes agypti',  fillcolor =  '#2EB67D', color = 'white', fontcolor = 'white', fontsize = 20]
 
 ##### define the relation
 
 inf->step1 -> step2 -> step3 -> step4 -> step5 -> step6 -> step7
 
 
}")
