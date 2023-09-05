DiagrammeR::grViz("digraph {
                  # graph definitions
  graph [layout = dot, rankdir = TB]
  
  # node definitions
  node [shape = rectangle, style = filled, color = grey, 
  nodesep = 1, 
  fixedsize = true, 
  width = 12] 
  
  # edge definition
  edge [color = grey, arrowhead = normal, arrowtail = dot]
  
  ##### datasets
  
  step1 [label = 'Describe & Analiza la situación de las ETVS',  fillcolor =  '#0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
  
  step2 [label = 'Estratifica la ETV basado en variables relevantes',  fillcolor =  ' #0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
  
  step3 [label = 'Determinar el control de vectores en cada estrato',  fillcolor =  ' #0F9D58', color = 'white', fontcolor = 'white',fontsize = 20]

  step4 [label = 'Determine qué método(s) es(son) adecuado(s)',  fillcolor =  ' #0F9D58', color = 'white', fontcolor = 'white', fontsize = 20]
 
 # 
 step5 [label = 'Cuando el uso de insecticidas sea esencial, seleccione el(los) método(s) de aplicación',  fillcolor =  '#DB4437', color = 'white', fontcolor = 'white', fontsize = 20]
 

 # final dataset
 
 step6 [label = 'Determinar qué insecticida usar y cuándo, dónde y cómo aplicarlo',  fillcolor =  '#DB4437', color = 'white', fontcolor = 'white', fontsize = 20]
 
 ###
 step7 [label = 'Establecer productos y objetivos operativos y seleccionar métodos de seguimiento y evaluación',  fillcolor =  '#2EB67D', color = 'white', fontcolor = 'white', fontsize = 20]
 
 ##### define the relation
 
 step1 -> step2 -> step3 -> step4 -> step5 -> step6 -> step7
 
 
}")

