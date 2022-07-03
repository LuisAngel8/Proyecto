x = c("C", "A", "T")
y = c("C", "T")

allAlignment = function(x,y){
  "
  Funcion principal
  "
  ff = function(x,y){
    "
    Funcion anidada
    "
    #if (length(x) == 0 && length(y) == 0){
    #Aquí va el yield deque()
    
    scenarios = data.frame()
    if (length(x) > 0 && length(y) > 0){
      p1 = c(x[1], paste0(x[2], x[3]), y[1], y[2])
      scenarios = append(scenarios, list(p1))
    }
    if (length(x) > 0){
      p2 = c(x[1], paste0(x[2],x[3]), "None", paste0(y[1],y[2]))
      scenarios = append(scenarios, list(p2))
    }
    if (length(y) > 0){
      p3 = c("None",paste0(x[1], x[2], x[3]), y[1], y[2])
      scenarios = append(scenarios, list(p3))
    }
    
    for (xh in scenarios[[2]]){
      for (xt in xh){
        for (yh in xt){
          for (yt in yh){
            if (xh == xt && xt == yh && yh == yt){
              for (alignment in ff(xt, yt)){
                append(alignment, c(xh, yh), after = 0)
                #print(scenarios[[1]]) # Aquí va el alignment
              } 
            }
          }
        }
      }
    }
  alignments
  return()
  }
alignments = ff(seq(length(x)), seq(length(y)))
return(ff(alignments))
}