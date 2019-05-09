#' Creates the CEG starting from a staged tree
#'
#' @param object a staged event tree 
#' @return a ceg object
#' @export
toCeg.sevt <- function(object)
{
  levels <- sapply(object$tree, length)[-1]
  stages <- object$stages
  position <- stages
  temp_position <- position
  
  for(i in (length(stages)-1):1)  # quanti livelli dell'albero bisogna ispezionare, tranne l'ultimo che è = a stages
  {
    unique_stages <- unique(stages[[i]])
    pos <- 1
    count <- 0  # se non entra mai nell'if vuol dire che per quel livello i tutti i nodi sono in posizioni diverse.
    
    for(j in 1:length(unique_stages)) # ispeziono i valori diversi assunti dagli stage al livello i-esimo
    {
      for(k in 1:(length(stages[[i]])-1)) # stage passo i: ciclo su tutti i valori che ho (anche ripetuti) in stage[[i]]
      {
        position1 <- temp_position[[i+1]][(levels[i] * (k-1) + 1):(levels[i] * k)]
        
        for(l in (k+1):length(stages[[i]]))  # stage passo i: ciclo su tutti i valori che ho (anche ripetuti) in stage[[i]]
        {
          position2 <- temp_position[[i+1]][(levels[i] * (l-1) + 1):(levels[i] * l)]
          
          if(stages[[i]][k] == unique_stages[j] & stages[[i]][l] == unique_stages[j] & 
             sum((position1 == position2) * 1) == levels[i])  # se sono nello stesso stage e dopo sono nella stessa posizione
          {
            count <- count + 1
            position[[i]][l] <- pos
            position[[i]][k] <- pos
            stages[[i]][l] <- "NA"
            temp_position[[i+1]][(levels[i] * (l-1) + 1):(levels[i] * l)] <- rep("NA", levels[i])
          }
        }
        pos <- pos + 1
      }
    }
    
    temp_position[[i]] <- position[[i]]  # sovrascrivo alla fine del ciclo il risultato aggiornato
    
    ##### rinominare 1,2,3,... le posizioni.
    unique_pos <- unique(position[[i]])
    ord_pos <- paste(c(1:length(unique_pos)), sep = "")
    
    for(p in 1:length(unique_pos))
    {
      for(q in 1:length(position[[i]]))
      {
        if(position[[i]][q] == unique_pos[p])
        {
          position[[i]][q] <- ord_pos[p]
        }
      }
    }
    #####
    
    if(count == 0)
    {
      position[[i]] <- paste(c(1:length(position[[i]])), sep = "")
      temp_position[[i]] <- paste(c(1:length(temp_position[[i]])), sep = "")
      
      for(u in i:1)
      {
        position[[u]] <- paste(c(1:length(position[[u]])), sep = "")
      }
      break
    }
  }
  object$position <- position
  class(object) <- c("ceg", class(object))
  return(object)
}