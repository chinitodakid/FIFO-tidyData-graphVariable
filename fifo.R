

# Función Average First In First Out
# Recibe como primer argumento el vector a aplicarse
# y como segundo argumento la cantidad de muestras iniciales.
# Solo acepta vectores sin valores NA.


fifo <- function(vector,buffer){
     #Se le pasa los primeros datos con los que empezará a sacar la media.
     promedios <- 0;
     varmax <- buffer;
     varmin <- 1;
     vec <- vector(mode = "numeric", length=length(buffer));     
     
     for (j in 1:(length(vector)-buffer+1)){
          er <- 1;
          for (k in varmin:varmax){
               vec[er] <- vector[k]
               er <- er + 1;
          }
          promedios[j]<- mean(vec)
          varmax <- varmax + 1;
          varmin <- varmin + 1;
     }
     promedios
     
}
