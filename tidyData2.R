

# Función para graficar tres variables contra otra variable.
# No necesita ningun argumento.
# Se necesita tener la función "fifo.R" en el working directory.
# Todas las columnas relacionadas con Date y Time, están para uso opcional.

tidyData <- function()
{
     source("fifo.R")
     
     #Lectura del archivo y de los valores de las variables del molino
     raw_data <- read.csv("2015-03-03-10-45-11-151.csv")
     col_date <- as.character(raw_data$Date)
     col_time <- as.character(raw_data$Time)
     col_tagname <- as.character(raw_data$Tagname)
     col_value <- as.numeric(as.character(raw_data$Value))
     
     
     #Arreglo de la columna Date y Time:
     date_time <- paste(col_date,col_time); date_time <- gsub("mar","03",date_time)
     date_time <- gsub("2015","15",date_time); date_time <- gsub(" a. m.","", date_time)
      
     #Creacion de lista donde se guardarán vectores lógicos
     logical_vectors <- vector(mode= "list", length= length(levels(raw_data$Tagname)))
     value_vectors <- vector(mode= "list", length= length(levels(raw_data$Tagname)))
     tagname_vectors <- vector("list", length(levels(raw_data$Tagname)))
     

     for (i in 1:length(levels(raw_data$Tagname)))
     {
          logical_vectors[[i]] <- col_tagname == levels(raw_data$Tagname)[i]
          tagname_vectors[[i]] <- col_tagname[logical_vectors[[i]]]
          value_vectors[[i]] <- col_value[logical_vectors[[i]]]
     }
     
     print(levels(raw_data$Tagname))
     x <- as.numeric(readline("Ingrese el numero de la variable horizontal: "))
     y1 <- as.numeric(readline("Ingrese el numero de la variable vertical: "))
     y2 <- as.numeric(readline("Ingrese el numero de otra variable vertical: "))
     y3 <- as.numeric(readline("Ingrese el numero de otra variable vertical: "))
     
     #Variables a graficar, aplicando filtro "fifo" 
     
     val_x <- value_vectors[[x]]
     val_y1 <- value_vectors[[y1]]
     val_y2 <- value_vectors[[y2]]
     val_y3 <- value_vectors[[y3]]
     
     frame <- data.frame(val_x,val_y1,val_y2,val_y3)
     ordered_frame <- frame[order(frame[,1],frame[,2],frame[,3],frame[,4]),]
     
     val2x <- ordered_frame[,1]
     val2y <- ordered_frame[,2]
     val2y2 <- ordered_frame[,3]
     val2y3 <- ordered_frame[,4]
     
     val2x <- fifo(val2x,300)
     val2y <- fifo(val2y,300)
     val2y2 <- fifo(val2y2,300)
     val2y3 <- fifo(val2y3,300)
     
     frame2 <- data.frame(val2x,val2y)

     plot(val2x,val2y,type="l")
     par(new=TRUE)
     plot(val2x,val2y2, type = "l", col= "red", axes=FALSE)
     par(new=TRUE)
     plot(val2x,val2y3,type="l",col="blue",axes=FALSE)
     #plot(ordered_frame,type="l")
     #plot(frame2,type="l")

}
