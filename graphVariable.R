

# Funcion para graficar el Valor de cualquier variable contra el tiempo.
# Para hacerla funcionar, solamente escriba "graphVariable()"; no necesita ningun argumento.
# Se necesita tener la función "fifo.R" en el working directory y además tener cargada
# la librería ggplot2. install.packages("ggplot2") ; library("ggplot2")

graphVariable <- function(){
     
     source("fifo.R")
     
     #Lectura del archivo csv y obtención de las variables que se encuentran en la columna Tagname
     data <- read.csv("2015-03-03-10-45-11-151.csv")
     tagnames <- levels(data$Tagname)
     print(tagnames)
     y <- as.numeric(readline("Escoja el numero de la variable que quiera graficar: "))
     variable <- tagnames[y]
     colTagname <- as.character(data$Tagname)
     LogTagname <- colTagname == variable 
     Values <- data$Value
     Fecha <- data$Date
     
     print(paste("La cantidad de valores para ", variable, " que se encuentran en este archivo son: ", sum(LogTagname),sep=""))
     horas <- as.character(data$Time); horas <- gsub(" a. m.","", horas)
     
     #Subseteo de las columnas Date, Time y Value considerando la variable seleccionada.
     errorFecha <- Fecha[LogTagname]
     errorValues <- Values[LogTagname]
     errorTime <- horas[LogTagname]
     
     #Inspección de valores repetidos en la columna Value de la variable.
     valoresrepetidos <- sum(duplicated(errorValues))
     print(paste("La cantidad de valores repetidos es: ", valoresrepetidos, sep=""))
     duplicados <- as.numeric(readline("Escoja 1 si desea remover los duplicados, y 2 si desea dejarlos: "))
     
     if (duplicados==1){
          
          valores <- !duplicated(errorValues)
          Valor <- errorValues[valores]
          Hora <- errorTime[valores]
          Fec <- errorFecha[valores]
     
     }
     else if (duplicados == 2){
     
          valores <- LogTagname
          Valor <- Values[LogTagname]
          Hora <- horas[LogTagname]
          Fec <- Fecha[LogTagname]
          
     }
     else{
          
          stop("No se ha introducido el valor correcto.")     

     }
     
     #Limpieza de las columnas Date y Time (se acomodan para procesar su información).
     fechayhora <- paste(Fec,Hora)
     fechayhora <- gsub("/","-",fechayhora)
     fechayhora <- gsub("mar","03",fechayhora)
     fechayhora <- gsub("2015","15",fechayhora)
     #Transformando la fecha y la hora a clase POSXLT
     Time <- strptime(fechayhora, "%d-%m-%Y %H:%M:%S")
     
     buf <- as.numeric(readline("Numero de buffer: "))
     Variable <- fifo(Valor,buf)
     Tiempo <- Time[buf:length(Time)]
     
     table <- data.frame(Tiempo,Variable)
     print("Cantidad de datos que se van a graficar debido a la eliminación de valores repetidos y a la aplicación del FIFO")
     print(paste("Variable: ",length(Variable)," de ",length(errorValues)))
     #print(length(Tiempo))
     print("Resumen de la variable seleccionada: ")
     print(summary(Valor))
     
     #Función para graficar Variable vs Tiempo
     ggplot(aes(x = Tiempo, y = Variable), data = table ) + geom_line()
     
     #hist(Variable)
     #ggplot(aes(x = Tiempo, y = medias), data = table ) + geom_point()
     #ggplot(aes(x = Time, y = Valor), data = dframe ) + geom_boxplot(color = 'red')
}
