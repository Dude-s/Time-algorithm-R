#=================================MAIN
Tiempos=data.frame()
Incremento=100
Ns=4000
Renglon=1
while(Incremento<=Ns){
  Tarea=Actividad(Incremento,1)
  #Tarea=Actividad(Incremento,2)
  Tiempos[Renglon,1]=Incremento
  Tiempos[Renglon,2]=Tarea
  print(Incremento)
  Incremento=Incremento+100
  Renglon=Renglon+1
}

plot(Tiempos[,2],type="l",xlab="Ns",ylab="Tiempo",main="Comportamiento del tiempo",panel.first=grid())
points(Tiempos[,2])
agregarFamilias()
#=================================FUNCIONES
Actividad=function(Incremento,actividad){
  #ciclo 
  if(actividad==1){
    TInicio <- proc.time()
    for (i in 1:Incremento) {
      for (j in 1:200) {
        for (h in 1:200) {
          i*4
        }
      }
    }
    TFInal=proc.time()-TInicio
    return(TFInal[2])
  }
  
  if(actividad==2){
    TInicio <- proc.time()
    for (i in 1:Incremento) {
      for (j in 1:200) {
        for (h in 1:200) {
          for (k in 1:200){
            i*4
          }
        }
      }
    }
    TFInal=proc.time()-TInicio
    return(TFInal[2])
  }
}

agregarFamilias=function(){
  familiaParabolas=calcularCuadrados()
  familiaLineal=calcularLineal()
  familiaLogaritmos=calcularLogaritmo()
  familiaConstante=calcularConstante()
  
  Tiempos=cbind(Tiempos,familiaParabolas)
  Tiempos=cbind(Tiempos,familiaLineal)
  Tiempos=cbind(Tiempos,familiaLogaritmos)
  Tiempos=cbind(Tiempos,familiaConstante)
  row.names(Tiempos)=Tiempos[,1]
  Tiempos=Tiempos[,-1]
  
  MaxTodos=max(Tiempos)
  
  plot(c(1:nrow(Tiempos)),Tiempos$V2,col="red", ylim = c(0,MaxTodos),xlab="Ns",
       ylab="Tiempo", type = "o",
       main="Comportamiento del tiempo")
  lines(c(1:nrow(Tiempos)),familiaLineal,col="darkblue",lwd=3)
  lines(c(1:nrow(Tiempos)),familiaParabolas,col="yellow",lwd=3)
  lines(c(1:nrow(Tiempos)),familiaLogaritmos,col="darkgreen",lwd=3)
  lines(c(1:nrow(Tiempos)),familiaConstante,col="darkorange",lwd=3)
  
 plot(c(1:nrow(Tiempos)),Tiempos$V2,col="red", ylim = c(0,10),xlab="Ns",
      ylab="Tiempo",type="l",
     main="Comportamiento del tiempo")
 lines(c(1:nrow(Tiempos)),familiaLogaritmos,col="darkgreen",lwd=3)
 lines(c(1:nrow(Tiempos)),familiaConstante,col="darkorange",lwd=3)
  
  
  
  CoeficienteParabola=abs(cor(Tiempos[,1],Tiempos[,2]))
  CoeficienteLineal=abs(cor(Tiempos[,1],Tiempos[,3]))
  CoeficienteLogaritmo=abs(cor(Tiempos[,1],Tiempos[,4]))
  CoeficienteConstante=abs(cor(Tiempos[,1],Tiempos[,5]))
  print(paste("Coeficiente r con parabola: ",CoeficienteParabola))
  print(paste("Coeficiente r con lineal: ",CoeficienteLineal))
  print(paste("Coeficiente r con logaritmica: ",CoeficienteLogaritmo))
  print(paste("Coeficiente r con constante: ",CoeficienteConstante))
}

calcularConstante=function(){
  Vector1=rep(10,nrow(Tiempos)/2)
  Vector2=rep(10.01,nrow(Tiempos)/2)
  Vector=c(Vector1,Vector2)
  return(Vector)
}

calcularLogaritmo=function(){
  TOriginal=Tiempos$V1
  return(log(TOriginal))
}

calcularLineal=function(){
  TOriginal=Tiempos$V1
  return(TOriginal)
}

calcularCuadrados=function(){
  TOriginal=Tiempos$V1
  return(c(TOriginal*TOriginal))
}

