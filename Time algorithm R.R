#=========================Librerias=============================================
library(ggplot2)
#========================MAIN========================================
Tiempos=data.frame()
Grados=data.frame()
Incremento=100
Ns=6000
Renglon=1
while(Incremento<=Ns){
  Proceso=Ejecucion(Incremento)
  Tiempos[Renglon,1]=Incremento
  Tiempos[Renglon,2]=Proceso
  print(paste("Ejecucion No.  ",Incremento))
  Incremento=Incremento+100
  Renglon=Renglon+1
}

#======================GRAFICA INICIAL=========================================

print(ggplot(data=Tiempos, aes(x=V1, y=V2)) +  geom_point() + geom_line() + labs(title="Comportamiento del tiempo"))

#======================FAMILIAS=========================================

fam=Coe_corr() #Determina el coeficiente de correlacion y muestra a cual de las familias se asemeja

for(i in 1:nrow(Tiempos)){Grados[i,1]=conv_grados(atan(Tiempos[i,2]/Tiempos[i,1]))}
punto=Obtener_mayor(nrow(Tiempos))
punto2=Obtener_menor(nrow(Tiempos))

x = c(0,(punto*100))
y = c(0,Tiempos[punto,2])
y2 = c(0,Tiempos[punto2,2])

print("Seleccione la complejidad que desea que se grafique")
Menu()

#==================================FUNCIONES=============================================

#===============================BIG O======================================
BigO<-function(flag,opc){
  if(flag==1){graf_par(opc,1)}
  if(flag==2){plot(Tiempos[,1],Tiempos[,2],type="o",col="black",xlab="Ejecuciones",ylab="Tiempo",main="Comportamiento del tiempo",panel.first=grid())
    abline(lm(y ~ x,data = data.frame(x = x, y = y)),col="red",lwd=2)} #Graficar una parabola}}
  if(flag==3){graf_log(opc,1)}
  if(flag==4){
    plot(Tiempos[,1],Tiempos[,2],type="o",col="black",xlab="Ejecuciones",ylab="Tiempo",main="Comportamiento del tiempo",panel.first=grid())
    abline(lm(y ~ x,data = data.frame(x = c(0,max(Tiempos$V1)), y = Tiempos[punto,2])),col="red",lwd=2)
  }
}
#===============================LITTLE o======================================
Littleo<-function(flag,opc){
  if(flag==1){graf_par(opc,1.1)}
  if(flag==2){plot(Tiempos[,1],Tiempos[,2],type="o",col="black",xlab="Ejecuciones",ylab="Tiempo",main="Comportamiento del tiempo",panel.first=grid())
    abline(lm(y ~ x,data = data.frame(x = x*0.9, y = y)),col="red",lwd=2)} #Graficar una parabola}}
  if(flag==3){graf_log(opc,1.1)}
  if(flag==4){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = data.frame(x=Tiempos$V1,y=Tiempos[punto,2]*1.09), aes(x =x, y = y), color = "red") +
            labs(title="Comportamiento del tiempo"))
    
  }
}
#===============================BIG θ======================================
Big_theta<-function(flag,opc){
  if(flag==1){graf_par(opc,2)}
  if(flag==2){    
    plot(Tiempos[,1],Tiempos[,2],type="o",col="black",xlab="Ejecuciones",ylab="Tiempo",main="Comportamiento del tiempo",panel.first=grid())
    abline(lm(y ~ x,data = data.frame(x = x, y = y)),col="red",lwd=2)
    abline(lm(y ~ x,data = data.frame(x = x, y = y2)),col="red",lwd=2)} #Graficar una parabola}}
  if(flag==3){graf_log(opc,2)}
  if(flag==4){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = data.frame(x=Tiempos$V1,y=Tiempos[punto,2]), aes(x =x, y = y), color = "red") +
            geom_line(data = data.frame(x=Tiempos$V1,y=Tiempos[punto2,2]), aes(x =x, y = y), color = "red") +
            labs(title="Comportamiento del tiempo"))
    
  }
}
#===============================BIG Ω======================================
Big_omega<-function(flag,opc){
  if(flag==1){graf_par(opc,-1)}
  if(flag==2){   
    plot(Tiempos[,1],Tiempos[,2],type="o",col="black",xlab="Ejecuciones",ylab="Tiempo",main="Comportamiento del tiempo",panel.first=grid())
    abline(lm(y ~ x,data = data.frame(x = x, y = y2)),col="red",lwd=2)} #Graficar una parabola}}
  if(flag==3){graf_log(opc,-1)}
  if(flag==4){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = data.frame(x=Tiempos$V1,y=Tiempos[punto2,2]), aes(x =x, y = y), color = "red") +
            labs(title="Comportamiento del tiempo"))
  }
}
#===============================LITTLE ω======================================
Little_omega<-function(flag,opc){
  if(flag==1){graf_par(opc,-1.1)}
  if(flag==2){    
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) +  geom_point() + geom_line() +  
            geom_segment(aes(x = 0, y = 0,xend=max(V1), yend = Tiempos[punto2,2]-0.01), color="red", lwd=1) + 
            labs(title="Comportamiento del tiempo"))} #Graficar una parabola}}
  if(flag==3){graf_log(opc,-1.1)}
  if(flag==4){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = data.frame(x=Tiempos$V1,y=Tiempos[punto2,2]-0.01), aes(x =x, y = y), color = "red") +
            labs(title="Comportamiento del tiempo"))
  }
}

famConst = function(){
  x1=rep(3,nrow(Tiempos)/2)
  x2=rep(3.01,nrow(Tiempos)/2)
  const=c(x1,x2)
  return(const)
}

Ejecucion=function(Incremento){
  TI<-proc.time()
  for (i in 1:Incremento) {
    Merge(c(1,3,5, 2,4,6), 1, 3, 6)
  }
  TF=proc.time()-TI
  return(TF[2])
}

Coe_corr = function(){
  
  FamiliaConst = famConst()
  FamiliaLog = (log(Tiempos$V1))
  FamiliaParabola = c(Tiempos$V1*Tiempos$V1)
  FamiliaLineal = Tiempos$V1
  Tiempos=cbind(Tiempos,FamiliaParabola)
  Tiempos=cbind(Tiempos,FamiliaLineal)
  Tiempos=cbind(Tiempos,FamiliaLog)
  Tiempos=cbind(Tiempos,FamiliaConst)
  
  CoefParabola=abs(cor(Tiempos[,2],Tiempos[,3]))
  CoefLineal=abs(cor(Tiempos[,2],Tiempos[,4]))
  CoefLog=abs(cor(Tiempos[,2],Tiempos[,5]))
  CoefConst=abs(cor(Tiempos[,2],Tiempos[,6]))
  
  
  print("Los coeficientes de correlacion son: ")
  print(paste("Parabola: ",CoefParabola))
  print(paste("Lineal: ",CoefLineal))
  print(paste("Logaritmica: ",CoefLog))
  print(paste("Constante: ",CoefConst))
  
  fam=1
  aux=CoefParabola
  
  if(CoefLineal > aux){
    fam=2
    aux=CoefParabola
  }
  if(CoefLog > aux){
    fam=3
    aux=CoefLog
  }
  if(CoefConst > aux){
    fam=4
  }
  if(fam==1){
    print("Se parece mas a la familia de las parabola")
  }
  if(fam==2){
    print("Se parece mas a la familia de las lineales")
  }
  if(fam==3){
    print("Se parece mas a la familia de los logaritmos")
  }
  if(fam==4){
    print("Se parece mas a la familia de las constantes")
  }
  
  return(fam)
}

conv_grados=function(x){
  x*180/pi
}

Obtener_mayor=function(cant_var){
  aux=max(Grados)
  punto=0
  for(i in 1:cant_var){if(aux==Grados[i,1]){punto=i}}
  return(punto)
}

Obtener_menor=function(cant_var){
  punto=0
  aux=min(Grados)
  for(i in 1:cant_var){if(aux==Grados[i,1]){punto=i}
  }
  return(punto)
}

Menu=function(){
  
  Opciones <- c("Big O","Little o","Big θ","Big Ω","Little ω","Salir")
  
  while(TRUE){
    
    opc <- displayMenu(Opciones)
    
    
    if(opc == 1){
      BigO(fam,1)
      
    }
    else if(opc == 2){
      Littleo(fam,1)
    }
    else if(opc == 3){
      Big_theta(fam,2)
      
    }
    else if(opc == 4){
      Big_omega(fam,1)
    }
    else if(opc == 5){
      Little_omega(fam,1)
    }
    
    else if(opc== 6){
      break
    }
  }
  
}

displayMenu <- function(options){
  
  for(i in 1:length(options)){
    cat(sprintf("%d. %s\n",i,options[i]))
  }
  
  choice <- 0
  while(!any(choice == 1: length(options))){
    choice=intputNumber("Porfavor escoja una opcion: ")
  }
  return(choice)
}

intputNumber <- function(prompt){
  while(TRUE){
    num = suppressWarnings(as.numeric(readline(prompt)))
    if(!is.na(num)){
      break 
    }
  }
  return(num)
}

graf_log<-function(opc,lines){
  expo=data.frame()
  bas=Tiempos[punto,1]^(1/Tiempos[punto,2])
  bas2=Tiempos[punto2,1]^(1/Tiempos[punto2,2])
  for(i in 1:nrow(Tiempos)){
    expo[i,1]=log(Tiempos[i,1] , base = bas)
    expo[i,2]=log(Tiempos[i,1] , base = bas2)
  }
  
  expo=cbind(expo,Tiempos$V1)
  
  dif=1
  dif2=-1
  if(lines==1.1){
    dif=0.9
    dif2=-0.9
  }
  if(lines==-1){
    opc=3
  }
  
  if(lines==-1.1){
    opc=4
  }
  
  if(opc==1){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = expo, aes(x = Tiempos$V1*dif , y = V1), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  if(opc==2){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = expo, aes(x = Tiempos$V1*dif , y = V1), color = "red",size=2) +
            geom_line(data = expo, aes(x = Tiempos$V1, y = V2*dif2), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  if(opc==3){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = expo, aes(x = Tiempos$V1 , y = V2), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  if(opc==4){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = expo, aes(x = Tiempos$V1 , y = V2-0.01), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  
}

graf_par<-function(opc,lines){
  parabola=data.frame()
  
  n=log10(Tiempos[punto,2])/log10(Tiempos[punto,1])
  n2=log10(Tiempos[punto2,2])/log10(Tiempos[punto2,1])
  
  for(i in 1:nrow(Tiempos)){
    parabola[i,1]=Tiempos[i,1]^n
    parabola[i,2]=Tiempos[i,1]^n
  }
  
  parabola=cbind(parabola,Tiempos$V1)
  
  dif=1
  dif2=-1
  if(lines==1.1){
    dif=0.9
    dif2=-0.9
  }
  if(lines==-1){
    opc=3
  }
  
  if(lines==-1.1){
    opc=4
  }
  
  if(opc==1){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = parabola, aes(x = Tiempos$V1*dif , y = V1), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  if(opc==2){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = parabola, aes(x = Tiempos$V1*dif , y = V1), color = "red",size=2) +
            geom_line(data = parabola, aes(x = Tiempos$V1, y = V2*dif2), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  if(opc==3){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = parabola, aes(x = Tiempos$V1 , y = (V2*-1)+Tiempos[punto2,2]), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  if(opc==4){
    print(ggplot(data=Tiempos, aes(x=V1, y=V2)) + geom_point() + geom_line() + 
            geom_line(data = parabola, aes(x = Tiempos$V1 , y = V2*-1), color = "red",size=2) +
            labs(title="Comportamiento del tiempo"))
  }
  
}

Merge <- function(a, p, q, r){
  n1 <- q - p + 1
  n2 <- r - q
  L <- numeric(n1+1)
  R <- numeric(n2+1)
  for(i in 1:n1){
    L[i] <- a[p+i-1]
  }
  for(j in 1:n2){
    R[j] <- a[q+j]
  }
  L[n1+1] <- Inf
  R[n2+1] <- Inf
  i=1
  j=1
  for(k in p:r){
    if(L[i] <= R[j]){
      a[k] <- L[i]
      i <- i +1
    }else{
      a[k] <- R[j]
      j <- j+1
    }
  }
  a
}

