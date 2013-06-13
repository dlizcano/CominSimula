# complexity and information
#first part

# ext 1 es extension csv -eliminado
# f es file path -eliminado
# r es directorio - elimiado
# ld es ? borrar! - borrado
# it es seleccion vector o matriz - borrado
### que es la b???? base??? = 10 ?????

Comin=function(b,dato.simulado){
  t<-34
  dv<-34
 # rm(list=ls())
  pmatriz<-function(matriz,base){
    fias<-nrow(matriz)
    colum<-ncol(matriz)
    AUTOP<-0
    max1<-0
    min1<-0
    rango<-0
    basetrans<-matriz
    info<-0
    logar<-0
    cont<-0
    prob<-0
    E<-0
    S<-0
    C<-0
    CE<-0
    CS<-0
    CC<-0
    CA<-0
    color<-0
    
    for(f in 1:colum){
      info[f]<-0
    }
    base<-base-0.0001
    for (i in 1:colum){
      r<-matriz[,i]
      max1[i]<-max(r)
      min1[i]<-min(r)
      rango[i]<-max1[i]-min1[i]
      for(j in 1:fias){
        if(rango[i]==0){
          basetrans[j,i]<-0
        }
        else{
          basetrans[j,i]<-floor(base*((matriz[j,i]-min1[i])/rango[i]))
        }
        
      }
      
      lim=floor(base)
      for(k in 1:lim+1){
        cont[k]=0
      }
      for(h in 0:lim){
        for(j in 1:fias){
          if(basetrans[j,i]==h){
            cont[h+1]<-cont[h+1]+1
          }
        }
      }
      prob<-cont/fias
      cont<-0
      for(u in 0:lim){
        if(prob[u+1]==0){
          logar[i]<-0
        }
        else
        {
          logar[i]<-log(prob[u+1],lim+1)
        }
        info[i]<-info[i]+(prob[u+1]*logar[i])
      }
      info[i]=info[i]*(-1)
      E[i]=info[i]
      S[i]=1-E[i]
      C[i]=4*E[i]*S[i]
      if(E[i]<=1 & E[i]>=0.8){
        CE[i]<-"darkblue"
      }
      if(E[i]<0.8& E[i]>=0.6){
        CE[i]<-"green"
      }
      if(E[i]<0.6 & E[i]>=0.4){
        CE[i]<-"yellow"
      }
      if(E[i]<0.4 & E[i]>=0.2){
        CE[i]<-"orange"
      }
      if(E[i]<0.2 & E[i]>=0){
        CE[i]<-"red"
      }
      if(S[i]<=1 & S[i]>=0.8){
        CS[i]<-"darkblue"
      }
      if(S[i]<0.8& S[i]>=0.6){
        CS[i]<-"green"
      }
      if(S[i]<0.6 & S[i]>=0.4){
        CS[i]<-"yellow"
      }
      if(S[i]<0.4 & S[i]>=0.2){
        CS[i]<-"orange"
      }
      if(S[i]<0.2 & S[i]>=0){
        CS[i]<-"red"
      }
      if(C[i]<=1 & C[i]>=0.8){
        CC[i]<-"darkblue"
      }
      if(C[i]<0.8& C[i]>=0.6){
        CC[i]<-"green"
      }
      if(C[i]<0.6 & C[i]>=0.4){
        CC[i]<-"yellow"
      }
      if(C[i]<0.4 & C[i]>=0.2){
        CC[i]<-"orange"
      }
      if(C[i]<0.2 & C[i]>=0){
        CC[i]<-"red"
      }
      
    }
    aux<-1
    conta<-1
    for(fg in 1:colum){
      if(aux==1){
        color[conta]<-CE[fg]
        aux<-2
        conta<-conta+1
      }
      if(aux==2){
        color[conta]<-CS[fg]
        aux<-3
        conta<-conta+1
      }
      if(aux==3){
        color[conta]<-CC[fg]
        aux<-1
        conta<-conta+1
      }
      
    }
    factor<-colum*(1/105)
    factor<-1-factor
    haming<-c(1:fias)
    for(g in 1:fias){
      haming[g]<-0
    }
    #hasta aca
    for (i in 1:colum){
      aux<-1
      c<-1
      for(j in 1: fias){
        if((basetrans[j,i])!=(basetrans[aux,i])){
          haming[c]<-haming[c]+1
        }
        aux<-j
        c<-c+1
      }
    }
    haming<-haming/(colum)
    homeostasis<-1-haming
    sumcom<-0
    sumcom<-sum(C)
    res<-colum-1
    
    for(i in 1:colum){
      gh<-(sumcom-C[i])/res
      AUTOP[i]<-(C[i]/gh)
      if(AUTOP[i]==1){
        CA[i]<-"black"
      }
      if(AUTOP[i]>1){
        CA[i]<-"darkblue"
      }
      if(AUTOP[i]<1){
        CA[i]<-"darkred"
      }
    }
    resul<-matrix(c(E,S,C,AUTOP),ncol=4,nrow=colum)
    lim<-max(AUTOP)
    rownames(resul)<-names(matriz)
    colnames(resul)<-c("Emergence", "Self-organization", "Complexity","Autopoiesis")
    K<-rownames(resul)
    names(E)<-rownames(resul)
    names(S)<-rownames(resul)
    names (C)<-rownames(resul)
    names(AUTOP)<-rownames(resul)
    
    
#     if(tipo==2){
#       K<-""
#     }
    
    
    #attach(mtcars) 
    ########################
    ####### Plot part ######
    ########################
    
#     layout(matrix(c(1,2,3,4,5,5), 3,2, byrow = TRUE)) 
#     barplot(E,beside=TRUE, ylim=c(0,1),main="Emergence",col=c(CE),cex.names=factor)
#     barplot(S,beside=TRUE, ylim=c(0,1),main="Self-organization",col=c(CS),cex.names=factor)
#     barplot(C,beside=TRUE, ylim=c(0,1),main="Complexity",col=c(CC),cex.names=factor)
#     plot(homeostasis,type="l",main="Homeostasis",xlab="",ylab="",ylim=c(0,1))
#     barplot(AUTOP,beside=TRUE, ,main="Autopoiesis",col=c(CA),ylim=c(0,lim),cex.names=factor)
#     
    
    M <-resul
    return(M)
    # return(homeostasis) ### Pensar como recuperarla !!!!!!
  }
  
 
  ################
  ### New part ###
  ################
  
  data<-dato.simulado # lee datos de matriz generados en f.data.gen.XX
  
  #attach(data)
  M<-pmatriz(data,b)
  M
  return(M)
  
  
} # cierre final 
  
  #############################################
  
#   
#   pvectores<-function(data,b){
#     max1<-max(data)
#     min1<-min(data)
#     rango<-(max1-min1)
#     tama<-length(data)
#     i=0
#     basetrans<-3
#     cont<-0
#     info<-0
#     b<-b-0.001
#     for(i in 1:tama){ # recorriendo el vector por el tama?o
#       if(rango==0){
#         basetrans[i]<-0
#       }
#       else{
#         basetrans[i]<-floor(b*((data[i]-min1)/rango))#transformando el vector a la base deseada
#       }
#     }
#     lim=floor(base)
#     for(k in 1:lim+1){ #creando el vector que cuenta cuantas veces esta el dato
#       cont[k]=0
#     }
#     for(i in 0:lim){
#       for(j in 1:tama){
#         if(basetrans[j]==i){
#           cont[i+1]<-cont[i+1]+1  #contando cuantas veces esta el dato
#         }
#       }
#     }
#     prob<-cont/tama #probabiidad del vector
#     for(u in 0:lim){
#       if(prob[u+1]==0){
#         logar<-0
#       }
#       else
#       {
#         logar<-log(prob[u+1],lim+1) 
#       }
#       info<-info+(prob[u+1]*logar) #informacion del vectores negatico
#     }
#     info=info*-1 #informacion del vector
#     E=info #emergencia ,auto-organizacion y complejidad
#     S=1-E
#     C=4*E*S
#     if(E<=1 & E>=0.8){#colores de las emergencias ,auto-organizacion y complejidad
#       CE<-"darkblue"
#     }
#     if(E<0.8& E>=0.6){
#       CE<-"green"
#     }
#     if(E<0.6 & E>=0.4){
#       CE<-"yellow"
#     }
#     if(E<0.4 & E>=0.2){
#       CE<-"orange"
#     }
#     if(E<0.2 & E>=0){
#       CE<-"red"
#     }
#     if(S<=1 & S>=0.8){
#       CS<-"darkblue"
#     }
#     if(S<0.8& S>=0.6){
#       CS<-"green"
#     }
#     if(S<0.6 & S>=0.4){
#       CS<-"yellow"
#     }
#     if(S<0.4 & S>=0.2){
#       CS<-"orange"
#     }
#     if(S<0.2 & S>=0){
#       CS<-"red"
#     }
#     if(C<=1 & C>=0.8){
#       CC<-"darkblue"
#     }
#     if(C<0.8& C>=0.6){
#       CC<-"green"
#     }
#     if(C<0.6 & C>=0.4){
#       CC<-"yellow"
#     }
#     if(C<0.4 & C>=0.2){
#       CC<-"orange"
#     }
#     if(C<0.2 & C>=0){
#       CC<-"red"
#     }
#     resul<-c(E,S,C) #resultados de los datos
#     
#     names(resul)<-c("Emergence","Self-organization","Complexity") #nombres de los datos
#     par(mfrow=c(1,1)) 
#     barplot(resul,ylim=c(0,1), main="Emergence,Self-organization and Complexity", col=c(CE,CS,CC)) #graica de los datos
#     V <- matrix(c(E,S,C),nrow =1, ncol = 3, byrow=TRUE,  dimnames=list( c("value"),c("Emergence", "Selforganization", "Complexity")))
#     V
#     return(V)
#   } # cierra vectores
#   
#   ##########################################
#   
#   
  
  
  
  
  
#   repeat{
#     if(it>=1 && it<=4)break
#   }
  
  
  
  
#   #third part
#   if(it==1){ #datos para vectores en R
#     repeat{
#       max1<-max(data)
#       min1<-min(data)
#       rango<-(max1-min1)
#       tama<-length(data)
#       i=0
#       basetrans<-3
#       cont<-0
#       info<-0
#       if(tama>=2)break
#     }
#     V<-pvectores(ld,b)
#     return(V)
#   }
#   
  
  
#   
#   if(it==3) { #datos para matrices extraidas
#     #1
#     #2
#     setwd(r)
#     #3
#     if(ext==1){ #datos csv
#       #4
#       data<-read.csv(f) # lee toda la tabla Nelson
#       
#       attach(data)
#       M<-pmatriz(data,b)
#       M
#       return(M)
#     }
#     if(ext==2){ #datos txt
#       #4
#       data<-read.delim(f, header = TRUE, sep = "\t") # lee toda la tabla Nelson
#       
#       attach(data)
#       M<-pmatriz(data,b)
#       M
#       return(M)
#     }
#     if(ext==3){ #datos spps
#       #4
#       library(foreign)
#       data <- read.spss(f, to.data.frame = TRUE)
#       
#       attach(data)
#       M<-pmatriz(data,b)
#       M
#       return(M)
#     }
#   }
#   if(it==2){ #matrices en R
#     M<-pmatriz(ld,b)
#     return(M)
#   }
# } # last   
  
  
#   if(it==4){ #vectores extraidos
#     #2
#     setwd(r)
#     #3
#     if(ext==1){ #datos csv
#       data<-read.csv(f) 
#       
#       V<-pvectores(data,b)
#       V
#       return(V)
#     }
#     if(ext==2){ #datos txt
#       #4
#       data<-read.delim(f, header = TRUE, sep = "\t") 
#       V<-pvectores(data,b)
#       V
#       return(V)
#     }
#     
#     if(ext==3){ #datos spps
#       #4
#       library(foreign)
#       V <- read.spss(f, to.data.frame = TRUE)
#       V<-pvectores(data,b)
#       V
#       return(V)
#     }
#   }

# last }  eliminado y movido arriba  





##########################################################
################ simula datos  ##########################
##########################################################

#Genera matriz de observacion de individuos en una pobl. misma especie con igual detectabilidad (p) y ocupancia (psi)
# anio 1
# 
f.data.gen.pobl<-function(individuos,sitios,psi,p) {
  res<-matrix(NA,nr=individuos,nc=sitios)
  #generate the expected occupancies
  n<-rbinom(individuos,1,psi)
  #generate the observations
  for(i in 1:individuos)
    res[i,]<-rbinom(sitios,1,n[i]*p)
  return(res)  
}


#Genera matriz de una comunidad con n-especies, n-sitios; p uniforme (azar) entre 0.2 y 9.9  (p ~ runif(n=1,min=0.2,max=0.99))
# psi como distrib uniforme (azar) entre 0.2 y 9.9  (psi ~ runif(n=1,min=0.2,max=0.99))
# cada especie con p y psi diferente
f.data.gen.comunid<-function(especies,sitios) {
  res<-matrix(NA,nr=especies,nc=sitios)
  for (d in 1:especies){
    psi <- runif(n=1,min=0.2,max=0.99)
    p <- runif(n=1,min=0.2,max=0.99)
    #generate the expected occupancies
    n<-rbinom(especies,1,psi)
    #generate the observations
    for(i in 1:especies)
      res[d,]<-rbinom(sitios,1,n[i]*p)
    #print(res)
  } 
  return(res)
}





##############################################
######  Genera matriz en test ###############
######  Cambiar parametros    ###############
######  Hace coming de test   ###############
#############################################

# genera datos tipo TEAM
matrices<-list() #crea lista vacia para guardad matrices
for (i in 1:1000){ ####  <<<---Numero de simulaciones AQUI
test <- f.data.gen.comunid(especies=100,sitios=60)
sim.i <- as.data.frame(Comin(b=2,dato.simulado=test))
sim.i$sim_number <- as.vector(rep(i,60))
sim.i$y <-as.vector(seq(1, 60, by = 1))
matrices[[i]] <- sim.i
}
require (ggplot2)
require (plyr)

dat <- ldply(matrices, data.frame)


g2<-ggplot(data=dat,aes(y=Autopoiesis, x=y, group = sim_number)) 
g2 + geom_point(alpha = I(0.1),size = I(2)) + 
  geom_smooth(aes(colour=factor(sim_number),alpha = 0.1),method=lm,fullrange=T, se=FALSE) +
  scale_colour_discrete(guide = guide_legend(override.aes = list(alpha = 1)))+
  theme(legend.position="none") 
  
# 
# g2 + geom_path(alpha = I(0.1)) + 
#   geom_smooth(aes(colour=factor(sim_number),alpha = 0.1),method=lm,fullrange=T, se=FALSE) +
#   scale_colour_discrete(guide = guide_legend(override.aes = list(alpha = 1)))+
#   theme(legend.position="none") 




