# complexity and information
#first part


Comin=function(b,dato.simulado){
  #t<-34
  #dv<-34
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
    
    #for(f in 1:colum){
    #  info[f]<-0
    #} 
    info<-rep(x=0,colum)
    
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
      
#       if(E[i]<=1 & E[i]>=0.8){
#         CE[i]<-"darkblue"
#       }
#       if(E[i]<0.8& E[i]>=0.6){
#         CE[i]<-"green"
#       }
#       if(E[i]<0.6 & E[i]>=0.4){
#         CE[i]<-"yellow"
#       }
#       if(E[i]<0.4 & E[i]>=0.2){
#         CE[i]<-"orange"
#       }
#       if(E[i]<0.2 & E[i]>=0){
#         CE[i]<-"red"
#       }
#       if(S[i]<=1 & S[i]>=0.8){
#         CS[i]<-"darkblue"
#       }
#       if(S[i]<0.8& S[i]>=0.6){
#         CS[i]<-"green"
#       }
#       if(S[i]<0.6 & S[i]>=0.4){
#         CS[i]<-"yellow"
#       }
#       if(S[i]<0.4 & S[i]>=0.2){
#         CS[i]<-"orange"
#       }
#       if(S[i]<0.2 & S[i]>=0){
#         CS[i]<-"red"
#       }
#       if(C[i]<=1 & C[i]>=0.8){
#         CC[i]<-"darkblue"
#       }
#       if(C[i]<0.8& C[i]>=0.6){
#         CC[i]<-"green"
#       }
#       if(C[i]<0.6 & C[i]>=0.4){
#         CC[i]<-"yellow"
#       }
#       if(C[i]<0.4 & C[i]>=0.2){
#         CC[i]<-"orange"
#       }
#       if(C[i]<0.2 & C[i]>=0){
#         CC[i]<-"red"
#       #}
      
    } ### cierra for de i
    
    
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
      
#       if(AUTOP[i]==1){
#         CA[i]<-"black"
#       }
#       if(AUTOP[i]>1){
#         CA[i]<-"darkblue"
#       }
#       if(AUTOP[i]<1){
#         CA[i]<-"darkred"
#       }
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


#Genera matriz de una comunidad con n-especies, n-sitios; p uniforme (azar) entre 0.1 y 0.9  (p ~ runif(n=1,min=0.1,max=0.9))
# psi como distrib uniforme (azar) entre 0.1 y 0.9  (psi ~ runif(n=1,min=0.1,max=0.9))
# cada especie con p y psi diferente
f.data.gen.comunid<-function(especies,sitios) {
  res<-matrix(NA,nr=especies,nc=sitios)
  psi.p<-matrix(NA,nr=especies,nc=2)
  for (d in 1:especies){
    psi <- runif(n=1,min=0.1,max=0.9)
    p <- runif(n=1,min=0.1,max=0.9)
    #generate the expected occupancies
    n<-rbinom(especies,1,psi)
    #generate the observations
    for(i in 1:especies){
      res[d,]<-rbinom(sitios,1,n[i]*p)
    }
    #print(res)
    #print(c(psi,p))
    psi.p[d,1]<-psi #guarda psi
    psi.p[d,2]<-p #guarda p
  }
  res<-cbind(res,psi.p) # pega especies y psi.p
  return(res)
}





##############################################
######  Genera matriz en test ###############
######  Cambiar parametros    ###############
######  Hace coming de test   ###############
#############################################
require (ggplot2)
require (plyr)


# genera datos tipo TEAM 60 sitios
matrices<-list() #crea lista vacia para guardad matrices
especies=500 ####  <<<---Numero de especies  AQUI
sitios=60 ####  <<<---Numero de sitios AQUI
for (i in 1:1000){ ####  <<<---Numero de simulaciones AQUI
test <- f.data.gen.comunid(especies=especies,sitios=sitios)
sim.i <- as.data.frame(Comin(b=2,dato.simulado=test[,1:sitios]))
sim.i$sim_number <- as.vector(rep(i,sitios))
sim.i$sitios <-as.vector(seq(1, sitios, by = 1))
matrices[[i]] <- sim.i
}

# convierte lista a dataframe
dat <- ldply(matrices, data.frame)

# 1000 lineas. una por simulacion
g2<-ggplot(data=dat,aes(y=Self.organization, x=sitios, group = sim_number)) 
g2 + geom_point(alpha = I(0.1),size = I(2)) + 
  geom_smooth(aes(colour=factor(sim_number),alpha = 0.1),method=lm,fullrange=T, se=FALSE) +
  scale_colour_discrete(guide = guide_legend(override.aes = list(alpha = 1)))+
  theme(legend.position="none") 
# # una sola linea promedio
# g2 + geom_smooth(aes(colour="red",size=1),method=lm,fullrange=T, se=T) +
#   geom_point(alpha = I(0.01),size = I(2)) + geom_jitter() +
#   scale_colour_discrete(guide = guide_legend(override.aes = list(alpha = 1)))+
#   theme(legend.position="none") 
# 

# g2 + geom_hex(bins=10) # test con hex bins
  

source("code\\vwReg.R")
# build a demo data set
# set.seed(1)
# x <- rnorm(200, 0.8, 1.2) 
# e <- rnorm(200, 0, 3)*(abs(x)^1.5 + .5) + rnorm(200, 0, 4)
# e2 <- rnorm(200, 0, 5)*(abs(x)^1.5 + .8) + rnorm(200, 0, 5)
# y <- 8*x - x^3 + e
# y2 <- 20 + 3*x + 0.6*x^3 + e2
# df <- data.frame(x, y, y2)
# 
# p1 <- vwReg(y~x, df, spag=TRUE, shade=FALSE)
# p2 <- vwReg(y2~x, df, add=TRUE, spag=TRUE, shade=FALSE, spag.color="red", shape=3)
# p3 <- p1 + p2
# p3

p1 <- vwReg(Self.organization~sitios, dat, spag=TRUE, shade=FALSE)
p2 <- vwReg(Complexity~sitios, dat, add=TRUE, spag=TRUE, shade=FALSE, spag.color="red", shape=3)
p3 <- vwReg(Emergence~sitios, dat, add=TRUE, spag=TRUE, shade=FALSE, spag.color="green", shape=3)

p4 <- p1 + p2 + p3
p4


