# paquetes
suppressMessages(suppressWarnings(library(dplyr))) 
suppressMessages(suppressWarnings(library(ggplot2))) 
library(readr)

SB11_1 <- read_delim("C:/Users/PC1/Downloads/SB11_20222.TXT", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

icfes<-SB11_1

icfes<-icfes%>%
  filter(ESTU_ESTADOINVESTIGACION=="PUBLICAR",ESTU_NACIONALIDAD=="COLOMBIA",ESTU_PAIS_RESIDE=="COLOMBIA",COLE_DEPTO_UBICACION!="SAN ANDRES")%>%
  as.data.frame()


icfes<-subset(icfes,!(is.na(COLE_MCPIO_UBICACION) & is.na(COLE_DEPTO_UBICACION) & is.na(PUNT_GLOBAL)))
icfes<-icfes[order(icfes$COLE_COD_MCPIO_UBICACION),]


# tabla
estadisticos_mun <- icfes %>% 
  group_by(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION,COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION) %>% 
  summarise(njK = n(), 
            yb_mun = mean(PUNT_GLOBAL), 
            s2_mun = var(PUNT_GLOBAL))%>%
  as.data.frame()

estadisticos_dep <- estadisticos_mun %>% 
  group_by(COLE_COD_DEPTO_UBICACION) %>% 
  summarise(codigo_dep = unique(COLE_COD_DEPTO_UBICACION), 
            nombre_dep = unique(COLE_DEPTO_UBICACION), 
            nK = n(), 
            yb_dep = mean(yb_mun),
            s2_dep = var(yb_mun))
  

departamentos<-icfes%>%
  group_by(COLE_COD_DEPTO_UBICACION)%>%
  summarise(codigo_dep = unique(COLE_COD_DEPTO_UBICACION), 
            nombre_dep = unique(COLE_DEPTO_UBICACION), 
            nj = n(), 
            ybj = mean(PUNT_GLOBAL),
            s2j = var(PUNT_GLOBAL))
            

nj<-departamentos$nj
ybj<-departamentos$ybj
s2j<-departamentos$s2j

njk<-estadisticos_mun$njK
yb_mun<-estadisticos_mun$yb_mun
s2_mun<-estadisticos_mun$s2_mun
nk<-estadisticos_dep$nK
yb_dep<-estadisticos_dep$yb_dep
s2_dep<-estadisticos_dep$s2_dep


n<-length(icfes$ESTU_ESTUDIANTE)
n_mun<-length(yb_mun)
m<-length(yb_dep)
yijkb<-mean(icfes$PUNT_GLOBAL)
s2<-var(icfes$PUNT_GLOBAL)


y <- icfes$PUNT_GLOBAL
sum_y<-sum(y)
Y <- vector(mode = "list", length = n_mun)


# PUNTO 1

#mapa pobreza
library(sf)
mapa<-st_read("C:/Users/PC1/Downloads/MGN_DPTO_POLITICO.shp")


mapa <- within(mapa,{
  departamento <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(DPTO_CNMBR))
})

mapa$departamento[mapa$departamento=="NORTE DE SANTANDER"]<-"NORTE SANTANDER"
mapa$departamento[mapa$departamento=="BOGOTA, D.C."]<-"BOGOTA"
mapa$departamento[mapa$departamento=="VALLE DEL CAUCA"]<-"VALLE"



library(readxl)
pobreza_monetaria <- read_excel("C:/Users/PC1/Downloads/pobreza monetaria.xls",sheet=2)

pobreza<-pobreza_monetaria[11:34,c(1,16)]

pobreza <- within(pobreza,{
  departamento <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(`Incidencia de la Pobreza Monetaria`))
})

pobreza$departamento[pobreza$departamento=="BOGOTA D.C."] <- "BOGOTA"
pobreza$departamento[pobreza$departamento=="NORTE DE SANTANDER"] <- "NORTE SANTANDER"
pobreza$departamento[pobreza$departamento=="VALLE DEL CAUCA"] <- "VALLE"


colnames(pobreza)<-c("dep","incidencia","departamento")

pobreza<-pobreza[,-1]

deptos <-icfes %>%
  group_by(COLE_DEPTO_UBICACION) %>%
  summarise(pglobal=mean(PUNT_GLOBAL)) %>%
  as.data.frame()


deptos <- within(deptos,{
  departamento <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(COLE_DEPTO_UBICACION))
})

mapapobreza<-mapa%>%
  left_join(y=pobreza,by="departamento")


mapdeptos<-mapa%>%
  left_join(y=deptos, by="departamento")

g1<-ggplot() +
  geom_sf(data=mapdeptos$geometry,aes(fill=mapdeptos$pglobal),col="darkgray",linetype="solid")+
  scale_fill_gradient(low="green",high="yellow")+
  labs(title = "Puntaje global por departamento",  
       fill = "Puntaje global")



g2<-ggplot() +
  geom_sf(data=mapapobreza$geometry,aes(fill=mapapobreza$incidencia),col="darkgray",linetype="solid")+
  scale_fill_gradient(low="green",high="yellow")+
  labs(title = "Incidencia de la pobreza por departamento",  
       fill = "Incidencia de pobreza")



library(gridExtra)
# Establecer el diseño de la figura
par(mfrow = c(1, 2))

# Mostrar ambos gráficos en una sola figura
grid.arrange(g1, g2, ncol = 2)




#PUNTO 2

#mapa educación

library(readr)
estadísticas_educación <- read_csv("C:/Users/PC1/Downloads/estadísticas educación.csv")

estadísticas_educación$MUNICIPIO
educacion<-subset(estadísticas_educación,AÑO==2022)

a<-intersect(estadisticos_mun$COLE_COD_MCPIO_UBICACION,educacion$CÓDIGO_MUNICIPIO)

b<-setdiff(estadisticos_mun$COLE_COD_MCPIO_UBICACION,a)

educacion<-subset(educacion,educacion$CÓDIGO_MUNICIPIO %in% estadisticos_mun$COLE_COD_MCPIO_UBICACION )

which(estadisticos_mun$COLE_COD_MCPIO_UBICACION==b[1])
which(estadisticos_mun$COLE_COD_MCPIO_UBICACION==b[2])


munm<-st_read("C:/Users/PC1/Downloads/MGN_MPIO_POLITICO.shp")
munm<-within(munm,{
  mun<-MPIO_CDPMP
})
educacion<-within(educacion,{
  mun<-CÓDIGO_MUNICIPIO
})
mapa_municipios<-munm%>%
  left_join(y=educacion, by = "mun")

mpios <-icfes %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(pglobal=mean(PUNT_GLOBAL)) %>%
  as.data.frame()

educacion<-within(educacion,{
  mun<-CÓDIGO_MUNICIPIO
})

mpios<-within(mpios,{
  mun<-COLE_COD_MCPIO_UBICACION
})



mapa_mpios<-munm%>%
  left_join(y=mpios, by = "mun")




g1mun<-ggplot() +
  geom_sf(data = mapa_mpios$geometry, aes(fill=mapa_mpios$pglobal), color = "darkgray", linetype = "solid") +
  labs(title = "Puntaje global por municipio",  
       fill = "Puntaje global")+
  scale_fill_gradient(low="white",high="purple")


g2mun<-ggplot() +
  geom_sf(data = mapa_municipios$geometry, aes(fill = mapa_municipios$COBERTURA_NETA_SECUNDARIA), color = "darkgray", linetype = "solid") +
  labs(title = "Cobertura neta secundaria por municipio",  
       fill = "Cobertura neta secundaria")+
  scale_fill_gradient(low="white",high="purple")

library(gridExtra)
# Establecer el diseño de la figura
par(mfrow = c(1, 2))

# Mostrar ambos gráficos en una sola figura
grid.arrange(g1mun, g2mun, ncol = 2)



# MODELO 1

#previas
mu0 <- 250  
t20  <- 50^2
s20 <- 50^2
nu0 <- 1

# número de muestras 
B <- 101000
# matriz para almacenar las muestras
THETA_1 <- matrix(data = NA, nrow = (B-1000)/10, ncol = 2)
LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)

# ALGORITMO (muestreador de Gibbs)
# inicializar

set.seed(1234)
isig2<-rgamma(n=1, shape = nu0/2, rate=nu0*s20/2) #GI (shape, rate), G(shape, scale)
#cadena
set.seed(1234)
i<-1
for(b in 1:B) {
  # actualizar theta
  t2n   <- 1/(1/t20 + n*isig2)      
  mun   <- t2n*(mu0/t20 + isig2*sum_y)
  theta <- rnorm(n = 1, mean = mun, sd = sqrt(t2n))
  # actualizar sigma^2
  nun   <- nu0 + n
  s2n   <- (nu0*s20 + sum((y - theta)^2))/nun
  isig2 <- rgamma(n = 1, shape = nun/2, rate = nun*s2n/2)
  # almacenar
  if ((b-1) %% 10 == 0 & b > 1000){
    THETA_1[i,] <- c(theta, isig2)
    LL[i] <- sum(dnorm(x = y, mean = theta, sd = sqrt(1/isig2), log = TRUE))
    i <- i + 1
  }
  # progreso
  
}


colnames(THETA_1) <- c("theta", "isig2")


THETA_1 <- as.data.frame(THETA_1)
LL    <- as.data.frame(LL)
list(THETA_1 = THETA_1, LL = LL)



# MODELO 2

# hiperparámetros
mu0  <- 50 
g20  <- 25
eta0 <- 1  
t20  <- 100
nu0  <- 1  
s20  <- 100

# algoritmo modelo 2

MCMC2 <- function(B, nj, ybj, s2j, mu0, g20, eta0, t20, nu0, s20) {
  # tamaños
  n <- sum(nj)
  m <- length(nj)
  # valores iniciales
  theta <- ybj
  sig2  <- mean(s2j)
  mu    <- mean(theta)
  tau2  <- var(theta)
  i<-1
  # almacenamiento
  THETA2 <- matrix(data = NA, nrow = (B-1000)/10, ncol = m+3)
  LL2    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/tau2 + nj/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nj*ybj/sig2), sd = sqrt(vtheta))
    # actualizar sigma^2
    sig2 <- 1/rgamma(n = 1, shape = 0.5*(nu0 + n), rate = 0.5*(nu0*s20 + sum((nj-1)*s2j + nj*(ybj - theta)^2)))
    # actualizar mu
    vmu <- 1/(1/g20 + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(mu0/g20 + m*mean(theta)/tau2), sd = sqrt(vmu)) 
    # actualizar tau^2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(eta0 + m), rate = 0.5*(eta0*t20 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    # almacenar valores
    if ((b-1) %% 10 == 0 & b > 1000){
    THETA2[i,] <- c(theta, sig2, mu, tau2)
    # log-verosimilitud
    LL2[i] <- sum(dnorm(x = y, mean = rep(theta, nj), sd = sqrt(sig2), log = T))
    i <- i + 1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA2) <- c(paste0("theta",1:m), "sig2", "mu", "tau2")
  colnames(LL2) <- c("ll")
  THETA2 <- as.data.frame(THETA2)
  LL2    <- as.data.frame(LL2)
  return(list(THETA2 = THETA2, LL2 = LL2))
}


# ajuste del modelo 2
tictoc::tic()
set.seed(123)
chain2 <- MCMC2(B = 101000, nj, ybj, s2j, mu0, g20, eta0, t20, nu0, s20)
tictoc::toc()


# MODELO 3

# hiperparámetros
mu0  <- 50 
g20  <- 25
eta0 <- 1  
t20  <- 100
lam0 <- 1  
al0  <- 1
be0  <- 1/100 
nus0 <- 1:50  

# algoritmo modelo 3
MCMC3 <- function(B, nj, ybj, s2j, mu0, g20, eta0, t20, lam0, al0, be0, nus0) {
  # tamaños
  n <- sum(nj)
  m <- length(nj)
  # valores iniciales
  theta <- ybj
  sig2  <- s2j  # sigma_j^2
  mu    <- mean(theta)
  tau2  <- var(theta)
  nu    <- 1
  ups2  <- 100  # sigma^2
  i<-1
  # almacenamiento
  THETA3 <- matrix(data = NA, nrow = (B-1000)/10, ncol = 2*m+4)
  LL3    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/tau2 + nj/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nj*ybj/sig2), sd = sqrt(vtheta))
    # actualizar sigma_j^2
    sig2 <- 1/rgamma(n = m, shape = 0.5*(nu + nj), rate = 0.5*(nu*ups2 + (nj-1)*s2j + nj*(ybj - theta)^2))
    # actualizar mu
    vmu <- 1/(1/g20 + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(mu0/g20 + m*mean(theta)/tau2), sd = sqrt(vmu))
    # actualizar tau2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(eta0+m), rate = 0.5*(eta0*t20 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    # actualizar nu
    lpnu <- 0.5*m*nus0*log(0.5*nus0*ups2) - m*lgamma(0.5*nus0) - 0.5*nus0*sum(log(sig2)) - nus0*(lam0 + 0.5*ups2*sum(1/sig2))
    nu <- sample(x = nus0, size = 1, prob = exp(lpnu - max(lpnu)))
    # actualizar sigma^2
    ups2 <- rgamma(n = 1, shape = al0 + 0.5*m*nu, rate = be0 + 0.5*nu*sum(1/sig2))
    # almacenar
    if ((b-1) %% 10 == 0 & b >1000){
    THETA3[i,] <- c(theta, sig2, mu, tau2, nu, ups2)
    # log-verosimilitud
    LL3[i] <- sum(dnorm(x = y, mean = rep(theta, nj), sd = sqrt(rep(sig2, nj)), log = T))
    i <- i + 1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA3) <- c(paste0("theta", 1:m), paste0("sig2", 1:m), "mu", "tau2", "nu", "ups2")
  colnames(LL3) <- c("ll")
  THETA3 <- as.data.frame(THETA3)
  LL3    <- as.data.frame(LL3)
  return(list(THETA3 = THETA3, LL3 = LL3))
}


# ajuste del modelo 3
tictoc::tic()
set.seed(1234)
chain3 <- MCMC3(B = 101000, nj, ybj, s2j, mu0, g20, eta0, t20, lam0, al0, be0, nus0)
tictoc::toc()


# MODELO 4

#previa

eps0<-1
k20<-50^2
mu0<-250
g20<-50^2
eta0<-1
t20<-50^2
nu0<-1
s20<-50^2



# algoritmo modelo 4
MCMC4 <- function(B, njk, yb_mun, s2_mun,nk,yb_dep,s2_dep,eps0,k20,mu0,g20,eta0,t20,nu0,s20) {
  
  # valores iniciales
  zitajk<-yb_mun
  k2<-mean(s2_mun)
  thetak<-yb_dep
  mu<-mean(thetak)
  tau2<-var(thetak)
  sigma2<-mean(s2_dep,na.rm=TRUE)
  i=1
  # almacenamiento
  THETA4 <- matrix(data = NA, nrow = (B-1000)/10, ncol = n_mun+m+4)
  LL4    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar zita
    vzita <- 1/(njk/k2 + 1/sigma2)
    zitajk <- rnorm(n=n_mun,mean=vzita*((njk*yb_mun/k2)+(thetak/sigma2)),sd=sqrt(vzita))
    # actualizar kappa^2
    k2 <- 1/rgamma(n = 1, shape = 0.5*(n + eps0), rate = 0.5*(eps0*k20 + sum((y-rep(zitajk,njk))^2)))
    # actualizar thetak
    vtheta <- 1/(nk/sigma2 + 1/tau2)
    subvectores_dep <- split(zitajk, rep(1:m, nk))
    # Calcular las sumas acumulativas en cada subvector
    med_dep <- as.vector(sapply(subvectores_dep, mean))
    thetak<- rnorm(n=m,mean=vtheta*(nk*med_dep/sigma2 + mu/tau2),sd=sqrt(vtheta))
    # actualizar sigma2
    sigma2 <- 1/rgamma(n = 1, shape = 0.5*(nu0+n_mun), rate = 0.5*(nu0*s20 + sum((zitajk-rep(thetak,nk))^2)))
    # actualizar mu
    vmu <- 1/(m/tau2 + 1/g20)
    mu <- rnorm(n=1,mean=vmu*(m*mean(thetak)/tau2 + mu0/g20),sd=sqrt(vmu))
    # actualizar tau^2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(m+eta0), rate = 0.5*(eta0*t20 + sum((thetak - mu)^2)))
    
    # almacenar
    if ((b-1) %% 10 == 0 & b > 1000){
    THETA4[i,] <- c(zitajk,k2,thetak,sigma2,mu,tau2)
    
    # log-verosimilitud
    LL4[i] <- sum(dnorm(x = y, mean = rep(zitajk, njk), sd = sqrt(k2), log = T))
    i <- i+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA4) <- c(paste0("zitajk", 1:n_mun),"k2", paste0("thetak", 1:length(nk)), "sigma2", "mu", "tau2")
  colnames(LL4) <- c("ll")
  THETA4 <- as.data.frame(THETA4)
  LL4    <- as.data.frame(LL4)
  return(list(THETA4 = THETA4, LL4 = LL4))
}



# ajuste del modelo 4 
tictoc::tic()
set.seed(1234)
chain4 <- MCMC4(B = 101000, njk, yb_mun, s2_mun,nk,yb_dep,s2_dep,eps0,k20,mu0,g20,eta0,t20,nu0,s20)
tictoc::toc()



# MODELO 5
eps0<-1
k20<-50^2
mu0<-250
g20<-50^2
eta0<-1
t20<-50^2
al0<-1
b0<-1/50^2


s2_dep1<-s2_dep
s2_dep1[3]<-200

# algoritmo modelo 5
MCMC_5 <- function(B, njk, yb_mun, s2_mun,nk,yb_dep,s2_dep,eps0,k20,mu0,g20,eta0,t20) {
  
  # valores iniciales
  zitajk<-yb_mun
  k2<-mean(s2_mun)
  thetak<-yb_dep
  sigma2k<-s2_dep1
  mu<-mean(thetak)
  tau2<-var(thetak)
  sigma2<-50^2
  nu<-1
  i <- 1
  # almacenamiento
  THETA5 <- matrix(data = NA, nrow = (B-1000)/10, ncol = n_mun+2*m+4)
  LL5    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  kmd <- matrix(data=NA, nrow = (B-1000)/10, ncol = m)
  kmm <- matrix(data=NA, nrow=(B-1000)/10, ncol=n_mun)
  pobrezanomed<-matrix(data=NA,nrow=(B-1000)/10,ncol=8)
  educacion_nomed<-matrix(data=NA,nrow=(B-1000)/10,ncol=2)
  # cadena
  for (b in 1:B) {
    # actualizar zita
    vzita <- 1/(njk/k2 + 1/sigma2k)
    zitajk <- rnorm(n=n_mun,mean=vzita*((njk*yb_mun/k2)+(thetak/sigma2k)),sd=sqrt(vzita))
    # actualizar kappa^2
    k2 <- 1/rgamma(n = 1, shape = 0.5*(n + eps0), rate = 0.5*(eps0*k20 +sum(((y-rep(zitajk,njk))^2))))
    # actualizar thetak
    vtheta <- 1/(nk/sigma2k + 1/tau2)
    subvectores_dep <- split(zitajk, rep(1:m, nk))
    # Calcular las sumas acumulativas en cada subvector
    med_dep <- as.vector(sapply(subvectores_dep, mean))
    thetak<- rnorm(n=m,mean=vtheta*(nk*med_dep/sigma2k + mu/tau2),sd=sqrt(vtheta))
    # actualizar sigma2k
    subvectores <- split((zitajk-rep(thetak,nk))^2, rep(1:m, nk))
    # Calcular las sumas acumulativas en cada subvector
    var_dep <- sapply(subvectores, sum)
    sigma2k <- 1/rgamma(n = m, shape = 0.5*(nk+nu), rate = 0.5*(nu*sigma2 + var_dep))
    # actualizar mu
    vmu <- 1/(m/tau2 + 1/g20)
    mu <- rnorm(n=1,mean=vmu*(m*mean(thetak)/tau2 + mu0/g20),sd=sqrt(vmu))
    # actualizar tau^2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(m+eta0), rate = 0.5*(eta0*t20 + sum((thetak-mu)^2)))
    # actualizar sigma2
    sigma2 <- rgamma(n = 1, shape = 0.5*(m*nu + al0),rate = 0.5*(b0 + nu*sum(1/sigma2k)))
    
    # almacenar
    if ((b-1) %% 10 == 0 & b >=1000){
      THETA5[i,] <- c(zitajk,k2,thetak,sigma2k,mu,tau2,sigma2)
      # log-verosimilitud
      LL5[i] <- sum(dnorm(x = y, mean = rep(zitajk, njk), sd = sqrt(rep(k2,n_mun)), log = T))
      #kmeans departamentos
      kmd[i,] <- kmeans(thetak,centers=5)$cluster
      #kmeans municipios
      kmm[i,] <- kmeans(zitajk,centers=8)$cluster
      #regresión pobreza
      medias_dep<-thetak[1:24]
      lm_dep<-lm(pobreza$incidencia~medias_dep)
      new_dep<-data.frame(medias_dep<-thetak[25:32])
      pobrezanomed[i,]<-predict(lm_dep,newdata = new_dep,type="response")
      #regresión educación
      medias_mun<-zitajk[-c(582,1098)]
      lm_mun<-lm(educacion$COBERTURA_NETA_SECUNDARIA~medias_mun)
      new_mun<-data.frame(medias_mun<-zitajk[c(582,1098)])
      educacion_nomed[i,]<-predict(lm_mun,newdata = new_mun,type="response")
      
      i <- i + 1
    }
  }  
  # fin de la cadena
  # salida
  colnames(THETA5) <- c(paste0("zitajk", 1:n_mun),"k2", paste0("thetak", 1:m),paste0("sigma2k", 1:m), "mu", "tau2", "sigma2")
  colnames(LL5) <- c("ll")
  THETA5 <- as.data.frame(THETA5)
  LL5    <- as.data.frame(LL5)
  pobrezanomed<-as.data.frame(pobrezanomed)
  return(list(THETA5 = THETA5, LL5 = LL5,kmd=kmd,pobrezanomed=pobrezanomed,kmm=kmm,educacion_nomed=educacion_nomed))
}



# ajuste del modelo 5
tictoc::tic()
set.seed(1234)
chain5 <- MCMC_5(B = 101000, njk, yb_mun, s2_mun,nk,yb_dep,s2_dep,eps0,k20,mu0,g20,eta0,t20)
tictoc::toc()



######PUNTO 4######

par(mfrow = c(2,2), mar = c(2.75,2.75,1.5,0.5), mgp = c(1.7,0.7,0))
plot(chain2$LL2$ll,      type = "p", pch = ".", cex = 1.1, xlab = "Iteracion", ylab = "Log-verosimilitud",col="red",main="Modelo 2")
plot(chain3$LL3$ll,      type = "p", pch = ".", cex = 1.1, xlab = "Iteracion", ylab = "Log-verosimilitud",col="blue",main="Modelo 3")
plot(chain4$LL4$ll,      type = "p", pch = ".", cex = 1.1, xlab = "Iteracion", ylab = "Log-verosimilitud",main="Modelo 4")
plot(chain5$LL5$ll,      type = "p", pch = ".", cex = 1.1, xlab = "Iteracion", ylab = "Log-verosimilitud",col="purple",main="Modelo 5")


#coeficientes de variación

neffm2 <- coda::effectiveSize(chain2$THETA2)

EEMCm2 <- apply(X = chain2$THETA2, MARGIN = 2, FUN = sd)/sqrt(neffm2)
round(summary(EEMCm2), 3)

par(mfrow=c(2,2))
hist(EEMCm2[1:32],col="red",main="EMC departamentos Modelo 2",xlab="EMC",ylab="Frecuencia")
hist(EEMCm3[1:32],col="blue",main="EMC departamentos Modelo 3",xlab="EMC",ylab="Frecuencia")
hist(EEMCm4[1114:1146],col="black",main="EMC departamentos Modelo 4",xlab="EMC",ylab="Frecuencia")
hist(EEMCm5[1114:1146],col="purple",main="EMC departamentos Modelo 5",xlab="EMC",ylab="Frecuencia")


neffm3 <- coda::effectiveSize(chain3$THETA3)

EEMCm3 <- apply(X = chain3$THETA3, MARGIN = 2, FUN = sd)/sqrt(neffm3)
round(summary(EEMCm3), 3)


neffm4 <- coda::effectiveSize(chain4$THETA4)

EEMCm4 <- apply(X = chain4$THETA4, MARGIN = 2, FUN = sd)/sqrt(neffm4)
round(summary(EEMCm4), 3)


neffm5 <- coda::effectiveSize(chain5$THETA5)

EEMCm5 <- apply(X = chain5$THETA5, MARGIN = 2, FUN = sd)/sqrt(neffm5)
round(summary(EEMCm5), 3)

#EMC departamentos
par(mfrow=c(2,2))
hist(EEMCm2[1:32],col="red",main="CV medias pordepartamento Modelo 2",xlab="EMC",ylab="Frecuencia")
hist(EEMCm3[1:32],col="blue",main="CV medias por departamento Modelo 3",xlab="EMC",ylab="Frecuencia")
hist(EEMCm4[1114:1146],col="black",main="CV medias por departamento Modelo 4",xlab="EMC",ylab="Frecuencia")
hist(EEMCm5[1114:1146],col="purple",main="CV medias por departamento Modelo 5",xlab="EMC",ylab="Frecuencia")

#EMC varianzas por departamento
par(mfrow=c(1,2))
hist(EEMCm3[33:65],col="blue",main="CV varianzas por departamento Modelo 3",xlab="EMC",ylab="Frecuencia")
hist(EEMCm5[1145:1177],col="purple",main="CV varianzas por departamento Modelo 5",xlab="EMC",ylab="Frecuencia")

#EMC medias por municipio
par(mfrow=c(1,2))
hist(EEMCm4[1:1112],col="black",main="CV medias por municipio Modelo 4",xlab="EMC",ylab="Frecuencia")
hist(EEMCm5[1:1112],col="purple",main="CV medias por municipio Modelo 5",xlab="EMC",ylab="Frecuencia")

EEMCm2


#Criterios de información

#DIC MODELO 1

#DIC

LP1 <- as.numeric(LL$V1)
theta_hat <- mean(THETA_1[,1])
sigma2_hat <- mean(THETA_1[,2])
lpyth_m1 <- sum(dnorm(x = y, mean = theta_hat, sd = sqrt(1/sigma2_hat), log = TRUE))
pDIC_m1 <- 2 * (lpyth_m1 - mean(LP1))
dic_m1 <- -2 * lpyth_m1 + 2 * pDIC_m1
dic_m1

# Inicializar variables para el cálculo del WAIC
lppd_m1 <- 0
pWAIC_m1 <- 0

mean_theta <- mean(THETA_1[, 1])
var_sigma2 <- var(THETA_1[, 2])

for (i in 1:n) {
  
  # lppd
  tmp1 <- dnorm(x = y[i], mean = mean_theta, sd = sqrt(1 / var_sigma2))
  lppd_m1 <- lppd_m1 + log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = mean_theta, sd = sqrt(1 / var_sigma2), log = TRUE)
  pWAIC_m1 <- pWAIC_m1 + 2 * (log(mean(tmp1)) - mean(tmp2))
}

# Calcular WAIC
waic_m1 <- -2*lppd_m1 +2*pWAIC_m1
waic_m1



#DIC MODELO 2
# DIC
LP2<- as.numeric(chain2$LL2$ll)
theta_hat2  <- colMeans(chain2$THETA2[,1:m])
sigma2_hat2 <- mean(chain2$THETA2$sig2)
lpyth_m2   <- sum(dnorm(x = y, mean = rep(theta_hat2, nj), sd = sqrt(sigma2_hat2), log = T))
pDIC_m2    <- 2*(lpyth_m2 - mean(LP2))
dic_m2     <- -2*lpyth_m2 + 2*pDIC_m2 
dic_m2
pDIC_m2
# WAIC
g2 <- rep(NA, n)
for (j in 1:m) {
  idx <- icfes$COLE_COD_DEPTO_UBICACION == unique(icfes$COLE_COD_DEPTO_UBICACION)[j]
  g2[idx] <- j
  Y[[j]] <- y[idx]
}

lppd_m2  <- 0
pWAIC_m2 <- 0
for (i in 1:n) {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain2$THETA2[,g2[i]], sd = sqrt(chain2$THETA2$sig2))
  lppd_m2 <- lppd_m2 + log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain2$THETA2[,g2[i]], sd =  sqrt(chain2$THETA2$sig2), log = T)
  pWAIC_m2 <- pWAIC_m2 + 2*(log(mean(tmp1)) - mean(tmp2))
}
waic_m2 <- -2*lppd_m2 + 2*pWAIC_m2


#DIC MODELO 3
# DIC
LP3        <- as.numeric(chain3$LL3$ll)
theta_hat3  <- colMeans(chain3$THETA3[,1:m])
sigma2_hat3 <- colMeans(chain3$THETA3[,(m+1):(2*m)])
lpyth_m3   <- sum(dnorm(x = y, mean = rep(theta_hat3, nj), sd = sqrt(rep(sigma2_hat3, nj)), log = T))
pDIC_m3    <- 2*(lpyth_m3 - mean(LP3))
dic_m3     <- -2*lpyth_m3 + 2*pDIC_m3
# WAIC

g3 <- rep(NA, n)
for (j in 1:m) {
  idx <- icfes$COLE_COD_DEPTO_UBICACION == unique(icfes$COLE_COD_DEPTO_UBICACION)[j]
  g3[idx] <- j
  Y[[j]] <- y[idx]
}


lppd_m3  <- 0
pWAIC_m3 <- 0
for (i in 1:n) {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain3$THETA3[,g3[i]], sd = sqrt(chain3$THETA3[,m+g3[i]]))
  lppd_m3 <- lppd_m3 + log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain3$THETA3[,g3[i]], sd = sqrt(chain3$THETA3[,m+g3[i]]), log = T)
  pWAIC_m3 <- pWAIC_m3+ 2*(log(mean(tmp1)) - mean(tmp2))
}
waic_m3 <- -2*lppd_m3 + 2*pWAIC_m3

#DIC MODELO 4

# DIC
LP4        <- as.numeric(chain4$LL4$ll)
zita_hat4  <- colMeans(chain4$THETA4[,1:n_mun])
k2_hat4 <- mean(chain4$THETA4$k2)
lpyth_m4   <- sum(dnorm(x = y, mean = rep(zita_hat4, njk), sd = sqrt(k2_hat4), log = T))
pDIC_m4    <- 2*(lpyth_m4 - mean(LP4))
dic_m4     <- -2*lpyth_m4 + 2*pDIC_m4 

dic_m4
pDIC_m4

# WAIC
g4 <- rep(NA, n)
for (j in 1:n_mun) {
  idx <- icfes$COLE_COD_MCPIO_UBICACION == unique(icfes$COLE_COD_MCPIO_UBICACION)[j]
  g4[idx] <- j
  Y[[j]] <- y[idx]
}
lppd_m4  <- 0
pWAIC_m4 <- 0
for (i in 1:n) {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain4$THETA4[,g4[i]], sd = sqrt(chain4$THETA4$k2))
  lppd_m4 <- lppd_m4 + log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain4$THETA4[,g4[i]], sd =  sqrt(chain4$THETA4$k2), log = T)
  pWAIC_m4 <- pWAIC_m4 + 2*(log(mean(tmp1)) - mean(tmp2))
}
waic_m4 <- -2*lppd_m4 + 2*pWAIC_m4


#DIC MODELO 5
# DIC
LP5        <- as.numeric(chain5$LL5$ll)
zita_hat5  <- colMeans(chain5$THETA5[,1:n_mun])
k2_hat5 <- mean(chain5$THETA5$k2)
lpyth_m5   <- sum(dnorm(x = y, mean = rep(zita_hat5, njk), sd = sqrt(k2_hat5), log = T))
pDIC_m5    <- 2*(lpyth_m5 - mean(LP5))
dic_m5     <- -2*lpyth_m5 + 2*pDIC_m5 

dic_m5
pDIC_m5

# WAIC
lppd_m5  <- 0
pWAIC_m5 <- 0
for (i in 1:n) {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain5$THETA5[,g4[i]], sd = sqrt(chain5$THETA5$k2))
  lppd_m5 <- lppd_m5 + log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain5$THETA5[,g4[i]], sd =  sqrt(chain5$THETA5$k2), log = T)
  pWAIC_m5 <- pWAIC_m5 + 2*(log(mean(tmp1)) - mean(tmp2))
}
waic_m5 <- -2*lppd_m5 + 2*pWAIC_m5


# PUNTO 6

mean(chain2$THETA2$mu)
quantile(chain2$THETA2$mu,c(0.025,0.975))

mean(chain3$THETA3$mu)
quantile(chain3$THETA3$mu,c(0.025,0.975))

mean(chain4$THETA4$mu)
quantile(chain4$THETA4$mu,c(0.025,0.975))

mean(chain5$THETA5$mu)
quantile(chain5$THETA5$mu,c(0.025,0.975))






#PUNTO 7

# ranking bayesiano: Modelo 5
THETA <- chain5$THETA5
ids2  <- estadisticos_dep$nombre_dep
that  <- colMeans(THETA[,1114:1145])
ic1   <- apply(X = THETA[,1114:1145], MARGIN = 2, FUN = function(x) quantile(x, c(0.025,0.975)))
ranking <- order(that) 
ids2 <- ids2[ ranking]
that <- that[ ranking]
ic1  <- ic1 [,ranking]
colo <- rep(2,m)
colo[which(ic1[1,]>260)] <- 1
colo[which(ic1[2,]<240)] <- 3
colo <- c("green","black","red")[colo]
# grafico
par(mfrow = c(1,2))
plot(NA, NA, xlab = "Puntaje", ylab = "", main = "Ranking Bayesisano", xlim = c(190,280), ylim = c(1,m), cex.axis = 0.75, yaxt = "n")
axis(side = 2, at = 1:m, labels = ids2, las = 2)
abline(v = 250,  col = "gray", lwd = 3)
abline(h = 1:m, col = "lightgray", lwd = 1)
for (j in 1:m) {
  segments(x0 = ic1[1,j], y0 = j, x1 = ic1[2,j], y1 = j, col = colo[j])
  lines(x = that[j], y = j, type = "p", pch = 16, cex = 0.8, col = colo[j])
}

# ranking frecuentista
ids2 <- estadisticos_dep$nombre_dep
that <- yb_dep
ic1  <- NULL
for (j in 1:m)
  ic1  <- cbind(ic1, yb_dep[j] + c(-1,1)*qnorm(p = 0.975)*sqrt(s2_dep1[j])/sqrt(nk[j]))
ranking <- order(that) 
ids2 <- ids2[ ranking]
that <- that[ ranking]
ic1  <- ic1 [,ranking]
colo <- rep(2,m)
colo[which(ic1[1,]>260)] <- 1
colo[which(ic1[2,]<240)] <- 3
colo <- c("green","black","red")[colo]
# gráfico
plot(NA, NA, xlab = "Puntaje", ylab = "", main = "Ranking Frecuentista", xlim = c(190,280), ylim = c(1,m), cex.axis = 0.75, yaxt = "n")
axis(side = 2, at = 1:m, labels = ids2, las = 2)
abline(v = 250,  col = "gray", lwd = 3)
abline(h = 1:m, col = "lightgray", lwd = 1)
for (j in 1:m) {
  segments(x0 = ic1[1,j], y0 = j, x1 = ic1[2,j], y1 = j, col = colo[j])
  lines(x = that[j], y = j, type = "p", pch = 16, cex = 0.8, col = colo[j])
}





# PUNTO 8

set.seed(1234)
kmp<-kmeans(colMeans(chain5$THETA5[,1114:1145]),centers = 5)
kmp

dep<-cbind(estadisticos_dep,kmp$cluster)



dep <- within(dep,{
  departamento <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(nombre_dep))
})

mapacluster<-mapa%>%
  left_join(y=dep, by="departamento")


# Crea el gráfico con ggplot
ggplot() +
  geom_sf(data = mapacluster$geometry, aes(fill = factor(mapacluster$`kmp$cluster`)), color = "darkblue", linetype = "solid") +
  labs(title = "Segmentación departamental",  
       fill = "cluster")
# matriz de incidencia
A <- matrix(data = 0, nrow = m, ncol = m)
B <- nrow(chain5$kmd)
for (b in 1:B) {
  for (i in 1:(m-1)) {
    for (j in (i+1):m) {
      if (chain5$kmd[b,i] == chain5$kmd[b,j]) {
        A[i,j] <- A[i,j] + 1/B
      } 
    }
  }
}
A <- A + t(A)
diag(A) <- 1
# se organizan las observaciones de acuerdo al ranking por departamento
indices <- ranking
A <- A[indices,indices]
# visualización de la matriz de incidencia
par(mfrow=c(1,1))
par(mar = c(2.75,2.75,0.5,0.5), mgp = c(1.7,0.7,0))
corrplot::corrplot(corr = A, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "n")







#arauca, casanare, putumayo, amazonas, guainia, guaviare, vaupes, vichada


# PUNTO 9

colMeans(chain5$pobrezanomed)

IC<-matrix(data=NA,nrow=8,ncol=2)
for(i in 1:8){
  IC[i,]<-quantile(chain5$pobrezanomed[,i],probs = c(0.025,0.975))
}

IC

incidencia_nomed<-as.vector(colMeans(chain5$pobrezanomed))
departamentos_nomed<-as.vector(estadisticos_dep$nombre_dep[25:32])

a<-data.frame(departamento=departamentos_nomed,incidencia=incidencia_nomed)
pobrezatotal<-rbind(pobreza,a)

mapa_pobrezatotal<-mapa%>%
  left_join(y=pobrezatotal,by="departamento")

ggplot() +
  geom_sf(data=mapa_pobrezatotal$geometry,aes(fill=mapa_pobrezatotal$incidencia),col="darkgray",linetype="solid")+
  scale_fill_gradient(low="white",high="green")+
  labs(title = "Incidencia de la pobreza por departamento",  
       fill = "Incidencia de pobreza") +
  geom_sf_text(data=mapa_pobrezatotal,aes(label=ifelse(departamento %in% c("ARAUCA","CASANARE","PUTUMAYO","AMAZONAS","GUAINIA","GUAVIARE","VAUPES","VICHADA"),departamento,"")),col="black",
               fontface="bold",size=2,fun.geometry=function(x) sf::st_centroid(x)) 



# PUNTO 10

THETA5m <- chain5$THETA5
thatm  <- colMeans(THETA5m[,1:1112])
rankingm <- order(thatm)

# matriz de incidencia municipios
A <- matrix(data = 0, nrow = n_mun, ncol = n_mun)
B=nrow(chain5$kmm)
for (b in 1:B) {
  for (i in 1:(n_mun-1)) {
    for (j in (i+1):n_mun) {
      if (chain5$kmm[b,i] == chain5$kmm[b,j]) {
        A[i,j] <- A[i,j] + 1/B
      } 
    }
  }
}
A <- A + t(A)
diag(A) <- 1
# se organizan las observaciones de acuerdo al ranking por departamento
indices <- rankingm
A <- A[indices,indices]
# visualización de la matriz de incidencia
par(mfrow=c(1,1))
par(mar = c(2.75,2.75,0.5,0.5), mgp = c(1.7,0.7,0))
tictoc::tic()

corrplot::corrplot(corr = A, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "n")

set.seed(1234)
kmpmun<-kmeans(colMeans(chain5$THETA5[,1:1112]),centers = 8)
kmpmun

mun<-cbind(estadisticos_mun,kmpmun$cluster)



mun <- within(mun,{
  mun <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(COLE_COD_MCPIO_UBICACION))
})

mapaclustermun<-munm%>%
  left_join(y=mun, by="mun")


ggplot() +
  geom_sf(data = mapaclustermun$geometry, aes(fill = factor(mapaclustermun$`kmpmun$cluster`)), color = "darkblue", linetype = "solid") +
  labs(title = "Segmentación municipal",  
       fill = "cluster")



# PUNTO 11

colMeans(chain5$educacion_nomed)

IC<-matrix(data=NA,nrow=2,ncol=2)
for(i in 1:2){
  IC[i,]<-quantile(chain5$educacion_nomed[,i],probs = c(0.025,0.975))
}

IC

educacion1<-educacion[,c("MUNICIPIO","mun","COBERTURA_NETA_SECUNDARIA")]
educacion1

nombres<-c("Belén de Bajirá","Mapiripana")
mun<-c(27086,94663)
cobertura<-c(65.14,65.55)

df<-data.frame(MUNICIPIO=nombres,mun=mun,COBERTURA_NETA_SECUNDARIA=cobertura)

educacion1<-rbind(educacion1,df)

mapa_municipiosnomed<-munm%>%
  left_join(y=educacion1,by="mun")

ggplot() +
  geom_sf(data=mapa_municipiosnomed$geometry,aes(fill=mapa_municipiosnomed$COBERTURA_NETA_SECUNDARIA),col="darkgray",linetype="solid")+
  scale_fill_gradient(low="white",high="purple")+
  labs(title = "Cobertura neta secundaria por municipio",  
       fill = "Cobertura neta secundaria") +
  geom_sf_text(data=mapa_municipiosnomed,aes(label=ifelse(MUNICIPIO %in% c("Belén de Bajirá","Mapiripana"),MUNICIPIO,"")),col="black",
               fontface="bold",size=3,fun.geometry=function(x) sf::st_centroid(x)) 

subset(munm,DPTO_CNMBR %in% c("Belén de Bajirá","Mapiripana"))

choco<-mapa_municipiosnomed%>%
  filter(DPTO_CNMBR=="CHOCÓ")

ggplot() +
  geom_sf(data=choco$geometry,aes(fill=choco$COBERTURA_NETA_SECUNDARIA),col="darkgray",linetype="solid")+
  scale_fill_gradient(low="white",high="purple")+
  labs(title = "Cobertura neta secuandria Chocó",  
       fill = "Cobertura neta secundaria") 

guainia<-mapa_municipiosnomed%>%
  filter(DPTO_CNMBR=="GUAINÍA")

ggplot() +
  geom_sf(data=guainia$geometry,aes(fill=guainia$COBERTURA_NETA_SECUNDARIA),col="darkgray",linetype="solid")+
  scale_fill_gradient(low="white",high="purple")+
  labs(title = "Cobertura neta secuandria Guainía",  
       fill = "Cobertura neta secundaria") 

# PUNTO 12

# BONDAD DE AJUSTE

#valores observados por municipio




ts_obs <- icfes%>%
  filter(COLE_COD_MCPIO_UBICACION %in% estadisticos_mun$COLE_COD_MCPIO_UBICACION)%>%
  group_by(COLE_COD_MCPIO_UBICACION)%>%
  summarise(min=min(PUNT_GLOBAL),max=max(PUNT_GLOBAL),IQR=IQR(PUNT_GLOBAL),med=round(mean(PUNT_GLOBAL), 3),mediana=median(PUNT_GLOBAL),sd=round(sd(PUNT_GLOBAL), 3))




media_posterior<-(chain5$THETA5[,1:1112])
sd_posterior<-chain5$THETA5$k2



muestras_min<-matrix(data=NA,nrow=10000,ncol=1112)
muestras_max<-matrix(data=NA,nrow=10000,ncol=1112)
muestras_IQR<-matrix(data=NA,nrow=10000,ncol=1112)
muestras_media_mean<-matrix(data=NA,nrow=10000,ncol=1112)
muestras_median<-matrix(data=NA,nrow=10000,ncol=1112)
muestras_desviacion_sd<-matrix(data=NA,nrow=10000,ncol=1112)

for (i in 1:1112) {
  for(j in 1:10000){
  # Tomar muestras de la distribución predictiva posterior para el municipio i
  muestras <- rnorm(njk[i], mean= media_posterior[[j,i]], sd = sd_posterior[i])
  
  # Calcular los estadísticos para las muestras
  muestras_min[j,i] <- min(muestras)
  muestras_max[j,i] <- max(muestras)
  muestras_IQR[j,i] <- IQR(muestras)
  muestras_media_mean[j,i] <- mean(muestras)
  muestras_median[j,i] <- median(muestras)
  muestras_desviacion_sd[j,i] <- sd(muestras)
  }
}

ppp_min<-NULL
ppp_max<-NULL
ppp_IQR<-NULL
ppp_media<-NULL
ppp_mediana<-NULL
ppp_sd<-NULL

for (i in 1:1112){
  ppp_min[i]<-sum(muestras_min[,i] < ts_obs$min)/10000
  ppp_max[i]<-sum(muestras_max[,i] < ts_obs$max)/10000
  ppp_IQR[i]<-sum(muestras_IQR[,i] < ts_obs$IQR)/10000
  ppp_media[i]<-sum(muestras_media_mean[,i] < ts_obs$med)/10000
  ppp_mediana[i]<-sum(muestras_median[,i] < ts_obs$mediana)/10000
  ppp_sd[i]<-sum(muestras_desviacion_sd[,i] < ts_obs$sd)/10000
}
  

df<-data.frame(ppp_min,ppp_max,ppp_IQR,ppp_media,ppp_mediana,ppp_sd)
colnames(df)<-c("mín","max","IQR","media","mediana","sd")

boxplot(df,col="purple",main="ppp estadísticos de prueba")
