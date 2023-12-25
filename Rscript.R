#install.packages("markovchain", dependencies = TRUE)
library(markovchain)

# definim un vector care sa contina starile lantului Markov
states <- c('A', 'B', 'C', 'D')
states

# introducem matricea de trecere P pt 5%
#P <- matrix(c(0.22, 0.29, 0.26, 0.23,
#              0.25, 0.23, 0.29, 0.23,
#              0.19, 0.27,  0.21, 0.33,
#              0.35, 0.20, 0.23, 0.22),
#            nrow = 4,
#            byrow = TRUE,
#            dimnames=list(states,states))

P <- matrix(c(0.100, 0.375, 0.375, 0.150, #pt 3%
              0.122, 0.351, 0.412, 0.115,
              0.084, 0.342,  0.434, 0.140,
              0.163, 0.408, 0.266, 0.163),
            nrow = 4,
            byrow = TRUE,
            dimnames=list(states,states))
P 


# create markov chain
mcA <- new('markovchain',
           transitionMatrix=P)
mcA
plot(mcA)

absorptionProbabilities(mcA)
hittingProbabilities(mcA)



### Pentru aflarea distributiei
stationary <- function(mat) {
  x = eigen(t(mat))$vectors[,1]
  as.double(x/sum(x))
}

# distributia stationara a matricei de trecere P
distributie_stationara <- round(stationary(P), digits = 3) 
distributie_stationara
#0.110 0.358 0.397 0.135

# o functie ce calculeaza iterativ puteri ale unei matrice patratice
matrixpower <- function(matrice,k) {
  if (k == 0) return (diag(dim(matrice)[1])) 
  if (k == 1) return(matrice)
  if (k > 1) return( matrice %*% matrixpower(matrice, k-1))
}

#Identificarea distributiei limita
matrixpower(P, 5) 
matrixpower(P, 6) 
matrixpower(P, 7) 
distributie_limita <-matrixpower(P, 7)[1,]
distributie_limita
#0.1100424 0.3577790 0.3969115 0.1352671
#Observam ca distr limita ~= distr stat


#Durata medie de revenire intr-o stare
round(1/stationary(P), digits=3)



#Functie pentru simularea traiectoriei
markov <- function(init,matrice,n,labels) {
  if(missing(labels)) labels <- 1: length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  
  for (i in 2:(n+1))
  { simlist[i] <- sample(states,
                         1,
                         prob=matrice[simlist[i-1],]) }
  labels[simlist]
}

#Simularea traiectoriei pe 30 de pasi
distrib_initiala_alfa_X1<-c(0.25, 0.25, 0.25, 0.25)
traiectorie <- markov(distrib_initiala_alfa_X1,mcA,30,states)
traiectorie

length_traiectorie<-length(traiectorie)
distributie_absoluta<-table(traiectorie)
distributie_relativa<-table(traiectorie)/length_traiectorie
distributie_absoluta
distributie_relativa 
#0.1159420 0.3513243 0.3923038 0.1404298 

distributie_limita
distributie_stationara


#Replicarea traiectoriei de 10 000 de ori (pentru 10 000 de investitori)

sim <- replicate(10000,markov(distrib_initiala_alfa_X1,P,30,states)[31])
sim
#Pentru durata a 30 de zile, s-a simulat traiectoria pretului Bitcoin in 10 000 de cazuri separate
#iar rezultatul din ultima zi a fost stocat in sim

length(sim)# numarul de elemente/replicari/investitori
sim[1:25]

table(sim)
table(sim)/10000
#0.110 0.358 0.397 0.135
# Este aproximativ egal cu distributia stationara de asemenea