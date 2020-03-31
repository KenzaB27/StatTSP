set.seed(2704)
#1.1 Longueur des chemins
# question prof: c quoi les retours des fonctions TSP
X1 <- vector(mode="integer", length=50)
X2 <- vector(mode="integer", length=50)
X3 <- vector(mode="integer", length=50)
X4 <- vector(mode="integer", length=50)
X5 <- vector(mode="integer", length=50)
n <-10
for(i in 1:50)
{
  sommets <- data.frame(x = runif(n), y = runif(n))
  couts <- distance(sommets)
  v1<-TSPbranch(couts)
  v2<-TSPnearest(couts)
  v3<-TSPsolve(couts, "repetitive_nn")
  v4<-TSPsolve(couts, "farthest_insertion")
  v5<-TSPsolve(couts, "two_opt")
  X1[i]<-v1
  X2[i]<-v2$longueur
  X3[i]<-v3
  X4[i]<-v4
  X5[i]<-v5
}
# affichage boxplot 
mat <- cbind(X1,X2,X3,X4,X5)
par(mfrow=c(1,1))
boxplot(mat,notch=TRUE)

# tests de comparaison
results<- c(X1,X2,X3,X4,X5)
methods<- c(rep("branch", 50),rep("nearest", 50),rep("repetitive_nn", 50),rep("farthest_insertion", 50),rep("two_opt", 50))

pairwise.t.test(results,methods,adjust.method="bonferroni")

#1.2 temps de calcul 

microbenchmark(TSPbranch(couts),TSPnearest(couts), TSPsolve(couts, "repetitive_nn"), TSPsolve(couts, "farthest_insertion"), TSPsolve(couts, "two_opt"), times=20, setup={ n<-10
  sommets <- data.frame(x = runif(n), y = runif(n))
  couts <- distance(sommets)
})


#2 Etude de la complexité de l'algorithme Branch&bound

## 2.1 Comportement par rapport au nombre de sommets: premier modèle 


sommets <- data.frame(x = runif(n), y = runif(n))
couts <- distance(sommets)

seqn <- seq(4,20,1)

#calcul de temps
temps<-matrix(0,nrow=17,ncol=10)
for ( i in 1:length(seqn)){
  temps[i,]<-microbenchmark(TSPsolve(couts, method = "branch"),
                           times = 10,
                           setup = { n <- seqn[i]
                           couts <- distance(cbind(x = runif(n), y = runif(n)))}
  )$time
}

# representation de temps

par(mfrow=c(1,2)) # 2 graphiques sur 1 ligne
matplot(seqn, temps, type ='p', xlab='n', ylab='temps')
matplot(seqn, log(temps)^2, xlab='n', ylab=expression(log(temps)^2))




