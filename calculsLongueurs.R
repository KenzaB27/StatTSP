set.seed(2704)

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

U1 <- rnorm(100)
U2 <- rnorm(100,5)
U3 <- rnorm(100,0,8)
U4 <- rnorm(100,10)
U5 <- rnorm(100,10,3)


mat <- cbind(X1,X2,X3,X4,X5)
par(mfrow=c(1,1))
boxplot(mat,notch=TRUE)



results<- c(X1,X2,X3,X4,X5)
methods<- c(rep("branch", 50),rep("nearest", 50),rep("repetitive_nn", 50),rep("farthest_insertion", 50),rep("two_opt", 50))

pairwise.t.test(results,methods,adjust.method="bonferroni")

