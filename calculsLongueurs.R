set.seed(2704)

X <- vector(mode="integer", length=50)
V <- vector(mode="integer", length=50)
n <-10
for(i in 1:50)
{
  sommets <- data.frame(x = runif(n), y = runif(n))
  couts <- distance(sommets)
  v<-TSPbranch(couts)
  V[i]<-v
  l<-calculeLongueur(couts, v)
  X[i]<-l
}
