##--------------------------------------------------------------------
##
##  Suppose we want to classify each county in the US into one
##  of the four clusters, but I'm just guessing.
##
##  For each county, I guess the clusters C1, C2, C3, and C4
##  with probabilities  1261/3108, 226/3108, 827/3108, 794/3108
##
##  Let's classify (by guessing) each of the 3108 counties
##  We'll estimate the sensitivity by taking the number of correct
##  guesses divided by the number of counties in each cluster.
##
##  We'll repeat 10000 times the random guessing over all 
##  3108 counties and make histograms for the sensitivity
##  obtained by guessing.
##
##--------------------------------------------------------------------


x = c( 1261 , 226 , 827 , 794 )
p = x / 3108
nsim = 10000
correct = matrix( NA , nrow=nsim , ncol=4 )

for( i in 1:nsim)
{
  choices = sample( x = c( rep('c1', 1261),
                           rep('c2', 226),
                           rep('c3', 827),
                           rep('c4', 794)), # four classes
                    size = 3108, # size of sample = sim_size
                    replace = TRUE ) %>%  # sample with replacement
            as.factor()
  correct[i,1] = sum( choices[ 1:x[1]] == "c1" )
  correct[i,2] = sum( choices[ (x[1]+1):(x[1]+x[2]) ] == "c2" )
  correct[i,3] = sum( choices[ (x[1]+x[2]+1):(x[1]+x[2]+x[3]) ] == "c3" )
  correct[i,4] = sum( choices[ (x[1]+x[2]+x[3]+1):3108 ] == "c4" )
}

windows( 12 , 10 )
par( mfrow=c(2,2) )
hist( correct[,1]/x[1] , xlim=c(0,1) , xlab="Sensitivity" , 
      main="Sensitivity for Cluster 1 if Guessing")
  abline(v=0.71)
hist( correct[,2]/x[2] , xlim=c(0,1) , xlab="Sensitivity" ,
      main="Sensitivity for Cluster 2 if Guessing")
  abline(v=0.42)
hist( correct[,3]/x[3] , xlim=c(0,1) , xlab="Sensitivity" ,
      main="Sensitivity for Cluster 3 if Guessing")
  abline(v=0.39)
hist( correct[,4]/x[4] , xlim=c(0,1) , xlab="Sensitivity" ,
      main="Sensitivity for Cluster 4 if Guessing")
  abline(v=0.74)

 
  
  
  
  
  
  
  x = c( 1261 , 226 , 827 , 794 )
  p = x / 3108
  nsim = 10000
  correct = matrix( NA , nrow=nsim , ncol=4 )
  
  for( i in 1:nsim)
  {
    choices = sample( x = c("c1","c2","c3","c4") ,
                      size = 3108, 
                      p = c(0.25,0.25,0.25,0.25) ,
                      replace = TRUE ) %>%  # sample with replacement
      as.factor()
    correct[i,1] = sum( choices[ 1:x[1]] == "c1" )
    correct[i,2] = sum( choices[ (x[1]+1):(x[1]+x[2]) ] == "c2" )
    correct[i,3] = sum( choices[ (x[1]+x[2]+1):(x[1]+x[2]+x[3]) ] == "c3" )
    correct[i,4] = sum( choices[ (x[1]+x[2]+x[3]+1):3108 ] == "c4" )
  }
  
  windows( 12 , 10 )
  par( mfrow=c(2,2) )
  hist( correct[,1]/x[1] , xlim=c(0,1) , xlab="Sensitivity" , 
        main="Sensitivity for Cluster 1 if Guessing")
  abline(v=0.71)
  hist( correct[,2]/x[2] , xlim=c(0,1) , xlab="Sensitivity" ,
        main="Sensitivity for Cluster 2 if Guessing")
  abline(v=0.42)
  hist( correct[,3]/x[3] , xlim=c(0,1) , xlab="Sensitivity" ,
        main="Sensitivity for Cluster 3 if Guessing")
  abline(v=0.39)
  hist( correct[,4]/x[4] , xlim=c(0,1) , xlab="Sensitivity" ,
        main="Sensitivity for Cluster 4 if Guessing")
  abline(v=0.74)
  

