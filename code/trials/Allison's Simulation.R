install.packages("caret")
library(caret)

nruns=1000
nsim=3108
pclass=c(1261/3108,226/3108,827/3108,794/3108)
pcorrect=c(0.25,0.25,0.25,0.25)
class=1:4
S=matrix(NA,nrow=nruns,ncol=4)
colnames(S)=c("C1","C2","C3","C4")
for(i in 1:nruns) {
actual=factor(sample(class,nsim,replace=T,prob=pclass))
predicted=factor(sample(class,nsim,replace=T,prob=pcorrect))
test=confusionMatrix(data=predicted,reference=actual)
S[i,]=test$byClass[,1]
}
S=as.data.frame(S)
p=ggplot(S,aes(x=C4))+
  geom_histogram(binwidth=0.005, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  theme_bw()
p  
summary(S)
