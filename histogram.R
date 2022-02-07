data<-read.csv('C:/Users/james/Desktop/6A/STAT 371/Project/Proposal/cbb.csv')
data$win_prct<-data$W/data$G
hist(data$win_prct)
