#Question 1
#part a) 
(crit.value.20 = qt(0.967, df=19))

#part b)
(crit.value.30 = qt(0.967, df=29))

#part c)
#install.packages("VGAM")
library(VGAM)
set.seed(7272)
simulations = 10000 #number of simulations
n = 30 #total sample size
n.peek = 20 #sample size for peeking 
rejections = 0 #counter for number of rejections (rejecting the null)
for(i in 1:simulations){
  x = rlaplace(n, location = 0, scale = 4) #30 observations of Laplace(0, 4) distribution
  x.peek = x[1:20] #first 20 observations
  t.peek = mean(x.peek)/(sd(x.peek)/sqrt(n.peek)) #t statistic after peeking
  if(t.peek > crit.value.20){
    rejections = rejections + 1 #reject if T20 is greater than the critical value with df=19
  }
  else {
    t.final = mean(x)/(sd(x)/sqrt(n)) #t statistic for all 30 observations
    if(t.final > crit.value.30){
      rejections = rejections + 1 #reject if T30 is greater than the critical value with df=29
    }
  }
}
#every rejection is a type 1 error because we are assuming the null is true
(type1.error.rate = rejections / simulations)

