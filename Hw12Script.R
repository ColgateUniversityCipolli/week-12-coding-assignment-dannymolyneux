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

#Question 2
n = 15 #sample size
sig.level = 0.05 #significance level
#function that gives Type I error rate for each type of t test for a given Beta distribution
beta.t.test = function(true.mean, alpha, beta){
  #counters for rejections for each type of t test 
  rejections.left = 0
  rejections.right = 0
  rejections.two = 0
  set.seed(7272)
  for(i in 1:simulations){
    x = rbeta(n, shape1 = alpha, shape2 = beta)
    #left tailed test for Ha: mu < true.mean
    left.t.test = t.test(x, mu = true.mean, alternative = "less")
    #right tailed test for Ha: mu > true.mean
    right.t.test = t.test(x, mu = true.mean, alternative = "greater")
    #Two tailed test for Ha: mu != true.mean
    twosided.t.test = t.test(x, mu = true.mean, alternative = "two.sided")
    
    if(left.t.test$p.value < sig.level) { #if p value for left-tailed test is less than 0.05
      rejections.left = rejections.left + 1 #reject
    }
    if(right.t.test$p.value < sig.level) { #if p value for right-tailed test is less than 0.05
      rejections.right = rejections.right + 1 #reject
    }
    if(twosided.t.test$p.value < sig.level) { #if p value for two-tailed test is less than 0.05
      rejections.two = rejections.two + 1 #reject
    }
    
  }
  return (c(left.rate = rejections.left/simulations,
            rightt.rate = rejections.right/simulations,
            twosided.rate = rejections.two/simulations))
}

(beta.10.2 = beta.t.test(10/12, 10, 2)) #.0306, 0.0790, .0596

(beta.2.10 = beta.t.test(2/12, 2, 10)) #.0790, .0306, .0596

(beta.10.10 = beta.t.test(10/20, 10, 10)) #.0490, .0503, .0498

