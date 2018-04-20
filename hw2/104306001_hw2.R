#Q1

#a.產生學生成績
score_mean <- sample(0:100, 40)
score_error <- rnorm(40, 0, 2) #n=40, mean=0, variance=2
score = score_mean + score_error

#處理score[i]<0和score[i]>100 
for(i in c(1:40)){
  ifelse(score[i] > 100 , score[i] <- 100,
         ifelse(score[i] < 0, score[i] <- 0, score[i] <- score [i]))
}

#b.依照成績分成ABCD四組
score_group <- function(x){
  group.id <- ifelse(x >= 80, "A",
                     ifelse(x < 80 & x >= 60, "B",
                            ifelse(x < 60 & x >= 40, "C", "D")))
  data.frame(score = x, group.id, row.names = NULL)
}
score_group(score)

#c.計算95%信賴區間和平均數
CI_Mean <- function(x){
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  z <- qnorm(0.025,lower.tail = F) #z-value
  ci <- c()
  ci[1] <- mean - z*sd/sqrt(n) #下限
  ci[2] <- mean + z*sd/sqrt(n) #上限
  return (c("95%CI:", ci[1], "~", ci[2],
            "mean:", mean))
}
CI_Mean(score)

#Q2

#a
x <- c(-13.87,-2.53,-2.44,-2.40,-1.75,-1.34,-1.05,-0.23,-0.07,0.27,1.77,
       2.76,3.29,3.47,3.71,3.80,4.24,4.53,43.21,56.75)
l <- function(theta){
  n <- length(x)
  result <- 0
  for(i in 1:n){
    result <- result + log(1 + (theta - x[i])^2)
  }
  result <- result*(-1) - n*log(pi)
  return (result)
}
plot(l,xlim=c(-50,50))

#b
l1 <- function(theta){
  m=0
  for (i in 1:length(x)){
    m <- m + (theta - x[i])/(1 + (theta - x[i])^2)
  }
  m <- m*(-2)
  return(m)
}

l2 <- function(theta){
  m=0
  for (i in 1:length(x)){
    m <- m + (1 - (theta - x[i])^2)/((1 + (theta - x[i])^2)^2)
  }
  m <- m*(-2)
  return(m)
}

#the function of N-R method
NR <- function(theta){ -l1(theta)/l2(theta) }

t0 <- matrix(c(-1, 0, 1.4, 4.1, 7, 38),6,1) # initial theta values
t1 <- matrix(0,6,1) #save results

for(i in 1:6){
  t1[i,1] <- t0[i,1] + NR(t0[i,1])
  c <- abs( (t1[i,1] - t0[i,1]) / (t0[i,1] + 0.05) ) #criticl value
  t <- 0
  while (c > 0.00001){  
    t <- t+1
    t0[i,1] <- t1[i,1]
    t1[i,1] <- t0[i,1] + NR(t0[i,1])
    c <- abs( (t1[i,1] - t0[i,1]) / (t0[i,1] + 0.05) )
  }
}
t1