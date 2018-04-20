library(tidyverse)
# Q1
# a
score = sample(1:100,size = 40)
score_error = rnorm(n= 40,mean =  0, 2)
score = score + score_error
for(i in 1:40){
  if(score[i] >100) score[i] = 100
  else if(score[i]<0) score[i] = 0
}

score
# b
score_group = function(x){
  if(x>79){
    return("A")
  } else if(x<80 && x>59){
    return("B")
  }else if(x<60 && x>39){
    return("C")
  }else{
    return("D")
  }
  
}

score_g = c()
for(i in 1:length(score)){score_g[i] = score_group(score[i])}
score_g

# c
cl_mean = function(data){
  n = length(data)
  mean = mean(data)

  sd = sd(data)
  Z = qnorm(0.025,lower.tail = F)
  
  return(c(upper=mean + Z*sd/sqrt(n), lower=mean - Z*sd/sqrt(n), mean= mean))
}


score = as.vector(score)
cl_mean(score)


help("qnorm")




# Q2
# a
x = c(-13.87,-2.53,-2.44,-2.40,-1.75,-1.34,-1.05,-0.23,-0.07,0.27,1.77,2.76,3.29,3.47,3.71,3.80,4.24,4.53,43.21,56.75)

l = function(theta){
  n = length(x)
  result = -n*log(pi)
  for(i in 1:n){
    result = result - log(1+(theta-x[i])^2)
    
  }
  return(result)
} 
plot(l,xlim=c(-50,50))

# b
n = length(x)
l1 = function(theta = 0){
  result=0
  for (i in 1:n){
    
    result = result + (theta-x[i])/(1+(theta-x[i])^2)
  }
  return(-2*result)}



l2 = function(theta = 0){
  result=0
  for (i in 1:n){
    b = 1 - (theta-x[i])^2
    a = (1+(theta-x[i])^2)^2
    result = result + b/a
  }
  return(-2*result)}

MLE_recursion = function(theta, error = 0.00001){
  print(1)
  if(abs((l1(theta)/l2(theta))/(theta+0.05))<error) return(theta)
  else return(MLE_recursion(theta-(l1(theta)/l2(theta))))
}
MLE_recursion()


MLE_iteration = function(theta, error = 0.00001){
  e = abs((l1(theta)/l2(theta))/(theta+0.05))
  while(e>error){
    theta = theta-(l1(theta)/l2(theta))
    e = abs((l1(theta)/l2(theta))/(theta+0.05)) 
  }
  return(theta)
}
MLE_iteration(1)

ini = c(-1, 0, 1.4, 4.1, 7, 38)
result = c()
length(ini)
for (i in 1:length(ini)) {
  print(ini[i])  
  result[i] = MLE_iteration(theta=ini[i])
  
}
result

#疊代版本可符合要求順利運作，遞迴版本有bug還望能跟老師討論