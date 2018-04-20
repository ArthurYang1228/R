library(tidyverse)

data = read.csv(file="C:\Users\sunlite\Desktop\106¤U½Òµ{\R\hw1\airline_delay.csv")
summary(data)


location = c("JFK","LAX","SFO","SMF")
carrier = c("AA","B6","DL","OO","UA","VX","WN")

# how much in every line totally
line_num = data.frame()
line_num

for (i in location) {
  
  for (l in location) {
    line_num[i,l] = data %>%
      filter(ORIGIN == i & DEST == l) %>%
       tally()
    
    
  }
}

line_num



#how much were cancelled in every line
line_cancel = data.frame()
line_cancel

for (i in location) {
  
  for (l in location) {
    line_cancel[i,l] = data %>%
      filter(ORIGIN == i & DEST == l & CANCELLED == 1) %>%
      tally()
    
    
  }
}

line_cancel

# ratio of total number and cancel number
line_cancel_ratio = data.frame()
for (i in location) {
  
  for (l in location) {
    line_cancel_ratio[i,l] =  line_cancel[i,l]/ line_num[i,l]
      
    
    
  }
}
line_cancel

# expection value for every line
exp = line_cancel_ratio*5000
exp

# how much  flight for  every carrier totally
carrier_num = data.frame(row.names = 1)
carrier_num
for (c in carrier){
  carrier_num[c] = data %>%
    filter(CARRIER == c) %>%
    tally()
}
carrier_num

# how much  flight be cancelled for  every carrier 
carrier_cancel_num = data.frame(row.names = 1)
carrier_cancel_num
for (c in carrier){
  carrier_cancel_num[c] = data %>%
    filter(CARRIER == c & CANCELLED ==1 ) %>%
    tally()
}
carrier_cance

# how much ratuo  flight be cancelled for  every carrier 
carrier_cancel_ratio = data.frame(row.names = 1)
carrier_cancel_ratio
for (c in carrier){
  carrier_cancel_ratio[c] =  carrier_cancel_num[c]/carrier_num[c]
}
carrier_cancel_ratio*5000



#make barplot
forPhoto = carrier_cancel_num
forPhoto
barplot(as.matrix(forPhoto),
        col = "blue")


help("barplot")


