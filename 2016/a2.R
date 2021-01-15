## Advent of Code 2016

## Day 2
library(tidyverse)
library(magrittr)
library(glue)

i2 <- readLines("2016/inputs/i2.txt", warn = FALSE)
#i2 <- c("ULL", "RRDDD", "LURDL", "UUUUD")

## Part 1
i2a <- i2 %>% str_split("")
kp <- matrix(1:9, 3) #%>% t()
p <- c(2,2)
for (i2a1 in 1:length(i2a)) {
  for (i2a2 in 1:length(i2a[[i2a1]])) {
    p[1] <- max(1, p[1]-(i2a[[i2a1]][i2a2]=="L"))
    p[1] <- min(3, p[1]+(i2a[[i2a1]][i2a2]=="R"))
    p[2] <- max(1, p[2]-(i2a[[i2a1]][i2a2]=="U"))
    p[2] <- min(3, p[2]+(i2a[[i2a1]][i2a2]=="D"))
    #cat(i2a[[i2a1]][i2a2]); cat(" "); cat(p); cat("\n")
  }
  i2a[i2a1] <- kp[p[1], p[2]]
  #print(kp[p[1], p[2]])
}
glue("The answer to Part 1 is {paste(i2a, collapse='')}") %>% message()

## Part 2
i2a <- i2 %>% str_split("")
kp <- matrix(c('','',5,'','','',2,6,'A','',1,3,7,'B','D','',4,8,'C','','','',9,'',''), 5) %>% t()
p <- c(1,3)
for (i2a1 in 1:length(i2a)) {
  for (i2a2 in 1:length(i2a[[i2a1]])) {
    putative <- p
    putative[1] <- putative[1]-as.numeric(i2a[[i2a1]][i2a2]=="L")
    putative[1] <- putative[1]+as.numeric(i2a[[i2a1]][i2a2]=="R")
    putative[2] <- putative[2]-as.numeric(i2a[[i2a1]][i2a2]=="U")
    putative[2] <- putative[2]+as.numeric(i2a[[i2a1]][i2a2]=="D")
    #print(putative)
    if (sum(abs(3-putative)) < 3) p <- putative
    #cat(i2a[[i2a1]][i2a2]); cat(" "); cat(p); cat("\n")
  }
  i2a[i2a1] <- kp[p[1], p[2]]
  #print(kp[p[1], p[2]])
}
glue("The answer to Part 2 is {paste(i2a, collapse='')}") %>% message()
