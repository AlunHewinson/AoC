## Advent of Code 2017

## Day 2
library(tidyverse)
library(magrittr)
library(glue)
library(combinat)

i2 <- readLines("2017/inputs/i2.txt", warn = FALSE) 

## Part 1
i2a <- i2 %>% str_split("\t") %>% unlist() %>% 
  as.numeric() %>% 
  matrix(ncol=length(i2)) %>% t()
i2a %<>% 
  apply(1, function(q) max(q)-min(q)) %>% 
  sum()
glue("The answer to Part 1 is {i2a}") %>% message()

## Part 2
i2b <- i2 %>% str_split("\t") %>% unlist() %>% 
  as.numeric() %>% 
  matrix(ncol=length(i2)) %>% t()
i2b %<>% apply(1, function(q) {
  divs <- combn(q, 2) %>% 
    apply(2, function(qq) max(qq)/min(qq))
  #print(divs %% 1)
  #print(divs %% 1 == 0)
  divs[divs %% 1 == 0]
}) %>% sum()
glue("The answer to Part 1 is {i2b}") %>% message()
