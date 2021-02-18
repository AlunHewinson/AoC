## Advent of Code 2017

## Day 1
library(tidyverse)
library(magrittr)
library(glue)

i1 <- readLines("2017/inputs/i1.txt", warn = FALSE) 
i1 %<>% str_split("") %>% unlist() %>% as.numeric()

## Part 1
i1a <- i1 %>% cbind(c(i1[-1], i1[1]))
i1a %<>%
  apply(1, function(q) (1-abs(sign(diff(q))))*q[2]) %>% 
  sum()
glue("The answer to Part 1 is {i1a}") %>% message()

## Part 2
l <- length(i1)
half <- seq(l/2, by=1, length.out = l) %%(l) %>% add(1)
i1b <- i1 %>% cbind(i1[half])
i1b %<>%
  apply(1, function(q) (1-abs(sign(diff(q))))*q[2]) %>% 
  sum()
glue("The answer to Part 2 is {i1b}") %>% message()
