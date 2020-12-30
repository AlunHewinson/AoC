## Advent of Code 2015

## Day 2
library(magrittr)
library(tidyverse)
library(glue)

i02 <- readLines("2015/inputs/i02.txt", warn = FALSE)
i02 %<>% 
  str_split("x") %>% 
  lapply(as.numeric)

i02a <- i02 %>% 
  sapply(function(q) {
    sum(prod(q)*2/q) + min(prod(q)/q)
  }) %>% 
  sum()
glue('The answer to Part 1 is {i02a}') %>% 
  print()

i02b <- i02 %>% sapply(function(q) {
  prod(q) + 2*(sum(q)-max(q))
}) %>%
  sum()
glue('The answer to Part 2 is {i02b}') %>% 
  print()