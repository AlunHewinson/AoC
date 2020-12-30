## Advent of Code 2015

## Day 3
library(magrittr)
library(tidyverse)
library(glue)

i03 <- readLines("2015/inputs/i03.txt", warn = FALSE)
i03 %<>% 
  str_split("") %>% 
  unlist()

n <- i03=="^"
e <- i03==">"
s <- i03=="v"
w <- i03=="<"

## Part 1
ns <- cumsum(n - s)
ew <- cumsum(e - w)
i03a <- cbind(ns, ew) %>% unique() %>% nrow()
glue('The answer to Part 1 is {i03a}') %>% 
  print()

## Part 2
nsm <- matrix(n - s, 2)
ewm <- matrix(e - w, 2)
i03b <- rbind(cbind(cumsum(nsm[1, ]), cumsum(ewm[1, ])), cbind(cumsum(nsm[2, ]), cumsum(ewm[2, ]))) %>% 
  unique() %>% nrow()
glue('The answer to Part 2 is {i03b}') %>% 
  print()
