## Advent of Code 2015

## Day 4
library(magrittr)
library(tidyverse)
library(glue)
library(digest)

i04 <- readLines("2015/inputs/i04.txt", warn = FALSE)

## Part 1
i <- 0
repeat {
  i <- i+1
  #for (i in 609000:609100) {
  hash <- paste0(i04, i) %>% 
    digest(serialize = FALSE) %>% 
    str_sub(1, 5)
  if (hash=="00000") break
}
glue('The answer to Part 1 is {i}') %>% 
  print()

## Part 2
i <- i-1
repeat {
  i <- i+1
  hash <- paste0(i04, i) %>% 
    digest(serialize = FALSE) %>% 
    str_sub(1, 6)
  if (hash=="000000") break
}
glue('The answer to Part 2 is {i}') %>% 
  print()
