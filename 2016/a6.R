## Advent of Code 2016

## Day 6
library(tidyverse)
library(magrittr)
library(glue)

i6 <- readLines("2016/inputs/i6.txt", warn = FALSE)
i6a <- i6 %>% str_split("") %>% unlist() %>% matrix(8) %>% 
  apply(1, function(q) q %>% table() %>% sort(decreasing = TRUE) %>% names() %>% `[`(1)) %>% 
  paste(collapse="")
glue("The answer to Part 1 is {i6a}") %>% message()

i6b <- i6 %>% str_split("") %>% unlist() %>% matrix(8) %>% 
  apply(1, function(q) q %>% table() %>% sort() %>% names() %>% `[`(1)) %>% 
  paste(collapse="")
glue("The answer to Part 2 is {i6b}") %>% message()
