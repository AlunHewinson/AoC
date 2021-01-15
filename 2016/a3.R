## Advent of Code 2016

## Day 3
library(tidyverse)
library(magrittr)
library(glue)

i3 <- readLines("2016/inputs/i3.txt", warn = FALSE)
i3 %<>% str_split(" ") %>% unlist()
i3 <- i3[i3 != ""] %>% as.numeric()

## Part 1
i3a <- i3 %>% matrix(3)
i3a %<>% apply(2, function(q) {
  q %<>% sort()
  sum(q[1:2]) > q[3]
})
glue("The answer to Part 1 is {sum(i3a)}") %>% message()

## Part 1
i3b <- i3 %>% matrix(3) %>% t() %>% matrix(3)
i3b %<>% apply(2, function(q) {
  q %<>% sort()
  sum(q[1:2]) > q[3]
})
glue("The answer to Part 2 is {sum(i3b)}") %>% message()
