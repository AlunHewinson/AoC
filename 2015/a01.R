## Advent of Code 2015

## Day 1
library(magrittr)
library(tidyverse)
library(glue)

i01 <- readLines("2015/inputs/i01.txt", warn = FALSE)
i01 %<>% 
  str_split("") %>% 
  unlist()

## part 1
i01a <- i01 %>% table()
print(glue('The answer to Part 1 is {i01a["("]-i01a[")"]}'))

## part 2
i01b <- i01 %>% 
  str_replace_all("\\(", "1") %>% 
  str_replace_all("\\)", "-1") %>% 
  as.numeric()
i01b %<>%
  cumsum() %>% 
  `<`(0) %>% 
  which() %>% 
  `[`(1)
glue('The answer to Part 2 is {i01b}') %>% 
  print()
