## Advent of Code 2015

## Day 8
library(magrittr)
library(tidyverse)
library(glue)

i08 <- readLines("2015/inputs/i08.txt", warn = FALSE)

## Part 1
i08a <- i08 %>%
  str_replace_all(fixed("\\"), "/") %>% 
  str_replace_all('"', "'") %>% 
  str_replace_all("/'", "'") %>% 
  str_replace_all("/(/|x[a-f0-9]{2})", "*") %>% 
  str_replace_all("(^')|('$)", "")
glue("The answer to Part 1 is {sum(nchar(i08)) - sum(nchar(i08a))}") %>% print()

## Part 2
i08b <- i08 %>%
  str_replace_all(fixed("\\"), "//") %>% 
  str_replace_all('"', "''")
i08b %>% nchar() %>% add(2)
glue("The answer to Part 2 is {sum(2+nchar(i08b)) - sum(nchar(i08))}") %>% print()
