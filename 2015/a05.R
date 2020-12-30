## Advent of Code 2015

## Day 5
library(magrittr)
library(tidyverse)
library(glue)
library(digest)

i05 <- readLines("2015/inputs/i05.txt", warn = FALSE)

## Part 1
i05a1 <- i05 %>% str_detect("[aeiou].*[aeiou].*[aeiou]")
i05a2 <- i05 %>% str_detect("(.{1})\\1")
i05a3 <- i05 %>% str_detect("ab|cd|pq|xy") %>% not()
glue('The answer to Part 1 is {sum(i05a1 & i05a2 & i05a3)}') %>% 
  print()

## Part 2
i05b1 <- i05 %>% str_detect("(.{2}).*\\1")
i05b2 <- i05 %>% str_detect("(.{1}).{1}\\1")
glue('The answer to Part 2 is {sum(i05b1 & i05b2)}') %>% 
  print()