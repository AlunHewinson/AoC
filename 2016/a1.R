## Advent of Code 2016

## Day 1
library(tidyverse)
library(magrittr)
library(glue)

i1 <- readLines("2016/inputs/i1.txt", warn = FALSE)
i1 %<>% 
  str_split(", ") %>% 
  unlist()
directions <- i1 %>% str_sub(1, 1) %>% equals("R") %>% multiply_by(2) %>% 
  subtract(1) %>% cumsum() %>% mod(4)
distances <- i1 %>% str_sub(2, 100) %>% as.numeric()

## Part 1
easts <- distances * ((directions==1) - (directions==3))
norths <- distances * ((directions==0) - (directions==2))
glue("The answer to Part 1 is {abs(sum(easts))+abs(sum(norths))}") %>% message()

## Part 2
prog <- tibble(directions = mapply(FUN=rep, directions, distances) %>% unlist())
prog %<>% 
  mutate(east = cumsum(directions==1)-cumsum(directions==3)) %>% 
  mutate(north = cumsum(directions==0)-cumsum(directions==2)) %>% 
  mutate(r=row_number())
prog_rep <- prog %>% 
  group_by(east, north) %>% 
  summarise(n=n(), first_row = min(r), .groups="drop") %>% 
  filter(n > 1)
glue("The answer to Part 2 is {sum(abs(prog_rep[1, 1:2]))}") %>% message()
