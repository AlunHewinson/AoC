## Advent of Code 2015

## Day 25
library(magrittr)
library(tidyverse)
library(glue)
library(gmp)

i25 <- readLines("2015/inputs/i25.txt", warn = FALSE)
i25 %<>% str_extract_all("[0-9]+") %>% unlist() %>% as.numeric()

i <- as.bigz("20151125")

## test
#i25 <- c(2,6)
## seeking cell (3010, 3019)
## first, find cell (3010+3019-1, 1)
col1 <- 1:(sum(i25)-2) %>% sum()
## then add 3019-1 to it
nth <- col1+i25[2]-2

i25a <- mod.bigz(i * gmp::powm(252533, nth+1, 33554393), 33554393)
glue("The answer to Part 1 is {i25a}") %>% message()

message("Part 2 requires the solving of all 49 previous puzzles in AoC2015!")
