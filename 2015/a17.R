## Advent of Code 2015

## Day 17
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i17 <- readLines("2015/inputs/i17.txt", warn = FALSE)
i17 %<>% as.numeric()

c150 <- sapply(4:11, function(buckets) {
  ## by inspection, three buckets are never enough and 12 always too much
  capacities <- combinations(20, buckets) %>% 
    apply(1, function(choices) {
      i17[choices] %>% sum()
    })
  toret <- (capacities==150) %>% sum()
  if (buckets==4) {
    glue("The answer to Part 2 is {toret}") %>% message()
  }
  toret
}) %>% sum()
glue("The answer to Part 1 is {c150}") %>% message()
