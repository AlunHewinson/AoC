## Advent of Code 2016

## Day 5
library(tidyverse)
library(magrittr)
library(glue)
library(digest)

i5 <- readLines("2016/inputs/i5.txt", warn = FALSE)
#i5 <- "abc"

base <- 24600000
finds <- 0
repeat {
  i5a <- paste0(i5, base + 0:999)
  i5a %<>% sapply(digest, serialize=FALSE)
  fiveohs <- i5a %>% str_detect("^00000")
  if (any(fiveohs)) {
    print(i5a[fiveohs])
    finds <- finds + sum(fiveohs)
  }
  if (finds >= 8) break
  base <- base + 1000
}

## This one's still a bit manual, just running it in blocks of 8 until you get the answers you need.
