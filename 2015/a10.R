## Advent of Code 2015

## Day 10
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i10 <- readLines("2015/inputs/i10.txt", warn = FALSE)
#i10 <- "1"

look_and_say <- function(x) {
  x %<>% str_split("") %>% unlist() %>% rle()
  paste(x$lengths, x$values, sep="") %>% paste(collapse="")
}

for (i in 1:50) {
  i10 %<>% look_and_say()
  if (i==40) glue("The answer to Part 1 is {nchar(i10)}") %>% print()
}
glue("The answer to Part 2 is {nchar(i10)}") %>% print()
