## Advent of Code 2015

## Day 18
library(magrittr)
library(tidyverse)
library(glue)
library(numbers)

i18 <- 36000000

for (i in seq(801, length.out=100)) {#000) {
  cat(i); cat(" ")
  maxsig <- 10 * sapply(seq(i*1000, length.out = 1000), Sigma) %>% max()
  if (maxsig >= i18) break
}
i18a <- i*1000 + min(which(10 * sapply(seq(i*1000, length.out = 1000), Sigma) >= i18))-1
glue("The answer to Part 1 is {i18a}") %>% message()

