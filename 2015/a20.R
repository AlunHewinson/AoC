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

## Part 2
start <- 800001
i18b <- which(
  sapply(seq(start, length.out = 100000), function(i) {
    divs <- i/(1:50)
    sum(divs[divs %% 1 == 0])*11
  }) > i18
) + start - 1

glue("The answer to Part 2 is {min(i18b)}") %>% message()
