## Advent of Code 2015

## Day 7
library(magrittr)
library(tidyverse)
library(glue)
library(digest)
library(future)

i07 <- readLines("2015/inputs/i07.txt", warn = FALSE)

`%NOT%` <- function(x, y) {
  x + bitwNot(y)
}
`%AND%` <- bitwAnd
`%OR%` <- bitwOr
`%RSHIFT%` <- bitwShiftR
`%LSHIFT%` <- bitwShiftL

targets <- i07 %>%
  str_extract("[a-z]+$") %>%
  paste0("0", .) %>% 
  str_extract("[0a-z]{2}$")
targets

i07 %<>%
  str_replace_all("(AND|OR|RSHIFT|LSHIFT)", "%\\1%") %>% 
  str_replace_all("NOT", "65536 %NOT%") %>% 
  str_replace_all("([a-z]{1,2})", "\\1_") ## This to kill keywords such as "in" and "
i07 <- i07[order(targets)]
i07 <- c(i07[-1], i07[1])

## Part 1
for (iii in i07) {
  eval(parse(text=iii))
}
glue('The answer to Part 1 is {a_}') %>% 
  print()

## Part 2
i07[1] <- glue("{a_} -> b_")
for (iii in i07) {
  eval(parse(text=iii))
}
glue('The answer to Part 2 is {a_}') %>% 
  print()
