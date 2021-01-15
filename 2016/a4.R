## Advent of Code 2016

## Day 4
library(tidyverse)
library(magrittr)
library(glue)
library(geocacheR)

i4 <- readLines("2016/inputs/i4.txt", warn = FALSE)

## Part 1
i4a <- i4 %>% str_split("-")
i4a_checks <- sapply(i4a, function(q) {
  q[length(q)]
})
i4a_names <- sapply(i4a, function(q) {
  q[-length(q)] %>% paste(collapse="")
})
i4a_tables <- sapply(i4a_names, function(q) {
  q %>% 
    str_split("") %>% 
    unlist() %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    head(5) %>% 
    names() %>% 
    paste(collapse="")
})
i4a_tables
reals <- str_detect(i4a_checks, i4a_tables)
glue("The answer to Part 1 is {sum(as.numeric(str_extract(i4a_checks[reals], '^[0-9]+')))}") %>% message()

## Part 2
i4b <- i4 %>% str_split("-")
i4b_rots <- sapply(i4b, function(q) {
  q[length(q)] %>% 
    str_extract("^[0-9]+") %>% 
    as.numeric()
})
i4b_names <- sapply(i4b, function(q) {
  q[-length(q)] %>% paste(collapse="-")
})
i4b_decoded <- mapply(FUN=rot, i4b_names, i4b_rots)
glue("The answer to Part 2 is {i4b_rots[i4b_decoded %>% str_detect('north') %>% which()]}") %>% message()
