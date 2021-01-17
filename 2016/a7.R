## Advent of Code 2016

## Day 7
library(tidyverse)
library(magrittr)
library(glue)

i7 <- readLines("2016/inputs/i7.txt", warn = FALSE)

## Part 1
inbracks <- i7 %>%
  str_detect("\\[[^\\]]*([a-z])(?!\\1)([a-z])\\2\\1[^\\]]*\\]")
alldoubs <- i7 %>%
  str_detect("([a-z])(?!\\1)([a-z])\\2\\1")
i7a <- alldoubs & !inbracks
glue("The answer to Part 1 is {sum(i7a)}") %>% message()

## Part 2
text_in_bracks <- i7 %>%
  str_extract_all("\\[[^\\]]*?\\]") %>% 
  sapply(paste, collapse="") %>% 
  str_replace_all("[\\[\\]]", "_")
text_not_in_backs <- i7 %>% 
  str_replace_all("\\[[^\\]]*?\\]", "__")
retext <- paste(text_not_in_backs, text_in_bracks)
ababab <- retext %>% str_detect("([a-z])(?!\\1)([a-z])\\1[^ ]* .*\\2\\1\\2")
glue("The answer to Part 2 is {sum(ababab)}") %>% message()
