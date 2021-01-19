## Advent of Code 2016

## Day 9
library(tidyverse)
library(magrittr)
library(glue)

i9 <- readLines("2016/inputs/i9.txt", warn = FALSE)
#i9 <- "ADVENT"
#i9 <- "A(1x5)BC"
#i9 <- "(3x3)XYZ"
#i9 <- "A(2x2)BCD(2x2)EFG"
#i9 <- "(6x1)(1x3)A"
#i9 <- "X(8x2)(3x3)ABCY"

## Part 1
#initial <- str_extract(i9, "[^(]*")
#naeste <- str_extract(i9, "\\([0-9]+x[0-9]+\\).*")

mess <- i9
i9a <- character()

repeat {
  ## anything before a bracket, just chuck it across
  i9a <- c(i9a, str_extract(mess, "[^\\(]*"))
  mess %<>% str_remove("^[^\\(]*")
  
  ## get out if there's nothing left
  if (nchar(mess) < 1) break
  
  ## find the scoop and gange
  scoop <- str_extract(mess, "[0-9]+") %>% as.numeric()
  gange <- str_extract(mess, "(?<=x)[0-9]+") %>% as.numeric()
  
  ## destroy the bracket
  mess %<>% str_remove("^[^\\)]*\\)")
  
  ## find the article and gange it, chuck that on the pile
  article <- str_sub(mess, 1, scoop)
  i9a <- c(i9a, article %>% rep(gange) %>% paste(collapse=""))

  ## destroy the article
  mess %<>% str_remove(fixed(article))
}
glue("The answer to Part 1 is {sum(nchar(i9a))}") %>% message()
