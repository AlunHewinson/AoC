## Advent of Code 2015

## Day 24
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)
library(FLSSS)

i24 <- readLines("2015/inputs/i24.txt", warn = FALSE) %>% 
  as.numeric()

## Part 1
i24a <- FLSSS(len = 6, v = i24, target=sum(i24)/3, ME=0.5, solutionNeed = 1000)
i24a %<>% sapply(function(q) {
  i24[q] %>% prod()
}) %>% min()
glue("The answer to Part 1 is {i24a}") %>% message()

## Part 2

getFLSSS <- function(len, v=i24, target=390, ME=0.5, solutionNeed=1000) {
  solutions <- FLSSS(len=len, v=v, target=target, ME=ME, solutionNeed=solutionNeed)
  remainder <- lapply(solutions, function(q) setdiff((1:length(v)), q))
  #print(remainder)
  mapply(FUN=function(solutions, remainder) {
    list(solutions=v[solutions], remainder=v[remainder])
  }, solutions=solutions, remainder=remainder, SIMPLIFY = FALSE)
}

for (b1 in 2:7) {
  print(glue("b1 = {b1}"))
  s1s <- getFLSSS(b1, v=i24)
  s1s <- s1s[order(sapply(s1s, function(q) prod(q$solutions)))]
  if (length(s1s) > 0) {
    for (s1 in s1s) {
      for (b2 in 2:10) {
        print(glue("b1 = {b1}   b2 = {b2}"))
        #print(s1$remainder)
        s2s <- getFLSSS(b2, v=s1$remainder)
        if (length(s2s) > 0) {
          for (s2 in s2s) {
            for (b3 in 2:10) {
              print(glue("b1 = {b1}   b2 = {b2}   b3 = {b3}"))
              s3s <- getFLSSS(b3, v=s2$remainder)
              if (length(s3s) > 0) {
                print(s1$solutions)
                print(s2$solutions)
                stop(glue("The answer to Part 2 is {prod(s1$solutions)}"))
                ## if we've reached this point, all is good methinks
              }
            }
          }
        }
      }
    }
  }
}
stop("no solution reached")
