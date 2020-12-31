## Advent of Code 2015

## Day 9
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i09 <- readLines("2015/inputs/i09.txt", warn = FALSE)
i09 %<>%
  str_split("( to | = )") %>% 
  unlist() %>% 
  matrix(ncol=3, byrow=TRUE) %>% 
  as.data.frame()
names(i09) <- c("FROM", "TO", "DIST")
i09 %<>%
  rbind(i09 %>% select(FROM="TO", TO="FROM", DIST)) %>% 
  mutate(FROMTO=paste(FROM, TO)) %>%
  mutate(DIST=as.numeric(DIST)) %>% 
  arrange(FROMTO)

places <- c(i09$FROM, i09$TO) %>% unique()
routes <- permutations(n = length(places), r = length(places), v = places)
route_pairs <- sapply(1:7, function(cl) {
  paste(routes[, cl], routes[, cl+1])
})
distances <- i09$DIST[match(route_pairs, i09$FROMTO)] %>% 
  matrix(ncol=ncol(route_pairs))

dist_range <- distances %>% apply(1, sum) %>% range()
glue("The answer to Part 1 is {dist_range[1]}") %>% print()
glue("The answer to Part 1 is {dist_range[2]}") %>% print()
