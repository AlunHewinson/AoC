## Advent of Code 2015

## Day 13
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i13 <- readLines("2015/inputs/i13.txt", warn = FALSE)
i13 %<>%
  str_split("( would|happ.*xt to )") %>% 
  unlist() %>%
  str_replace_all("gain ", "+") %>% 
  str_replace_all("lose ", "-") %>% 
  matrix(ncol = 3, byrow=TRUE) %>% 
  as.data.frame() %>% as_tibble() %>% 
  rename(subject = "V1", gain = "V2", object = "V3") %>% 
  mutate(gain = as.numeric(gain)) %>% 
  mutate(object = str_replace_all(object, "\\.$", ""))
i13 %<>%
  left_join(i13 %>% rename(subject="object", object="subject", give="gain")) %>% 
  mutate(pair = paste(subject, object)) %>% 
  mutate(happy=gain+give)

guests <- c(i13$subject) %>% unique()
arranges <- permutations(n = length(guests), r = length(guests), v = guests)

## Part 1
arrange_pairs_nohost <- sapply(1:8, function(cl) {
  paste(arranges[, cl], arranges[, (cl%%8)+1])
})
happies_nohost <- i13$happy[match(arrange_pairs_nohost, i13$pair)] %>% 
  matrix(ncol=ncol(arrange_pairs_nohost))
glue("The answer to Part 1 is {happies_nohost[happies_nohost %>% apply(1, sum) %>% which.max(), ] %>% sum()}") %>% print()

## Part 2
arrange_pairs_host <- sapply(1:7, function(cl) {
  paste(arranges[, cl], arranges[, cl+1])
})
happies_host <- i13$happy[match(arrange_pairs_host, i13$pair)] %>% 
  matrix(ncol=ncol(arrange_pairs_host))
glue("The answer to Part 2 is {happies_host[happies_host %>% apply(1, sum) %>% which.max(), ] %>% sum()}") %>% print()
