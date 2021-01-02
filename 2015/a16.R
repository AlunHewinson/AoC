## Advent of Code 2015

## Day 16
library(magrittr)
library(tidyverse)
library(glue)

i16 <- readLines("2015/inputs/i16.txt", warn = FALSE)
i16 %<>% 
  lapply(function(q) {
    Sue <- q %>% str_extract("[0-9]+")
    memories <- q %>%
      str_extract_all("[a-z]+: [0-9]+") %>%
      unlist() %>% 
      str_split(": ") %>% 
      unlist() %>% 
      matrix(2) %>% 
      t() %>% 
      as.data.frame() %>% 
      set_colnames(c("compound", "n")) %>%
      as_tibble() %>% 
      mutate(Sue=Sue)
    memories
  }) %>% 
  bind_rows()

ticker <- tribble(~compound, ~detected,
                  "children", 3,
                  "cats", 7,
                  "samoyeds", 2,
                  "pomeranians", 3,
                  "akitas", 0,
                  "vizslas", 0,
                  "goldfish", 5,
                  "trees", 3,
                  "cars", 2,
                  "perfumes", 1)

## Part 1
i16a <- i16 %>% left_join(ticker)
i16a %<>% mutate(fit = n==detected)
i16a %<>% 
  group_by(Sue) %>% 
  summarise(allfit=all(fit)) %>% 
  filter(allfit==TRUE)
glue("The answer to Part 1 is {i16a$Sue}") %>% message()

## Part 2
i16b <- i16 %>% left_join(ticker)
operators <- tibble(compound = unique(i16b$compound), operator = "==")
operators[operators$compound %in% c("cats", "trees"), "operator"] <- ">"
operators[operators$compound %in% c("pomeranians", "goldfish"), "operator"] <- "<"
i16b %<>% 
  left_join(operators) %>% 
  mutate(toeval = paste(n, operator, detected)) %>% 
  mutate(fit = sapply(toeval, function(q) eval(parse(text = q))))
i16b %<>% 
  group_by(Sue) %>% 
  summarise(allfit=all(fit)) %>% 
  filter(allfit==TRUE)
glue("The answer to Part 2 is {i16b$Sue}") %>% message()
