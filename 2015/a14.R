## Advent of Code 2015

## Day 14
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i14 <- readLines("2015/inputs/i14.txt", warn = FALSE)
i14 %<>%
  str_split("( can fly | km/s for | seconds,.*for )") %>% 
  unlist() %>%
  str_replace_all("gain ", "+") %>% 
  str_replace_all("lose ", "-") %>% 
  str_replace_all(" seconds.", "") %>% 
  matrix(ncol = 4, byrow=TRUE) %>% 
  as.data.frame() %>% as_tibble() %>% 
  rename(rein="V1", speed="V2", dure="V3", rest="V4") %>% 
  mutate(speed=as.numeric(speed), dure=as.numeric(dure), rest=as.numeric(rest))

race <- 2503
i14a <- i14 %>% 
  mutate(period = dure+rest) %>% 
  mutate(full_cycles = race %/% period) %>% 
  mutate(rem = race %% period) %>% 
  mutate(dist = speed * full_cycles * dure + pmin(dure, rem) * speed)
glue("The answer to Part 1 is {max(i14a$dist)}") %>% print()

i14b <- rep(0, 9)
names(i14b) <- i14$rein
for (tid in 1:race) {
  leading <- i14 %>% 
    mutate(period = dure+rest) %>% 
    mutate(full_cycles = tid %/% period) %>% 
    mutate(rem = tid %% period) %>% 
    mutate(dist = speed * full_cycles * dure + pmin(dure, rem) * speed) %>% 
    filter(dist==max(dist)) %>% select(rein) %>% unlist()
  i14b[leading] <- i14b[leading] + 1
}
glue("The answer to Part 2 is {max(i14b)}") %>% print()
