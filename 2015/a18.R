## Advent of Code 2015

## Day 18
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i18 <- readLines("2015/inputs/i18.txt", warn = FALSE)
i18 %<>% 
  str_split("") %>% 
  unlist() %>% 
  match(c(".", "#")) %>% 
  subtract(1) %>% 
  matrix(100) %>% 
  t()
i18 %<>% 
  rbind(0, ., 0) %>% 
  cbind(0, ., 0)

## Part 1
i18a <- i18
q <- 0
repeat {
  i18aTEMP <- matrix(-1, 100, 100)
  for (rw in 1:100) {
    for (cl in 1:100) {
      mini <- i18a[(rw+0:2), (cl+0:2)]
      me <- mini[2,2]
      sumini <- sum(mini)-me
      i18aTEMP[rw, cl] <- sumini==3 | (sumini==2 & me)
    }
  }
  i18a <- i18aTEMP
  i18a %<>% 
    rbind(0, ., 0) %>% 
    cbind(0, ., 0)
  q <- q + 1
  cat(glue("{q} "))
  if (q >= 100) break
}
#image(i18a)
glue("\nThe answer to Part 1 is {sum(i18a)}") %>% message()

## Part 2
i18b <- i18
## 4 corners are stuck on
i18b[2,101] <- 1
i18b[101,101] <- 1
i18b[2,2] <- 1
i18b[101,2] <- 1
q <- 0
repeat {
  i18bTEMP <- matrix(-1, 100, 100)
  for (rw in 1:100) {
    for (cl in 1:100) {
      mini <- i18b[(rw+0:2), (cl+0:2)]
      me <- mini[2,2]
      sumini <- sum(mini)-me
      i18bTEMP[rw, cl] <- sumini==3 | (sumini==2 & me)
    }
  }
  i18b <- i18bTEMP
  i18b %<>% 
    rbind(0, ., 0) %>% 
    cbind(0, ., 0)
  
  ## 4 corners are stuck on
  i18b[2,101] <- 1
  i18b[101,101] <- 1
  i18b[2,2] <- 1
  i18b[101,2] <- 1
  
  q <- q + 1
  cat(glue("{q} "))
  if (q >= 100) break
}
image(i18b)
glue("\nThe answer to Part 2 is {sum(i18b)}") %>% message()


