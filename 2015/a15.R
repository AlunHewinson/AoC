## Advent of Code 2015

## Day 15
library(magrittr)
library(tidyverse)
library(glue)
library(partitions)

i15 <- readLines("2015/inputs/i15.txt", warn = FALSE)
i15a <- i15 %>% str_extract_all("[0-9-]+") %>% 
  lapply(as.numeric) %>% 
  as.data.frame() %>% #%>% t()
  as.matrix()
colnames(i15a) <- i15 %>% str_extract("^[A-z]+")
rownames(i15a) <- c("c", "d", "f", "t", "x")

comps <- compositions(i, 4) %>%
  as.matrix()

qc <- (comps * i15a["c", ]) %>% t() %>% apply(1, sum) %>% pmax(0)
qd <- (comps * i15a["d", ]) %>% t() %>% apply(1, sum) %>% pmax(0)
qf <- (comps * i15a["f", ]) %>% t() %>% apply(1, sum) %>% pmax(0)
qt <- (comps * i15a["t", ]) %>% t() %>% apply(1, sum) %>% pmax(0)
qx <- (comps * i15a["x", ]) %>% t() %>% apply(1, sum) %>% pmax(0)

glue("The answer to Part 1 is {max(qc*qd*qf*qt)}") %>% print()
glue("The answer to Part 2 is {max(qc[qx==500]*qd[qx==500]*qf[qx==500]*qt[qx==500])}") %>% print()

