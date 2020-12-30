## Advent of Code 2015

## Day 6
library(magrittr)
library(tidyverse)
library(glue)
library(digest)

i06 <- readLines("2015/inputs/i06.txt", warn = FALSE)

i <- matrix(0, 1000, 1000)

deluminator <- function(grid_of_horror, x_range, y_range, instruction) {
  x_range <- eval(parse(text=x_range))
  y_range <- eval(parse(text=y_range))
  if (instruction==0.5) {
    grid_of_horror[y_range, x_range] <- 1 - grid_of_horror[y_range, x_range]
    return(grid_of_horror)
  }
  grid_of_horror[y_range, x_range] <- instruction
  grid_of_horror
}

## this one is quite clunky because the logic of the instruction inputs was 
##   created prior to knowing task 2
deluminator2 <- function(grid_of_horror, x_range, y_range, instruction) {
  x_range <- eval(parse(text=x_range))
  y_range <- eval(parse(text=y_range))
  if (instruction==0.5) {
    grid_of_horror[y_range, x_range] <- grid_of_horror[y_range, x_range] + 2
    return(grid_of_horror)
  }
  grid_of_horror[y_range, x_range] <- grid_of_horror[y_range, x_range] + instruction * 2 - 1
  grid_of_horror[grid_of_horror < 0] <- 0
  grid_of_horror
}

diffindo <- function(x) {
  start <- x %>% str_extract("[0-9,]+")
  end <- x %>% str_extract("[0-9,]*$")
  instr <- x %>%
    str_replace(" [0-9].*$", "") %>%
    match(c("turn off", "toggle", "turn on")) %>% 
    `*`(0.5) %>% `-`(0.5) %>% 
    as.numeric()
  
  x_range <- paste(
    start %>% str_extract("^[0-9]+"),
    end %>% str_extract("^[0-9]+"),
    sep=":")
  y_range <- paste(
    start %>% str_extract("[0-9]+$"),
    end %>% str_extract("[0-9]+$"),
    sep=":")
  
  list(x_range, y_range, instr)
}

## Part 1
i <- matrix(0, 1000, 1000)
for (n in 1:length(i06)) {
  pieces <- diffindo(i06[n])
  i <- deluminator(i, pieces[[1]], pieces[[2]], pieces[[3]])
}
glue('The answer to Part 1 is {sum(i)}') %>% 
  print()

## Part 2
i <- matrix(0, 1000, 1000)
for (n in 1:length(i06)) {
  pieces <- diffindo(i06[n])
  i <- deluminator2(i, pieces[[1]], pieces[[2]], pieces[[3]])
}
glue('The answer to Part 2 is {sum(i)}') %>% 
  print()

## pretty!
image(i)
