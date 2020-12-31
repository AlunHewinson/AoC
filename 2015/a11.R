## Advent of Code 2015

## Day 11
library(magrittr)
library(tidyverse)
library(glue)

i11 <- readLines("2015/inputs/i11.txt", warn = FALSE)
#i11 <- "abcdefgh"

test_pw <- function(x) {
  ## Passwords may not contain the letters i, o, or l, as these letters can be
  ##   mistaken for other characters and are therefore confusing.
  if (x %>% str_detect("[iol]")) return(FALSE)

  ## Passwords must contain at least two different, non-overlapping pairs of
  ##   letters, like aa, bb, or zz.
  if (!x %>% str_detect("([a-z])\\1.*([a-z])\\2")) return(FALSE)

  ## Passwords must include one increasing straight of at least three letters,
  ##   like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; 
  ##   abd doesn't count.
  
  str_detect(x, c("abc", "bcd", "cde", "def", "efg", "fgh", "pqr", "qrs",
                  "rst", "stu", "tuv", "uvw", "vwx", "wxy", "xyz")) %>% any()
}
test_pw("hijklmmn")
test_pw("abbceffg")
test_pw("abbcegjk")
test_pw("abcdffaa")
test_pw("ghjaabcc")

increment_pw <- function(x) {
  left <- x %>% str_sub(1, -2)
  right <- x %>% str_sub(-1, -1)
  if (right=="z") return(paste0(increment_pw(left), "a"))
  #print(letters[match(right, letters)+1])
  #stop(98)
  paste0(left, letters[-c(9,12,15)][match(right, letters[-c(9,12,15)])+1])
}

repeat { #for (i in 1:20000) {
  i11 %<>% increment_pw()
  if (test_pw(i11)) break
}
glue("The answer to Part 1 is {i11}") %>% print()

repeat {
  i11 %<>% increment_pw()
  if (test_pw(i11)) break
}
glue("The answer to Part 2 is {i11}") %>% print()


