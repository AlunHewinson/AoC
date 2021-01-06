## Advent of Code 2015

## Day 23
library(magrittr)
library(tidyverse)
library(glue)
library(R6)
library(gmp)

i23 <- readLines("2015/inputs/i23.txt", warn = FALSE)
#i23 <- c("inc a", "jio a, +2", "tpl a", "inc a")
i23 %<>%
  str_replace(" ", "(") %>% 
  paste0("self$", ., ")") %>% 
  str_replace_all("\\(([ab])", "('\\1'")
i23

Turing <- R6::R6Class("Turing", list(
  debug = TRUE,
  a = 0, #as.bigz(0),
  b = 0, #as.bigz(0),
  i = character(),
  p = 1,
  ex = function() {
    if (self$debug) cat(glue("\n--------------------\nexecute {self$p}:\n\n"))
    if (!(self$p %in% 1:length(self$i))) {
      stop(glue("pointer is {self$p} with {length(self$i)} instructions"))
    } else {
      eval(parse(text=self$i[self$p]))
    }
  },
  hlf = function(x) {
    if (self$debug) cat(glue("halve {x} to "))
    self[[x]] <- self[[x]] / 2
    if (self$debug) cat(glue("{self[[x]]}\n\n"))
    self$jmp(1)
  },
  tpl = function(x) {
    if (self$debug) cat(glue("triple {x} to "))
    self[[x]] <- self[[x]] * 3
    if (self$debug) cat(glue("{self[[x]]}\n\n"))
    self$jmp(1)
    ## tpl r sets register r to triple its current value, then continues with the next instruction.
  },
  inc = function(x) {
    if (self$debug) cat(glue("inc {x} to "))
    self[[x]] <- self[[x]] + 1
    if (self$debug) cat(glue("{self[[x]]}\n\n"))
    self$jmp(1)
    ## inc r increments register r, adding 1 to it, then continues with the next instruction.
  },
  jmp = function(n) {
    if (self$debug) cat("jump to ")
    self$p <- self$p + n
    if (self$debug) cat(glue("{self$p}\n\n"))
    ## jmp offset is a jump; it continues with the instruction offset away relative to itself.
  },
  jie = function(x, n) {
    if (self$debug) cat(glue("jump {n} if {x} is even\n\n"))
    self$jmp(c(n, 1)[1+(self[[x]] %% 2)])
    ## jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
  },
  jio = function(x, n) {
    if (self$debug) cat(glue("jump {n} if {x} is 1\n\n"))
    self$jmp(c(1, n)[1+(self[[x]] == 1)])
    ## jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
  }
))

i23a <- Turing$new()
i23a$debug <- FALSE
i23a$i <- i23

#i23a %>% print()
q <- 0
repeat {
  tryCatch(expr = {
    i23a$ex()
  }, error = function(e) q <<- 1)
  if (q==1) break
}
glue("The answer to Part 1 is {i23a$b}") %>% message()

i23b <- Turing$new()
i23b$a <- 1
i23b$debug <- FALSE
i23b$i <- i23

q <- 0
repeat {
  tryCatch(expr = {
    i23b$ex()
  }, error = function(e) q <<- 1)
  if (q==1) break
}
glue("The answer to Part 1 is {i23b$b}") %>% message()

