## Advent of Code 2015

## Day 22
library(magrittr)
library(tidyverse)
library(glue)

i22 <- readLines("2015/inputs/i22.txt", warn = FALSE)

Magic <- tribble(~Name,~Cost,~Damage,~Delay,~Longevity,~Heal,~Armour,~Mana,
                 "Magic Missile", 53, 4, 0, 1, 0, 0, 0,
                 "Drain", 73, 2, 0, 1, 2, 0, 0,
                 "Shield", 113, 0, 1, 5, 0, 7, 0,
                 "Poison", 173, 3, 1, 6, 0, 0, 0,
                 "Recharge", 229, 0, 1, 5, 0, 0, 101)



Magic
