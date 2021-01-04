## Advent of Code 2015

## Day 21
library(magrittr)
library(tidyverse)
library(glue)
library(gtools)

i21 <- readLines("2015/inputs/i21.txt", warn = FALSE)

Weapons <- tribble(~Name,~Cost,~Damage,~Armour,
"Dagger",        8,     4,       0,
"Shortsword",   10,     5,       0,
"Warhammer",    25,     6,       0,
"Longsword",    40,     7,       0,
"Greataxe",     74,     8,       0)

Armour <- tribble(~Name,~Cost,~Damage,~Armour,
"Leather",      13,     0,       1,
"Chainmail",    31,     0,       2,
"Splintmail",   53,     0,       3,
"Bandedmail",   75,     0,       4,
"Platemail",   102,     0,       5,
"None",          0,     0,       0)

Rings <- tribble(~Name,~Cost,~Damage,~Armour,
"Damage +1",    25,     1,       0,
"Damage +2",    50,     2,       0,
"Damage +3",   100,     3,       0,
"Defense +1",   20,     0,       1,
"Defense +2",   40,     0,       2,
"Defense +3",   80,     0,       3,
"None1",         0,     0,       0,
"None2",         0,     0,       0)

weapon_choices <- 1:5
armour_choices <- 1:6
ring_choices <- combinations(8, 2, c(1:8)) %>%
  as_tibble(.name_repair = "unique") %>% 
  set_colnames(c("ring1", "ring2"))
choices <- crossing(weapon=weapon_choices, armour=armour_choices, ring_choices)
choices

choices$Cost <- Weapons$Cost[choices$weapon] + Armour$Cost[choices$armour] + 
  Rings$Cost[choices$ring1] + Rings$Cost[choices$ring2]
choices$Damage <- Weapons$Damage[choices$weapon] + Armour$Damage[choices$armour] + 
  Rings$Damage[choices$ring1] + Rings$Damage[choices$ring2]
choices$Armour <- Weapons$Armour[choices$weapon] + Armour$Armour[choices$armour] + 
  Rings$Armour[choices$ring1] + Rings$Armour[choices$ring2]

choices %>% print()

## "Hit Points: 109" "Damage: 8"       "Armor: 2"
i21 %<>%
  str_split(": ") %>% 
  unlist() %>% 
  matrix(2) %>% 
  t()
i21 <- i21[, 2] %>% as.numeric()
i21

i21a <- choices %>% 
  mutate(boss_pain = pmax(1, Damage-i21[3])) %>% 
  mutate(play_pain = pmax(1, i21[2]-Armour)) %>% 
  mutate(boss_dies = ceiling(i21[1]/boss_pain)) %>% 
  mutate(play_dies = ceiling(100/play_pain)) %>% 
  filter(play_dies >= boss_dies) %>% 
  select_all()
glue("The answer to Part 1 is {min(i21a$Cost)}") %>% message()

i21b <- choices %>% 
  mutate(boss_pain = pmax(1, Damage-i21[3])) %>% 
  mutate(play_pain = pmax(1, i21[2]-Armour)) %>% 
  mutate(boss_dies = ceiling(i21[1]/boss_pain)) %>% 
  mutate(play_dies = ceiling(100/play_pain)) %>% 
  filter(play_dies < boss_dies) %>% 
  select_all()
glue("The answer to Part 2 is {max(i21b$Cost)}") %>% message()
