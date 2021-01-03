## Advent of Code 2015

## Day 19
library(magrittr)
library(tidyverse)
library(glue)

i19 <- readLines("2015/inputs/i19.txt", warn = FALSE)
i19 <- i19[i19 != ""] 

replacements <- i19[str_detect(i19, " => ")] %>% 
  str_split(" => ")
target <- i19[!str_detect(i19, " => ")]
medicine <- target %>% 
  str_replace_all("([A-Z])", " \\1") %>% 
  trimws() %>% 
  str_split(" ") %>% 
  unlist()

argets <- target %>%
  str_split("Ar") %>%
  unlist() %>% 
  paste0("Ar")
argets
argets[1]

#for (replacement in replacements[1:2]) {
i19a <- sapply(replacements, function(replacement) {
  mols <- which(medicine==replacement[1]) #str_locate_all(medicine, replacement[1])# %>% length()
  #print(mols)
  sapply(mols, function(mol) {
    #print(replacement)
    #print(mol)
    medicine[mol] <- replacement[2]
    medicine %>% paste(collapse="")
  })
}) %>% 
  unlist() %>% 
  as.vector() %>% 
  unique()
glue("The answer to Part 1 is {length(i19a)}") %>% message()

made <- tibble(med="e", steps=0)
q <- 0
repeat {
  new_made <- made %>% 
    filter(steps==q) %>% 
    apply(1, function(med) {
      medicine <- med[1] %>% 
        str_replace_all("([A-Z])", " \\1") %>% 
        trimws() %>% 
        str_split(" ") %>% 
        unlist()
      
      new_meds <- sapply(replacements, function(replacement) {
        mols <- which(medicine==replacement[1])
        sapply(mols, function(mol) {
          medicine[mol] <- replacement[2]
          medicine %>% paste(collapse="")
        })
      }) %>% 
        unlist() %>% 
        as.vector() %>% 
        unique()
      tibble(med=new_meds, steps=q+1)
    }) %>% 
    bind_rows()
  #made <- bind_rows(made, new_made) %>% 
  #  group_by(med) %>% 
  #  summarise(steps=min(steps), .groups="drop")
  made <- new_made

  q <- q + 1
  cat(q)
  cat(" ")

  #if (q > 3) {  
  #platelet <- max(0, q-3)
  #platelet <- round(min(nchar(made$med))-2)
  platelet1 <- switch(q, 0, 0, 2, 4, 7, 9, 11, 13, 17, 19, 19, 19)
  if (!is.null(platelet1)) platelet <- platelet1
  print(platelet)
  made %<>% 
    filter(str_sub(made$med, 1, platelet)==str_sub(target, 1, platelet))
  print(nrow(made))
  #}
  
  #made %>% filter(nchar(med) <= 488)
  if (any(made$med==target)) {
  #if (any(str_sub(made$med, 1, nchar(argets[1]))==argets[1])) {
  #if (any(str_sub(made$med, 1, platelet)==str_sub(target, 1, platelet))) {
      #message(glue("The answer to Part 2 is {made %>% filter(med==target) %>% select(steps))}"))
    break
  }
  if (q >= 12) stop("too many!")
}
table(nchar(made$med))
made
