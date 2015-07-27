execscript <- function(text){

  writeLines(text, con="input.R")
  source('input.R', local = TRUE)
}

execscript2 <- function(text){

  writeLines(text, con="input.R")
  source('input.R', local = TRUE)
}
