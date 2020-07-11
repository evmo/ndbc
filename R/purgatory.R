cleanEnv <- function() {
  rm(list=ls(pattern = "b.{5}_\\d{4}", pos=".GlobalEnv"), envir=.GlobalEnv)
}
