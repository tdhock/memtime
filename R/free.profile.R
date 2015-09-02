free.profile.start <- function
### Start running free in the background.
(free.file
### File to which we will write output of free.
 ){
  stopifnot(is.character(free.file))
  stopifnot(length(free.file) == 1)
  sh.file <- system.file("exec", "free.sh", package="memtime")
  cmd <- paste("bash", sh.file, free.file)
  system(cmd, wait=FALSE)
### Nothing.
}

free.profile.stop <- function
### Stop running free in the background.
(free.file
### file where free output is writing.
 ){
  stopifnot(is.character(free.file))
  stopifnot(length(free.file) == 1)
  DONE.file <- paste0(free.file, ".DONE")
  cat("", file=DONE.file)
  readLines(free.file)
### character vector of lines written to free.file.
}

