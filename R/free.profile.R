### Start running free in the background.
free.profile.start <- function(free.file){
  stopifnot(is.character(free.file))
  stopifnot(length(free.file) == 1)
  sh.file <- system.file("exec", "free.sh", package="testthatQuantity")
  cmd <- paste("bash", sh.file, free.file)
  system(cmd, wait=FALSE)
}

### Stop running free in the background.
free.profile.stop <- function(free.file){
  stopifnot(is.character(free.file))
  stopifnot(length(free.file) == 1)
  DONE.file <- paste0(free.file, ".DONE")
  cat("", file=DONE.file)
}

