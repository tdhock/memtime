mem.time <- function
### Compute time and memory usage of R code.
(expr,
### R code.
 gcFirst = TRUE,
### Run gc before computing initial time/memory values?
 free.file=tempfile()
 ){
  ppt <- function(y) {
    if (!is.na(y[4L])) 
      y[1L] <- y[1L] + y[4L]
    if (!is.na(y[5L])) 
      y[2L] <- y[2L] + y[5L]
    y[1L:3L]
  }
  if (!exists("proc.time")) 
    return(rep(NA_real_, 5L))
  if (gcFirst) 
    gc(FALSE)
  time <- proc.time()
  free.profile.start(free.file)
  on.exit(cat("Timing stopped at:", ppt(proc.time() - time), 
              "\n"))
  expr
  new.time <- proc.time()
  free.profile.stop(free.file)
  browser()
  on.exit()
  list(time=structure(new.time - time, class = "proc_time"),
       memory=NULL)
### List of time and memory usage.
}

