memtime <- function
### Compute time and memory usage of R code.
(expr,
### R code.
 gcFirst = TRUE,
### Run gc before computing initial time/memory values?
 free.file=tempfile(),
### file where free output will be written.
 sleep.seconds=0.1
### Seconds to wait before/after calling free.profile.start/stop.
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
  free.profile.start(free.file)
  Sys.sleep(sleep.seconds)
  time <- proc.time()
  on.exit(cat("Timing stopped at:", ppt(proc.time() - time), 
              "\n"))
  expr
  new.time <- proc.time()
  Sys.sleep(sleep.seconds)
  free.lines <- free.profile.stop(free.file)
  firstValue <- function(txt){
    value <- sub(".*? ([0-9]+) .*", "\\1", txt)
    as.numeric(value)
  }
  used.lines <- grep("-/+", free.lines, value=TRUE)
  used.values <- firstValue(used.lines)
  first <- used.values[1]
  max.mem <- max(used.values)
  megabytes <- 
    c(first=first,
      max=max.mem,
      last=used.values[length(used.values)],
      max.increase=max.mem-first,
      total=firstValue(free.lines[2]))
  on.exit()
  list(time=structure(new.time - time, class = "proc_time"),
       memory=data.frame(megabytes))
### List of time and memory usage.
}

