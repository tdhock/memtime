memtime <- function
### Compute time and memory usage of R code. WARNING: since memory
### usage is measured using free, it includes all running process
### (even background processes which are not relevant to the R code,
### expr). Memory usage numbers are only interpretable for your R code
### when there is constant background process activity on your entire
### system!
(expr,
### R code.
 gcFirst = TRUE,
### Run gc before computing initial time/memory values?
 free.file=tempfile(),
### file where free output will be written.
 sleep.seconds=1
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
  on.exit({
    free.profile.stop(free.file)
    cat("Time/memory measurement stopped at:", ppt(proc.time() - time), 
        "\n")
  })
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
  kilobytes <- 
    c(first.used=first,
      max.used=max.mem,
      last.used=used.values[length(used.values)],
      max.increase=max.mem-first,
      total=firstValue(free.lines[2]))
  megabytes <- as.integer(kilobytes/1024)
  on.exit()
  list(time=structure(new.time - time, class = "proc_time"),
       memory=data.frame(kilobytes, megabytes))
### List of time and memory usage.
}

