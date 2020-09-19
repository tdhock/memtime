\name{memtime}
\alias{memtime}
\title{memtime}
\description{Compute time and memory usage of R code. WARNING: since memory
usage is measured using free, it includes all running process
(even background processes which are not relevant to the R code,
\code{expr}). Memory usage numbers are only interpretable for your R code
when there is constant background process activity on your entire
system!}
\usage{memtime(expr, gcFirst = TRUE, 
    free.file = tempfile(), 
    sleep.seconds = 1)}
\arguments{
  \item{expr}{R code.}
  \item{gcFirst}{Run gc before computing initial time/memory values?}
  \item{free.file}{file where free output will be written.}
  \item{sleep.seconds}{Seconds to wait before/after calling free.profile.start/stop.}
}

\value{List of time and memory usage.}

\author{Toby Dylan Hocking}




\examples{

library(memtime)
## Example: mclapply uses more memory than lapply! (compare
## max.increase rows)
library(parallel)
N <- 10000
memtime({
  mclapply(1:N, function(i)NULL, mc.cores=2)
})
memtime({
  lapply(1:N, function(i)NULL)
})

}
