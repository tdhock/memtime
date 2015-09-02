\name{memtime}
\alias{memtime}
\title{memtime}
\description{Compute time and memory usage of R code.}
\usage{memtime(expr, gcFirst = TRUE, free.file = tempfile(), 
    sleep.seconds = 0.1)}
\arguments{
  \item{expr}{R code.}
  \item{gcFirst}{Run gc before computing initial time/memory values?}
  \item{free.file}{file where free output will be written.}
  \item{sleep.seconds}{Seconds to wait before/after calling free.profile.start/stop.}
}

\value{List of time and memory usage.}

\author{Toby Dylan Hocking}





