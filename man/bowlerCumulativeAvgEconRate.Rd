% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bowlerCumulativeAvgEconRate.R
\name{bowlerCumulativeAvgEconRate}
\alias{bowlerCumulativeAvgEconRate}
\title{Bowler's cumulative average economy rate}
\usage{
bowlerCumulativeAvgEconRate(df,name)
}
\arguments{
\item{df}{Data frame}

\item{name}{Name of batsman}
}
\value{
None
}
\description{
This function computes and plots the cumulative average economy rate  of a bowler
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{)
#'Get the data frame for RA Jadeja
jadeja <- getBowlerWicketDetails(team="India",name="Jadeja",dir=pathToFile)
bowlerCumulativeAvgEconRate(jadeja,"RA Jadeja")
}
}
\author{
Tinniam V Ganesh
}
\references{
\url{http://cricsheet.org/}\cr
\url{https://gigadom.wordpress.com/}\cr
\url{https://github.com/tvganesh/yorkrData}
}
\seealso{
\code{\link{batsmanCumulativeAverageRuns}}
\code{\link{bowlerCumulativeAvgWickets}}
\code{\link{batsmanCumulativeStrikeRate}}
\code{\link{batsmanRunsVsStrikeRate}}
\code{\link{batsmanRunsPredict}}
}

