% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamBowlingWicketKindAllOppnAllMatches.R
\name{teamBowlingWicketKindAllOppnAllMatches}
\alias{teamBowlingWicketKindAllOppnAllMatches}
\title{team bowling wicket kind against all opposition all matches}
\usage{
teamBowlingWicketKindAllOppnAllMatches(matches,t1,t2="All",plot=TRUE)
}
\arguments{
\item{matches}{The matches of the team against all oppositions and all matches}

\item{t1}{Team for which bowling performance is required}

\item{t2}{t2=All gives the performance of the team against all opponents. Giving a opposing team (Australia, India
) will give the performance against this  team}

\item{plot}{If plot= TRUE the dataframe will be plotted else a data frame will be returned}
}
\value{
None or data fame
A data frame with the bowling performance in alll matches against all oppositions
}
\description{
This function computes returns kind of wickets (caught, bowled etc) of bowlers in all matches against
all oppositions. The user can chose to plot or return a data frame
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
#Get all matches between India  and other opposition
matches <-getAllMatchesAllOpposition("India",dir="../data/",save=TRUE)

# Or load directly from saved file
# load("allMatchesAllOpposition-India.RData")

teamBowlingWicketKindAllOppnAllMatches(matches,t1="India",t2="All")
m <-teamBowlingWicketKindAllOppnAllMatches(matches,t1="India",t2="All",plot=FALSE)

teamBowlingWicketKindAllOppnAllMatches(matches,t1="India",t2="Bangladesh")
teamBowlingWicketKindAllOppnAllMatches(matches,t1="India",t2="South Africa")
}

}
\author{
Tinniam V Ganesh
}
\references{
\url{http://cricsheet.org/}\cr
\url{https://gigadom.wordpress.com/}
}
\seealso{
\code{\link{teamBowlingScorecardAllOppnAllMatchesMain}}\cr
\code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
\code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
}

