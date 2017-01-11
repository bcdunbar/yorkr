##########################################################################################
# Designed and developed by Branden C Dunbar
# Date : 08 Jan 2017
# Function: matchWormGraph_ly
# This function adds plotly functionality to previous function matchWormChart
#
###########################################################################################
#' @title
#' Plot the match worm graph
#'
#' @description
#' This function  plots the match worm graph between 2 teams in a match
#'
#' @usage
#' matchWormGraph(match,t1,t2)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The 1st team of the match
#'
#' @param t2
#' the 2nd team in the match
#'
#' @return none
#'
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}\cr
#' \url{https://github.com/tvganesh/yorkrData}
#'
#' @author
#' Tinniam V Ganesh
#' Branden C Dunbar
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#' Maintainer: Branden C Dunbar \email{branden.dunbar@gmail.com}
#'
#' @examples
#' \dontrun{
#' #Get the match details
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#' # Plot tne match worm plot
#' matchWormGraph(a,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
matchWormGraph_ly <- function(match,t1,t2) {
  team=ball=totalRuns=NULL
  # Filter the performance of team1
  a <-filter(overs,team==t1)
  b <- select(a,ball,totalRuns,wicketPlayerOut,wicketKind,wicketFielder,bowler)
  # Check for both possibilities
  if(grepl("1st",b$ball[1])){
    c <-mutate(b,ball=gsub("1st\\.","",ball))
  } else{
    c <-mutate(b,ball=gsub("2nd\\.","",ball))
  }
  
  # as number 
  c$ball <- as.numeric(c$ball)
  
  # Compute cumulative sum vs balls bowled
  d <- mutate(c,total=cumsum(totalRuns))
  
  # Filter the performance of team2
  a1 <-filter(overs,team==t2)
  b1 <- select(a1,ball,totalRuns,wicketPlayerOut,wicketKind,wicketFielder,bowler)
  # Check for both possibilities
  if(grepl("2nd",b1$ball[1])){
    c1 <-mutate(b1,ball=gsub("2nd\\.","",ball))
  } else{
    c1 <-mutate(b1,ball=gsub("1st\\.","",ball))
  }
  
  #as number
  c1$ball <- as.numeric(c1$ball)
  
  # Compute cumulative sum vs balls bowled
  d1 <- mutate(c1,total=cumsum(totalRuns))
  
  
  # merge, reorder; add Delivery#
  df <- merge(d,d1,by="ball", all = TRUE)
  df$ball <- as.numeric(df$ball)
  df <- df[order(df$ball),]
  df$ID<-seq.int(nrow(df))
  
  # plot interactive worm with wicket markers (added wicket info)
  plot_ly(x=df$ID) %>%
    add_trace(y = df$total.x, name=t1, type='scatter', mode='lines',
              line = list(color = 'rgb(22, 96, 167)'),
              text=paste('Total: ',df$total.x), connectgaps=TRUE, hoverinfo='text+name') %>%
    add_trace(y = df$total.y, name=t2, type='scatter', mode='lines', 
              line = list(color = 'rgb(205, 12, 24)'),
              text=paste('Total: ',df$total.y), connectgaps=TRUE, hoverinfo='text+name') %>%
    add_markers(x=df$ID[df$wicketPlayerOut.x != "nobody"], 
                y = df$total.x[df$wicketPlayerOut.x != "nobody"], 
                name=paste(t1,'wicket'), type='scatter', 
                marker=list(size=8, color = 'rgb(22, 96, 167)'), hoverinfo='text',
                text=paste('Batsman: ',df$wicketPlayerOut.x[df$wicketPlayerOut.x != "nobody"],
                           '</br>Wicket: ',df$wicketKind.x[df$wicketPlayerOut.x != "nobody"],
                           '</br>Bowler: ',df$bowler.x[df$wicketPlayerOut.x != "nobody"],
                           '</br>Fielder: ',df$wicketFielder.x[df$wicketPlayerOut.x != "nobody"])) %>%
    add_markers(x=df$ID[df$wicketPlayerOut.y != "nobody"], 
                y = df$total.y[df$wicketPlayerOut.y != "nobody"], 
                name=paste(t2,'wicket'), type='scatter', 
                marker=list(size=8, color = 'rgb(205, 12, 24)'), hoverinfo='text',
                text=paste('Batsman: ',df$wicketPlayerOut.y[df$wicketPlayerOut.y != "nobody"],
                           '</br>Wicket: ',df$wicketKind.y[df$wicketPlayerOut.y != "nobody"],
                           '</br>Bowler: ',df$bowler.y[df$wicketPlayerOut.y != "nobody"],
                           '</br>Fielder: ',df$wicketFielder.y[df$wicketPlayerOut.y != "nobody"])) %>%
    layout(title = 'Worm Chart of Match', 
           xaxis = list(title='Deliveries'),
           yaxis = list(title='Runs'),
           legend = list(x = 0.1, y = 0.9))
}




  