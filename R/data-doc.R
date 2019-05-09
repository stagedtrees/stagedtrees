#' Pokemon Go Users
#'
#' Demographic information of a population of possible Pokemon Go users.
#' 
#'
#' @format A data frame with 999 rows and 5 variables:
#' \describe{
#'   \item{Use}{\code{Y} if the individual used the app, \code{N} otherwise}
#'   \item{Age}{\code{>30} if the individual is older than 30, 
#'               \code{<=30} otherwise}
#'   \item{Degree}{\code{Yes} if the individual completed a Higher 
#'                 Education degree, \code{No} otherwise}
#'   \item{Gender}{\code{Male} or \code{Female}}
#'   \item{Activity}{\code{Yes} if the individual was physically active 
#'                   (i.e. had a walk longer than 30 mins, went for a run or 
#'                   had a bike ride to get some exercise) in the past week 
#'                   before the experiment, \code{No} otherwise}
#' }
#' 
#' @source \url{https://osf.io/xy5g6/}
#' @references Gabbiadini, Alessandro, Christina Sagioglou, and Tobias Greitemeyer. 
#' "Does Pokémon Go lead to a more physically active life style?."
#'  *Computers in Human Behavior* 84 (2018): 258-263.
"Pokemon"

#' PhD Students Publications
#'
#' Number of publications of 915 PhD biochemistry students 
#' during the 1950’s and 1960’s
#' 
#'
#' @format A data frame with 915 rows and 6 variables:
#' \describe{
#'   \item{Articles}{Number of articles during the last 3 years of PhD: either 
#'                   \code{0}, \code{1-2} or \code{>2}.}
#'   \item{Gender}{\code{male} or \code{female}.}
#'   \item{Kids}{\code{yes} if the student has at least one kid 5 or younger, 
#'               \code{no} otherwise.}
#'   \item{Married}{\code{yes} or \code{no}.}
#'   \item{Mentor}{Number of publications of the student's mentor: 
#'                \code{low} between 0 and 3, \code{medium} between 4 and 10, 
#'                \code{high} otherwise.}
#'   \item{Prestige}{\code{low} if the student is at a low-prestige university, 
#'         \code{high} otherwise.}
#' }
#' 
#' @source The data has been modified from the \code{Rchoice} package.
#' @references Long, J. S. (1990). The origins of sex differences in science.
#'  *Social Forces*, 68(4), 1297-1316.
"PhDArticles"

#' IsReallyTrump
#'
#' Information about tweets posted between January 2016 and February 2017 by the Twitter account \code{realDonaldTrump}, which are either written by himself or his team. tweets posted by @realDonaldTrump are either written by himself or his team.
#' Donald Trump is known to have used an Android smartphone to write his tweets, whereas his team
#' uses iOS devices. 
#' 
#'
#' @format A data frame with 2746 rows and 8 variables:
#' \describe{
#'   \item{Source}{\code{iOS} if the tweet was sent by an iOS device, \code{Android} otherwise}
#'   \item{Words}{\code{>20} if the tweet has more than 20 words, \code{<=20} otherwise}
#'   \item{Sentiment}{\code{Negative} if the overall sentiment of the tweet was negative, \code{Positive} if the overall sentiment of the tweet was positive, or \code{Other} otherwise}
#'   \item{Day}{\code{Weekdays} or \code{Weekend}}
#'   \item{Time}{\code{>10am} if the tweet was sent after 10am , \code{<=10am} otherwise}
#'   \item{URL}{\code{TRUE} if the tweet included an URL, \code{FALSE} otherwise}
#' }
#' 
#' @source \url{https://github.com/kahultman/trump-tweets}
"Trump"
