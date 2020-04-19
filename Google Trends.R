#install.packages("gtrendsR")
devtools::install_github('diplodata/gtrendsR',force=T)
#devtools::install_github('PMassicotte/gtrendsR', ref = 'new-api',force=T)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(gtrendsR,maps,ggplot2)
install.packages("reshape2")
install.packages("anytime")
install.packages("ggmap")
library(gtrendsR)
library(reshape2)
library(anytime)
library(pacman)
library(ggmap)
google.trends = gtrends(c("blu-ray"), gprop = "web", time = "all")
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL

plot(google.trends)



google.trends = gtrends(c("Rajnikanth","Kamalhassan","TTV Dinakaran","Edappadi","O Panneerselvam"),gprop = "news",time="today+5-y")
warnings()
data("categories")

google.trends = gtrends(c("Superman","Batman","spiderman","Iron Man","Hulk"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")

google.trends = gtrends(c("Iron man","Hulk","Captain America"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")

google.trends = gtrends(c("Superman","Hulk","spiderman","Superboy","Captain America"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")
  
google.trends = gtrends(c("Superman","Hulk"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")

google.trends = gtrends(c("Spider-man","Superboy"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")

google.trends = gtrends(c("Wolverine","Lobo"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")

google.trends = gtrends(c("Storm","Wonder Woman"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")

google.trends = gtrends(c("gun control","gun shop"),gprop= c("web","news","images","froogle","youtube"),time ="today+5-y",category = "3")
plot(google.trends)
head(gprop)
head(gtrends("NHL")$interest_over_time)
head(gtrends("NHL")$related_topics)
head(gtrends("NHL")$related_queries)
head(gtrends(c("NHL", "NFL"))$interest_over_time)
head(gtrends(c("NHL", "NFL"), geo = c("CA", "US"))$interest_over_time)
## Sport category (20)
data(categories)
categories[grepl("^Sport", categories$name), ]
gtrends(c("NHL", "NFL"), geo = c("CA", "US"), category = 20)

gtrends(c("NHL", "NFL"), time = "now 1-H") # last hour
gtrends(c("NHL", "NFL"), time = "now 4-H") # last four hours
gtrends(c("NHL", "NFL"), time = "now 1-d") # last day
gtrends(c("NHL", "NFL"), time = "today 1-m") # last 30 days
gtrends(c("NHL", "NFL"), time = "today 3-m") # last 90 days
gtrends(c("NHL", "NFL"), time = "today 12-m") # last 12 months
gtrends(c("NHL", "NFL"), time = "today+5-y") # last five years (default)
gtrends(c("NHL", "NFL"), time = "all") # since 2004

plot(gtrends)



#' Google Trends Query
#' 
#' The \code{gtrends} default method performs a Google Trends query for the 
#' \sQuote{query} argument and session \sQuote{session}. Optional arguments for 
#' geolocation and category can also be supplied.
#' 
#' @param keyword A character vector with the actual Google Trends query 
#'   keywords. Multiple keywords are possible using \code{gtrends(c("NHL", 
#'   "NBA", "MLB", "MLS"))}.
#'   
#' @param geo A character vector denoting geographic regions for the query, 
#'   default to \dQuote{all} for global queries. Multiple regions are possible 
#'   using \code{gtrends("NHL", c("CA", "US"))}.
#'   
#' @param time A string specifying the time span of the query. Possible values
#'   are:
#'   
#'   \describe{ \item{"now 1-H"}{Last hour} \item{"now 4-H"}{Last four hours} 
#'   \item{"now 1-d"}{Last day} \item{"now 7-d"}{Last seven days} \item{"today
#'   1-m"}{Past 30 days} \item{"today 3-m"}{Past 90 days} \item{"today
#'   12-m"}{Past 12 months} \item{"today+5-y"}{Last five years (default)} 
#'   \item{"all"}{Since the beginning of Google Trends (2004)} \item{"Y-m-d
#'   Y-m-d"}{Time span between two dates (ex.: "2010-01-01 2010-04-03")} }
#'   
#' @param category A character denoting the category, defaults to \dQuote{0}.
#'   
#' @param gprop A character string defining the Google product for which the 
#'   trend query if preformed. Valid options are:
#'   
#'   \itemize{ \item "web" (default) \item "news" \item "images" \item "froogle"
#'   \item "youtube" }
#'   
#' @param hl A string specifying the ISO language code (ex.: \dQuote{en-US} or 
#'   \dQuote{fr}). Default is \dQuote{en-US}. Note that this is only influencing
#'   the data returned by related topics.
#'   
#' @param low_search_volume Logical. Should include low search volume regions?
#'   
#' @section Categories: The package includes a complete list of categories that 
#'   can be used to narrow requests. These can be accessed using 
#'   \code{data("categories")}.
#'   
#' @section Related topics: Note that *related topics* are not retrieved when
#'   more than one keyword is provided due to Google restriction.
#'   
#' @importFrom stats na.omit reshape setNames
#' @importFrom utils URLencode read.csv
#'   
#' @return An object of class \sQuote{gtrends} (basically a list of data
#'   frames).
#'   
#' @examples
#' 
#' \dontrun{
#' 
#' head(gtrends("NHL")$interest_over_time)
#' head(gtrends("NHL")$related_topics)
#' head(gtrends("NHL")$related_queries)
#' 
#' head(gtrends(c("NHL", "NFL"))$interest_over_time)
#' 
#' head(gtrends(c("NHL", "NFL"), geo = c("CA", "US"))$interest_over_time)
#' 
#' ## Interest by city
#' 
#' gtrends(keyword="obama",geo="US-AL-630")
#' 
#' ## Sport category (20)
#' data(categories)
#' categories[grepl("^Sport", categories$name), ]
#' gtrends(c("NHL", "NFL"), geo = c("CA", "US"), category = 20)
#'  
#' ## Playing with time format
#' 
#' gtrends(c("NHL", "NFL"), time = "now 1-H") # last hour
#' gtrends(c("NHL", "NFL"), time = "now 4-H") # last four hours
#' gtrends(c("NHL", "NFL"), time = "now 1-d") # last day
#' gtrends(c("NHL", "NFL"), time = "today 1-m") # last 30 days
#' gtrends(c("NHL", "NFL"), time = "today 3-m") # last 90 days
#' gtrends(c("NHL", "NFL"), time = "today 12-m") # last 12 months
#' gtrends(c("NHL", "NFL"), time = "today+5-y") # last five years (default)
#' gtrends(c("NHL", "NFL"), time = "all") # since 2004
#' 
#' 
#' ## Custom date format
#' 
#' gtrends(c("NHL", "NFL"), time = "2010-01-01 2010-04-03") 
#' 
#' ## Search from various Google's services
#' 
#' head(gtrends(c("NHL", "NFL"), gprop = "news")$interest_over_time)
#' head(gtrends(c("NHL", "NFL"), gprop = "youtube")$interest_over_time)
#' 
#' ## Language settings
#' 
#' head(gtrends("NHL", hl = "en")$related_topics)
#' head(gtrends("NHL", hl = "fr")$related_topics)
#' }
#' @export
gtrends <- function(
  keyword, 
  geo = "", 
  time = "today+5-y", 
  gprop = c("web", "news", "images", "froogle", "youtube"), 
  category = 0,
  hl = "en-US",
  low_search_volume = FALSE) {
  
  stopifnot(
    # One  vector should be a multiple of the other
    (length(keyword) %% length(geo) == 0) || (length(geo) %% length(keyword) == 0),
    is.vector(keyword),
    length(keyword) <= 5,
    length(geo) <= 5,
    length(time) == 1,
    length(hl) == 1,
    is.character(hl),
    hl %in% language_codes$code
  )
  
  
  ## Check if valide geo
  if (geo != "" &&
      !all(geo %in% c(as.character(countries[, "country_code"]), as.character(countries[, "sub_code"])))) {
    stop("Country code not valid. Please use 'data(countries)' to retreive valid codes.",
         call. = FALSE)
  }
  
  ## Check if valide category
  if (!all(category %in% categories[, "id"]))  {
    stop("Category code not valid. Please use 'data(categories)' to retreive valid codes.",
         call. = FALSE)
  }
  
  ## Check if time format is ok
  if (!check_time(time)) {
    stop("Can not parse the supplied time format.", call. = FALSE)
  }
  
  # time <- "today+5-y"
  # time <- "2017-02-09 2017-02-18"
  # time <- "now 7-d"
  # time <- "all_2006"
  # time <- "all"
  # time <- "now 4-H"
  # geo <- c("CA", "FR", "US")
  # geo <- c("CA", "DK", "FR", "US", "CA")
  # geo <- "US"
  
  gprop <- match.arg(gprop, several.ok = FALSE)
  gprop <- ifelse(gprop == "web", "", gprop)
  
  # ****************************************************************************
  # Request a token from Google
  # ****************************************************************************
  
  comparison_item <- data.frame(keyword, geo, time, stringsAsFactors = FALSE)
  
  widget <- get_widget(comparison_item, category, gprop, hl)
  
  # ****************************************************************************
  # Now that we have tokens, we can process the queries
  # ****************************************************************************
  
  interest_over_time <- interest_over_time(widget, comparison_item)
  interest_by_region <- interest_by_region(widget, comparison_item, low_search_volume)
  related_topics <- related_topics(widget, comparison_item, hl)
  related_queries <- related_queries(widget, comparison_item)
  
  res <- list(
    interest_over_time = interest_over_time, 
    interest_by_country = do.call(rbind, interest_by_region[names(interest_by_region) == "country"]),
    interest_by_region = do.call(rbind, interest_by_region[names(interest_by_region) == "region"]),
    interest_by_dma = do.call(rbind, interest_by_region[names(interest_by_region) == "dma"]),
    interest_by_city = do.call(rbind, interest_by_region[names(interest_by_region) == "city"]),
    related_topics = related_topics, 
    related_queries = related_queries
  )
  
  ## Remove row.names
  res <- lapply(res, function(x){ row.names(x) <- NULL; x})
  
  class(res) <- c("gtrends", "list")
  
  return(res)
  
}

#' Plot Google Trends interest over time
#' 
#' @param x A \code{\link{gtrends}} object.
#' @param ... Additional parameters passed on in method dispatch. Currently not
#'   used.
#'   
#' @import ggplot2
#'   
#' @return A ggplot2 object is returned silently.
#' @export
#' 
#' @examples
#' \dontrun{
#' res <- gtrends("nhl", geo = c("CA", "US"))
#' plot(res)
#' }
plot.gtrends <- function(x, ...) {
  
  df <- x$interest_over_time
  
  df$legend <-  paste(df$keyword, " (", df$geo, ")", sep = "")
  
  p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) +
    geom_line() +
    xlab("Date") +
    ylab("Search hits") +
    ggtitle("Interest over time") +
    theme_bw() +
    theme(legend.title = element_blank()) 
  
  print(p)
  invisible(p)
  
}

plot(gtrends)

res <- gtrends("nhl", geo = c("CA", "US"))
plot(res)


library(gtrendsR)
library(reshape2)

google.trends = gtrends(c("blu-ray"), gprop = "web", time = "all")
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL


hurricanes = gtrends(c("Katrina","Harvey","Irma"), gprop = "web",time="all", geo = c("US"))
plot(hurricanes)

str(hurricanes)

harvey = gtrends(c("Harvey"), gprop = "web",time="2017-08-18 2017-08-25", geo = c("US"))
harvey = harvey$interest_by_region
harvey$region = sapply(harvey$location,tolower)
statesMap = map_data("state")
harveyMerged = merge(statesMap,harvey,by="region")

harveyPlot=ggplot() +
  geom_polygon(data=harveyMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkred",guide="colorbar",trans="log10") +
  theme_bw() +
  labs(title="Google search interest for Hurricane Harvey in each state")
harveyPlot

irma = gtrends(c("Irma"), gprop = "web", time="2017-09-03 2017-09-10",geo = c("US"))
irma = irma$interest_by_region
statesMap = map_data("state")
irma$region = sapply(irma$location,tolower)
irmaMerged = merge(statesMap ,irma,by="region")

regionLabels <- aggregate(cbind(long, lat) ~ region, data=irmaMerged, 
                          FUN=function(x) mean(range(x)))

irmaPlot=ggplot() +
  geom_polygon(data=irmaMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkblue",guide="colorbar",trans="log10") +
  geom_text(data=regionLabels, aes(long, lat, label = region), size=2) +
  theme_bw() +
  coord_fixed(1.3) +
  labs(title="Google search interest for Hurricane Irma in each state\nfrom the week prior to landfall in the US") 
irmaPlot
guns = gtrends(c("gun control","gun shop"), c("web","news","images","froogle","youtube"),time="all", geo = c("US"))
head(guns)
guns = guns$interest_by_region
statesMap=map_data("state")
guns$region = sapply(guns$location,tolower)
gunsMerged = merge(statesMap,guns,by="region")

gunsMerged

regionLabels <- aggregate(cbind(long, lat) ~ region, data=gunsMerged, 
                          FUN=function(x) mean(range(x)))

gunsPlot= ggplot() +
  geom_polygon(data=gunsMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="green",high="darkblue",guide="colorbar",trans="log10") +
  geom_text(data=regionLabels, aes(long, lat, label = region), size=2) +
  theme_bw() +
  coord_fixed(1.3) +
  labs(title="Google search interest for Gun Control in each state\n in the US") 
gunsPlot

music = gtrends(c("spotify","deezer","apple music","youtube"), "web",time="all", geo = c("US"))
music[complete.cases(music), ]
music = music$interest_by_region
statesMap =map_data("state")
music$region = sapply(music$location,tolower)
musicMerged = merge(statesMap,music,by="region")
musicMerged
musicMerged[musicMerged<0] = 1
musicMerged
musicMerged[complete.cases(musicMerged), ]
regionLabels <- aggregate(cbind(long, lat) ~ region, data=musicMerged, 
                          FUN=function(x) mean(range(x)))

musicPlot= ggplot() +
  geom_polygon(data=musicMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="green",high="darkblue",guide="colorbar",trans="log10") +
  geom_text(data=regionLabels, aes(long, lat, label = region), size=2) +
  theme_bw() +
  coord_fixed(1.3) +
  labs(title="Google search interest for disruptive music providers in each state\n in the US") 
musicPlot


cyclones = gtrends(c("Hudbud","Phailin","Vardah"), gprop = c("web","news","images","froogle","youtube"),time="all", geo = c("IN"))
data("country_codes")
?country_codes
plot(cyclones)

str(cyclones)

Vardah = gtrends(c("Vardah"), gprop = c("web","news","images","froogle","youtube"),time="all", geo = c("IN"))
Vardah = Vardah$interest_by_region
Vardah$region = sapply(Vardah$location,tolower)
statesMap = map_data("state")
head(statesMap)
VardahMerged = merge(statesMap,Vardah,by="region")

VardahPlot=ggplot() +
  geom_polygon(data=VardahMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkred",guide="colorbar",trans="log10") +
  theme_bw() +
  labs(title="Google search interest for Cyclone Vardah")
VardahPlot

irma = gtrends(c("Irma"), gprop = "web", time="2017-09-03 2017-09-10",geo = c("US"))
irma = irma$interest_by_region
statesMap = map_data("state")
irma$region = sapply(irma$location,tolower)
irmaMerged = merge(statesMap ,irma,by="region")

regionLabels <- aggregate(cbind(long, lat) ~ region, data=irmaMerged, 
                          FUN=function(x) mean(range(x)))

irmaPlot=ggplot() +
  geom_polygon(data=irmaMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkblue",guide="colorbar",trans="log10") +
  geom_text(data=regionLabels, aes(long, lat, label = region), size=2) +
  theme_bw() +
  coord_fixed(1.3) +
  labs(title="Google search interest for Hurricane Irma in each state\nfrom the week prior to landfall in the US") 
irmaPlot

map <- qmap('TamilNadu', zoom = 10, maptype = 'hybrid')
map