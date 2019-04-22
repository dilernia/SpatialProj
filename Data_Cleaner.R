#################
# Title: Data cleaning for spatial final project
# Author: Andrew DiLernia
# Date: 04/07/2019
# Purpose: Prepare data for spatial final project
#################

library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(ggspatial)
library(RColorBrewer)
library(pdftools)

# Data Sources
# Census population data: http://www.minneapolismn.gov/census/2010/index.htm
# ACS population data: https://www.mncompass.org/profiles/neighborhoods/minneapolis-saint-paul#!areas
# Crime data: http://opendata.minneapolismn.gov/
# Weather data: https://www.dnr.state.mn.us/climate/twin_cities/listings.html
# key: T=trace | M=missing | S=multi-day value follows | A=multi-day value | blank=unknown
# Temperatures in Farenheit, Rain/Snow in inches

# New neighborhood data
# Pop 2010: http://www.minneapolismn.gov/residents/neighborhoods/index.htm 

# Helpful sources
# CRAN spatiotemporal overview: https://cran.r-project.org/web/views/SpatioTemporal.html

# Scraping neighborhood data
compNeighbors <- read.csv("FinalProj/neighborhoodList.csv", header = FALSE) %>% 
  rename(Neighborhood = V1) %>% 
  mutate(Neighborhood = gsub(gsub(gsub(tolower(Neighborhood), 
                                  pattern = "\\s+", replacement = ""), 
                                  pattern = "-", replacement = ""),
                                  pattern = "[.]", replacement = ""))

# Downloading pdf's
lapply(X = compNeighbors$Neighborhood,
       FUN = function(x) {download.file(url = paste0("http://www.minneapolismn.gov/regservices/",
       x, "profile2016"), paste0("FinalProj/PDFs/", x, ".pdf"), mode="wb")})

# Extracting text from PDF's
extractor <- function(neighbor) {
  print(neighbor)
text <- pdf_text(paste0("FinalProj/PDFs/", neighbor, ".pdf"))[-c(1:5, 8:13)]

# Extracting population estimates
pop2010 <- text[1] %>% str_split(pattern = "other than English\r\n                   Number:") %>% 
  map(2) %>% trimws() %>% str_split(pattern = " ") %>% map(1) %>% unlist() %>% 
  gsub(pattern = ",", replacement = "")

# # Extracting perc 17 years or younger
# percU17 <- text[1] %>% str_split(pattern = "Percentage:               100.0%               ") %>% 
#   map(2) %>% str_split(pattern = " ") %>% map(1) %>% gsub(pattern = "%", replacement = "")
# 
# # Extracting perc of housing units vacant
# percVacant <- text[2] %>% str_split(pattern = "Percentage:            100.0%              ") %>% 
#   map(2) %>% str_split(pattern = " ") %>% map(10) %>% gsub(pattern = "%", replacement = "")
# 
# # Extracting perc with income less than $35,000
# percL35 <- text[2] %>% str_split(pattern = "Education & Household Income") %>% map(2) %>% 
#   str_split(pattern = " ") %>% map(962) %>% gsub(pattern = "%", replacement = "")

return(data.frame(Neighborhood = neighbor, pop2010 = pop2010))
}

# Extracting neighborhood data from PDFs
neighbor2010 <- map_dfr(.x = compNeighbors$Neighborhood, .f = extractor)

# Proper case function
propCase <- function(x) {
  x <- as.character(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Reading in crime data
year <- 2011
crimes <- st_read(paste0("FinalProj/Police_Incidents_",
year, "/Police_Incidents_", year, ".shp"))

# Reading in neighborhood boundaries data and merging
neighborhoods <- st_read("FinalProj/Neighborhoods/Neighborhoods.shp")
neighborhoods$BDNAME <- as.factor(sapply(X = neighborhoods$BDNAME, FUN = propCase))
crimeFull <- st_join(crimes, neighborhoods) %>% rename(Neighborhood = BDNAME, Type = Descriptio) %>% 
  select(-c(PublicAddr, ControlNbr, CCN, Precinct, ReportedDa, Time, UCRCode, 
            EnteredDat, GBSID, X, Y, Neighborho, LastChange, LastUpdate, BDNUM,
            TEXT_NBR, ESRI_OID, OBJECTID, INT_REFNO, PREFIX, SYMBOL_NAM))

# Cleaning crime variable
crimeFull$Type <- crimeFull$Type %>% gsub(pattern = "Of", replacement = "of") %>% 
  gsub(pattern = "Motr", replacement = "Motor") %>% gsub(pattern = "Vehc", replacement = "Vehicle") %>% 
  gsub(pattern = "From", replacement = "from")

# Cleaning Date variable
crimeFull <- crimeFull %>% 
  rename(DateTime = BeginDate) %>% 
  mutate(DateTime = ymd_hms(DateTime), Day = wday(DateTime, label = T), 
         Month = month(DateTime, label = T), Year = year(DateTime), Date = date(DateTime))

# Reading in and merging weather data
weather <- read_csv("FinalProj/Weather2010_2019.csv") %>% 
  mutate(Date = date(ymd(gsub(Date, pattern = "-", replacement = "")))) %>% 
  rename(MaxTemp = `Maximum Temperature degrees (F)`,
         MinTemp = `Minimum Temperature degrees (F)`,
         Rain = `Precipitation (inches)`, Snow = `Snow (inches)`,
         SnowAccum = `Snow Depth (inches)`)

# Changing trace amount to 0 and making columns numeric
weather[, -c(1)] <- lapply(weather[, -c(1)], FUN = function(x){as.numeric(gsub(as.character(x), pattern = "T",
                                                  replacement = "0.00"))})

# Merging crime and weather data
temp <- crimeFull %>% left_join(weather, by = c("Date" = "Date"))

# Importing and cleaning neighborhood level characteristics
neighbor2010 <- read_csv("FinalProj/MSP Neighborhoods_2010.csv")
neighbor1317 <- read_csv("FinalProj/MSP Neighborhoods_2013-2017.csv")

# Creating pie chart of most common crimes
ncrimes <- 5
crimeTab <- table(crimeFull$Type) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(Type = Var1)
crimeTab$Type <- as.character(crimeTab$Type)
cutoff <- crimeTab %>% top_n(5, Freq) %>% select(Freq) %>% min()
crimeTab[which(crimeTab$Freq < cutoff), "Type"] <- "Other Crime"

crimeTab <- crimeTab %>% group_by(Type) %>% summarize(Freq = sum(Freq)) %>% arrange(Freq)
crimeColor <- data.frame(Crime = c("Burglary of Business", "Motor Vehicle Theft",  "Theft from Motor Vehicle", 
               "Burglary of Dwelling", "Other Crime", "Other Theft"),
               Color = brewer.pal(n = ncrimes + 1, name = "Dark2"))
# Creating pie chart
pie(x = crimeTab[match(crimeTab$Type, crimeColor$Crime), ]$Freq, 
    labels = crimeTab[match(crimeTab$Type, crimeColor$Crime), ]$Type, 
    col = as.character(crimeColor$Color),
    main = paste0("Minneapolis Crimes in ", year))

# Creating bar chart
crimeTab$Type <- factor(crimeTab$Type, 
                        levels = c("Burglary of Business", "Motor Vehicle Theft",  "Theft from Motor Vehicle", 
                                   "Burglary of Dwelling", "Other Crime", "Other Theft"))
crimeTab %>% ggplot(aes(x = Type, y = Freq)) + 
  geom_col(fill = as.character(crimeColor$Color)) +
  labs(title = paste0("Minneapolis Crimes in ", year), x = "") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

# Making neighborhood map using ggplot
myMap <- neighborhoods %>% ggplot(aes(fill = BDNAME)) + 
  geom_sf(aes(fill = BDNAME)) + 
  ggtitle("Minneapolis Neighborhoods") + 
  labs(x = "Longitude", y = "Latitude") + theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = -1*seq(from = 93.32, to = 93.20, length.out = 4))
myMap

myMap <- neighborhoods %>% ggplot(aes(fill = BDNAME)) + 
  geom_sf(aes(fill = BDNAME)) + 
  ggtitle("Neighborhood Crime Rates") + 
  labs(x = "Longitude", y = "Latitude") + theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = -1*seq(from = 93.32, to = 93.20, length.out = 4))

#ggsave("text.png", plot = myMap, device = "png")

# Map of crime rate by day


# # Making leaflet map
# crimeFull %>% leaflet() %>% addProviderTiles("MapBox", options = providerTileOptions(
#   id = "mapbox.light",
#   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
#   setView(lng = -93.2629, lat = 44.9879, zoom = 12) 

# Example -----------------------------------------------------------------
#https://www.rdocumentation.org/packages/spBayes/versions/0.4-2/topics/spMvGLM
library(MBA)
library(spBayes)

##Some useful functions
rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p)))){stop("Dimension problem!")}
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

set.seed(1)

# Parameters for simulated data
n <- 25 #number of locations
q <- 5 #number of outcomes at each location
nltr <- q*(q+1)/2 #number of triangular elements in the cross-covariance matrix

# Generating observed coordinates
coords <- cbind(runif(n,0,1), runif(n,0,1))

# Parameters for the bivariate spatial random effects
theta <- rep(3/0.5,q)
A <- matrix(0,q,q)
A[lower.tri(A,TRUE)] <- runif(nltr, min = 0, max = 1)
K <- A%*%t(A)
Psi <- diag(0, q)
C <- mkSpCov(coords, K, Psi, theta, cov.model = "exponential")

w <- rmvn(1, rep(0, n*q), C)
ws <- c()
for(i in 1:q) {
ws[[i]] <- w[seq(i, length(w), q)]
names(ws)[i] <- paste0("w", i)
}

# Covariate portion of the mean (intercept and 2 predictors here)

# Design matrix for each location
xMats <- list()
for(i in 1:q) {
  p0 <- 1
  p1 <- scale(rnorm(n))
  p2 <- rbinom(n = 1, size = n, prob = 0.20)
  xMats[[i]] <- cbind(p0, p1, p2)
}

# Overall design matrix
design <- mkMvX(xMats)

# Number of predictors
p <- dim(xMats[[1]])[2]

# True intercept and slope values
betaVec <- c()
for(b in 1:p) {
  betaVec <- c(betaVec, runif(n = q)/3)
}

# Number of trials
weight <- 10

# True probabilities
p <- 1/(1+exp(-(design %*% betaVec + w)))

# Responses
y <- rbinom(n*q, size = rep(weight, n*q), prob = p)

ys <- list()
for(i in 1:q) {
  ys[[i]] <- y[seq(i, length(y), q)]
  names(ys)[i] <- paste0("y", i)
}

# Call spMvLM
fit <- glm((y/weight)~design-1, weights = rep(weight, n*q), 
           family="binomial")
beta.starting <- coefficients(fit)
beta.tuning <- t(chol(vcov(fit)))

A.starting <- diag(1,q)[lower.tri(diag(1,q), TRUE)]

n.batch <- 100
batch.length <- 50
n.samples <- n.batch*batch.length

starting <- list("beta"=beta.starting, "phi"=rep(3/0.5,q), "A"=A.starting, "w"=0)
tuning <- list("beta"=beta.tuning, "phi"=rep(1,q), "A"=rep(0.1,length(A.starting)),
               "w"=0.5)
priors <- list("beta.Flat", "phi.Unif"=list(rep(3/0.75,q), rep(3/0.25,q)),
               "K.IW"=list(q+1, diag(0.1,q)))

m.1 <- spMvGLM(list(y.1~x.1-1, y.2~x.2-1),
               coords=coords, weights=matrix(weight,n,q),
               starting=starting, tuning=tuning, priors=priors,
               amcmc=list("n.batch"=n.batch,"batch.length"=batch.length,"accept.rate"=0.43),
               cov.model="exponential", n.report=25)

burn.in <- 0.75*n.samples
sub.samps <- burn.in:n.samples

print(summary(window(m.1$p.beta.theta.samples, start=burn.in))$quantiles[,c(3,1,5)])

beta.hat <- t(m.1$p.beta.theta.samples[sub.samps,1:length(B)])
w.hat <- m.1$p.w.samples[,sub.samps]

p.hat <- 1/(1+exp(-(x%*%beta.hat+w.hat)))

y.hat <- apply(p.hat, 2, function(x){rbinom(n*q, size=rep(weight, n*q), prob=p)})

y.hat.mu <- apply(y.hat, 1, mean)

# Unstack to get each response variable fitted values
y.hat.mu.1 <- y.hat.mu[seq(1,length(y.hat.mu),q)]
y.hat.mu.2 <- y.hat.mu[seq(2,length(y.hat.mu),q)]

# Function for creating contour plots
contourPlot <- function(coords, response, title) {
  surf <- mba.surf(cbind(coords, response), 
                   no.X = 100, no.Y = 100, extend = TRUE)$xyz.est
  image(surf, main = title)
  contour(surf, add = TRUE)
  points(coords)
  zlim <- range(surf[["z"]], na.rm=TRUE)
}

par(mfrow=c(2,2))
contourPlot(response = y.1, title = "Observed y.1 positive trials",
            coords = coords)
contourPlot(response = y.hat.mu.1, title = "Fitted y.1 positive trials",
            coords = coords)
contourPlot(response = y.2, title = "Observed y.2 positive trials",
            coords = coords)
contourPlot(response = y.hat.mu.2, title = "Fitted y.2 positive trials",
            coords = coords)
