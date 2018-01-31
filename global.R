library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr) # data manupulation
library(tidyr)
library(plotly) # chart/graph
#library(reshape2)
library(ggplot2) # chart/grpah

library(leaflet) # map
library(RColorBrewer)
#library(googleVis)
library(maps)
library(DT) # data table

# For Word Cloud
library(tm)
library(wordcloud)
library(memoise)


#source("./helpers.R")

# data set load             ----------------------------------
raw.df <- as.data.frame(fread("./data_reduced.csv", stringsAsFactors = F))
region.df <- as.data.frame(fread("./region_code_reduced.csv", header=TRUE, stringsAsFactors = F))
region_map.df <- as.data.frame(fread("./region_map.csv", header=TRUE, stringsAsFactors = F))

# Change Variable Data Type ----------------------------------
raw.df <- raw.df %>% mutate(DATE = as.Date(DATE, "%Y-%m-%d"))
#raw.df <- raw.df %>% mutate(REGION = as.factor(REGION))

# remove row names
rownames(raw.df) <- NULL
rownames(region.df) <- NULL

# create variable with colnames as choice
choice <- colnames(raw.df)


############# only for test ##############################


#---------------------------------------------------------
#flights <- fread(file = "./flights14.csv")
# map-start ---------------------
colStates <- map("state", fill = TRUE, plot = FALSE,
                 region = c("florida", "louisiana", "mississippi",
                            "alabama", "georgia", "tennesse"))
#map-end ---------------------

# map/hist selectized input start -------------------- 
# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice2 <- colnames(state_stat)[-1]
# map/hist selectized input end --------------------

# Valid colors are:
Colorset <- c("green", "red", "yellow", "aqua", "blue", 
                "light-blue", "navy", "teal", "olive", "lime", 
                "orange", "fuchsia", "purple", "maroon", "black")
Titlecolor <- Colorset[15]
Titlewidth <- 8
list_RegionMap <- c(
  "Argentina" = "ar",
  "Austria" = "at",
  "Australia" = "au",
  "Belgium" = "be",
  "Bolivia" = "bo",
  "Brazil" = "br",
  "Canada" = "ca",
  "Switzerland" = "ch",
  "Chile" = "cl",
  "Colombia" = "co",
  "CostaRica" = "cr",
  "CzechRepublic" = "cz",
  "Germany" = "de",
  "Denmark" = "dk",
  "DominicanRepublic" = "do",
  "Ecuador" = "ec",
  "Estonia" = "ee",
  "Spain" = "es",
  "Finland" = "fi",
  "France" = "fr",
  "GreatBritain" = "gb",
  "Global" = "global",
  "Greece" = "gr",
  "Guatemala" = "gt",
  "HongKong" = "hk",
  "Honduras" = "hn",
  "Hungary" = "hu",
  "Indonesia" = "id",
  "Ireland" = "ie",
  "Iceland" = "is",
  "Italy" = "it",
  "Japan" = "jp",
  "Lithuania" = "lt",
  "Luxembourg" = "lu",
  "Latvia" = "lv",
  "Mexico" = "mx",
  "Malaysia" = "my",
  "Netherlands" = "nl",
  "Norway" = "no",
  "NewZealand" = "nz",
  "Panama" = "pa",
  "Peru" = "pe",
  "Philippines" = "ph",
  "Poland" = "pl",
  "Portugal" = "pt",
  "Paraguay" = "py",
  "Sweden" = "se",
  "Singapore" = "sg",
  "Slovakia" = "sk",
  "ElSalvador" = "sv",
  "Turkey" = "tr",
  "Taiwan" = "tw",
  "USA" = "us",
  "Uruguay" = "uy")

## Word Cloud ============================================================
# The list of valid books
books <<- list("A Mid Summer Night's Dream" = "summer",
               "The Merchant of Venice" = "merchant",
               "Romeo and Juliet" = "romeo")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./book/%s.txt.gz", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
})


## Leaflet with Shiny ============================================================
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
## Leaflet with Shiny ============================================================

## Plot  =========================================================================
nmRawdf = names(raw.df)
#rawInitArtist = unique(raw.df %>% filter(DATE > "2017-01-01" & DATE < "2017-01-31") %>% select(ARTIST))
rawInitArtist = unique(raw.df %>% select(ARTIST))
## Plotly with shiny =============================================================

#shinyApp(ui, server)