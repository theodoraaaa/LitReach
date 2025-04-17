world.path <- fs::path_package("extdata", "World.rda", package = "LitReach")
country.path <- fs::path_package("extdata", "Country List.rda", package = "LitReach")
state.path <- fs::path_package("extdata", "State List.rda", package = "LitReach")
primary.path <- fs::path_package("extdata", "Primary Template.rda", package = "LitReach")
go.path <- fs::path_package("extdata", "G.rda", package = "LitReach")
pub.path <- fs::path_package("extdata", "P.rda", package = "LitReach")
wos.path <- fs::path_package("extdata", "W.rda", package = "LitReach")
sco.path <- fs::path_package("extdata", "S.rda", package = "LitReach")

load(world.path) #Load the world shapefile
load(country.path)
load(state.path)
load(primary.path)
load(go.path)
load(pub.path)
load(wos.path)
load(sco.path)

world <- world %>%
  mutate(country = case_when(country == "U.K. of Great Britain and Northern Ireland" ~ "United Kingdom",
                             TRUE ~ country))

countrylist <- countrylist %>%
  pull(country) #A list of countries created form the shape file being used to create the interactive map

statelist <- statelist %>%
  pull(state) #A list of all 50 US states. this is needed because some author affiliations only go as far as state

countrystatelist <- c(countrylist, statelist) #Combined country and state list

textfilecreate = function(filename, name){
  if(name == "title"){
    myfile = paste0(fs::path_package("extdata", package = "LitReach"), "/", name, ".txt")
    write.table(filename, file = myfile, sep = "", row.names = FALSE,
                col.names = FALSE, quote = FALSE, append = FALSE)
  }
}

termscreate <- function(data){

  walk2(data, names(data), textfilecreate)

  title.text <- readLines(fs::path_package("extdata", "title.txt", package = "LitReach"))

  docs <- Corpus(VectorSource(title.text))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "  ")

  docs <- tm_map(docs, content_transformer(tolower)) #Convert the text to lower case
  docs <- tm_map(docs, removeNumbers) #Remove numbers
  docs <- tm_map(docs, removeWords, stopwords("english")) #Remove English common stop words
  docs <- tm_map(docs, removePunctuation) #Remove punctuations
  docs <- tm_map(docs, stripWhitespace) #Eliminate extra white spaces

  term.list <- TermDocumentMatrix(docs)
  term.matrix <- as.matrix(term.list)
  term.count <- sort(rowSums(term.matrix),decreasing=TRUE)
  terms <- data.frame(word = names(term.count),freq=term.count) %>%
    slice_head(n = 200)

  return(terms)

}

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

makePopupPlot2 <- function(clickedArea, df) {
  # prepare the df for ggplot
  map.data.subplot <- df %>%
    group_by(country, type) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(country == clickedArea) %>%
    mutate(type1 = case_when(type == "JA" ~ "Journal Article",
                             type == "A" ~ "Article",
                             type == "GR" ~ "Government Report",
                             type == "NR" ~ "Non-Governmental Report",
                             type == "B" ~ "Book",
                             type == "CP" ~ "Confrence Proceedings",
                             type == "LE" ~ "Letter to the Editor",
                             type == "T" ~ "Thesis",
                             type == "P" ~ "Patent",
                             type == "UK"~ "Unknown")) #Prep data for popup plots

  popupPlot <- ggplot(map.data.subplot, aes(fill = type1, values = count)) +
    geom_waffle(n_rows = 25, size = .033, colour = "white", flip = TRUE, na.rm = TRUE) +
    scale_fill_manual(name = "Literature Type",
                      values = c(brewer.pal(7, "Dark2"), brewer.pal(3, "Set1"))) +
    coord_equal() +
    theme_ipsum_rc(grid=FALSE) +
    theme_enhance_waffle() +
    theme(text = element_text(size = 18)) #Create ggplot for popup plot of the specific country data

  return(popupPlot)
}

worlddataplot <- function(citdata, world){

  map.data.ava <- world %>%
    st_drop_geometry() %>% #Remove geometry
    left_join(citdata, by = c("country")) %>% #Join data
    group_by(country, type, year) %>%
    summarise(count = n()) %>% #Summarise data to counts
    ungroup() %>%
    na.omit()

  return(map.data.ava)

}

worlddatatable <- function(citdata, world){

  map.table.data <- world %>% #Prep dat for table view
    st_drop_geometry() %>%
    rowid_to_column(var = "country_id") %>%
    left_join(citdata, by = c("country")) %>%
    group_by(country_id, country, type) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    select(-c(country_id)) %>%
    rename(Country = country, "Literature Type" = type, Count = count) %>%
    na.omit() %>%
    arrange(Country)

  return(map.table.data)

}

to.choropleth <- function(land_dat, land_shapes, map.plot) {
  ## 1. Merge attributes with shapefiles ----
  #Terrestrial and Freshwater
  geog_merge <- left_join(land_shapes, land_dat %>% group_by(country) %>% summarise(count = sum(count)) %>% ungroup(), by = "country")
  centroids <- geog_merge %>%
    st_centroid() %>%
    na.omit() %>%
    select(-count) %>%
    mutate(long = unlist(map(geometry,1)),
           lat = unlist(map(geometry,2)))

  ## 2. Define palettes ----
  landbins <- c(1, 5, 10, 25, 50, 100, 200, 300, 500, 1000)
  landpal <- colorBin("YlGn", domain = geog_merge$n, bins = landbins)

  ## 3. Define labels ----
  landlabels <- sprintf(
    "<strong>%s</strong><br/>%g IID1 and IID2 Citations <sup></sup>",
    geog_merge$country, geog_merge$count
  ) %>% lapply(htmltools::HTML)


  ## 4. Map Choropleth: ----
  leaflet() %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(
      data = geog_merge,
      fillColor = ~landpal(count),
      weight = 1,
      opacity = 0.7,
      color = "black",
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 2,
        color = "white",
        fillOpacity = 0.5,
        bringToFront = TRUE),
      label = landlabels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding ="3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      #popup = popupGraph(map.plot, type = "png", width = 500, height = 500)) %>%
    addLegend(
      pal = landpal,
      values = geog_merge$count,
      opacity = 1,
      title = "Citations",
      position = "bottomright",
      group = "Land") %>%
    addMarkers(lng = centroids$long, lat = centroids$lat) %>%
    setView(lng = -1.5, lat = 53.4, zoom = 1.5)

}

