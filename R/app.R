library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyBS)
library(DT)

library(tidyverse)
library(here)
library(stringr)
library(stringi)
library(cld2)
library(polyglotr)
library(easyPubMed)
library(readxl)
library(textclean)
library(fs)

library(waffle)
library(hrbrthemes)
library(scales)

library(tm)
library(SnowballC)
library(wordcloud2)
library(RColorBrewer)
library(randomcoloR)

library(plotly)

library(sf)
library(leaflet)
library(leafpop)

library(cld2)
library(polyglotr)
library(igraph)
library(ggraph)
library(tidygraph)
library(plotly)
library(htmlwidgets)

library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("dataTableOutput", "DT")
conflict_prefer("layout", "plotly")

source(here::here("R", "functions.R"))

# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(title = "LitReach"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Instructions and Data Upload", tabName = "insup"),
      menuItem("Research Outputs", tabName = "researchoutput"),
      menuItem("Use in Literature", tabName = "how"),
      menuItem("Global Reach", tabName = "where"),
      menuItem("FAQ and References", tabName = "FAQ")
    )
  ),

  dashboardBody(

    tags$head(tags$style(HTML('

                        .modal-lg {
                        width: 95%;
                        height: 95%;

                        }
                      '))),

    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .navbar {
          background-color: #DCB847;
        }
        .skin-blue .main-header .logo {
          background-color: #23BAB8;
        }
        .skin-blue .main-sidebar {
          background-color: #1E103E;
        }
      "))
    ),

    tabItems(
      tabItem(
        tabName = "home",
        tags$img(src = "assets/LitReach Logo 2.PNG", style = 'position: absolute', width = "100%", height = "100%")
      ),
      tabItem(
        tabName = "insup",
        fluidRow(
          column(12, h3("Introduction"),
                p("Welcome! The LitReach app allows authors to visualise and assess their published works' global reach and use. The app provides a range of
                  visualisations that will enable authors to explore the citation data from their work in various ways, from citations in types of
                  literature to global usage. LitReach also includes a data tidying tool that will take the raw data from literature searches and
                  tidy it into a format that can be used in LitReach."),
          ),
        ),
        fluidRow(
          column(12, h3("Data Tidying Instructions"),
                 p("LitReach requires data in a specific format. The following are step-by-step instructions for using LitReach’s built-in data tidying tool, which
                   will get data into the format LitReach expects."),
                 br(),
                 p(strong("Step 1:"), "Download citation lists from any combination of Google Scholar, PubMed, Scopus and Web of Science (WoS)
                   (be sure to tick the affiliations box when downloading data from WoS and Scopus) as .csv or .xlsx files
                   (WoS only offers .xlsx, so you will have to save it as a CSV before uploading). Below, we have provided video and written guides to show
                   you how this is done. You do not need to have citation information from each source for LitReach to work (except you must include the primary CSV), but the more
                   data that are included, the better the outputs will be. Unfortunately, Google Scholar currently does not have a mass download
                   function for citations, so you will have to use the Publish or Perish (Harzin. 2007) software (link below) and we have included a video tutorial
                   for using this software."),
                 tags$a(href="https://harzing.com/resources/publish-or-perish", "Link to the Publish or Perish Website", target = "_blank"),
                 br(),
                 br(),
                 actionButton("PubMed", "Watch PubMed Guide", icon = icon("play-circle")),
                 actionButton("Scopus", "Watch Scopus Guide", icon = icon("play-circle")),
                 actionButton("WebofScience", "Watch Web of Science Guide", icon = icon("play-circle")),
                 actionButton("PoP", "Watch Publish or Perish Guide", icon = icon("play-circle")),
                 bsModal("PubMedVid", "Watch the PubMed Guide", "PubMed", size = "large", tags$iframe(src = "PubMed.mp4", height = "900", width = "100%"), tags$body(p("PubMed:"),
                                                                                                                                                                  p("1. For your selected article, scroll down to the end of the 'Cited by' section and select 'See all Cited by articles'"),
                                                                                                                                                                  p("2. Press 'Save' and select 'All results' and 'CSV' from the drop-down menus"),
                                                                                                                                                                  p("3. Press 'Create file' and save your .csv file"))),
                 bsModal("ScoVid", "Watch the Scopus Guide", "Scopus", size = "large", tags$iframe(src = "Scopus.mp4", height = "900", width = "100%"), tags$body(p("Scopus:"),
                                                                                                                                                                  p("1. On the search page for your selected article, press on the number of citations for the article"),
                                                                                                                                                                  p("2. Check the 'Select all records' box to select all articles on the page"),
                                                                                                                                                                  p("3. Press 'Export' and select 'Excel'"),
                                                                                                                                                                  p("4. Select the first or third option to select all records and select 'Full Record' from the Record Content drop-down menu and press Export"),
                                                                                                                                                                  p("5. Save your .csv file"))),
                 bsModal("WoSVid", "Watch the Web of Science Guide", "WebofScience", size = "large", tags$iframe(src = "Web of Science.mp4", height = "900", width = "100%"),tags$body(p("Web of Science:"),
                                                                                                                                                                                       p("1. On the search page for your selected article, press on the number of citations for the article"),
                                                                                                                                                                                       p("2. Select 'All' and press 'Export' at the top of the page"),
                                                                                                                                                                                       p("3. Select 'CSV' as your method of export and ensure that both Citation information and Bibliographical information are selected and press 'Export'"),
                                                                                                                                                                                       p("4. Save your .csv file"))),
                 bsModal("PoPVid", "Watch the Publish or Perish Guide", "PoP", size = "large", tags$iframe(src = "PoP.mp4", height = "900", width = "100%"),tags$body(p("Publish or Perish:"),
                                                                                                                                                                                       p("1. Start a new Google Scholar search in Publish or Perish"),
                                                                                                                                                                                       p("2. Type in the title into the 'Title Words' field"),
                                                                                                                                                                                       p("3. Right-click on the desired paper and select 'Retrieve Citing Working in Publish or Perish'"),
                                                                                                                                                                                       p("4. On the right, click 'Save Results' and then click the 'Results as CSV...' option"))),
                 br(),
                 br(),
                 p(strong("Step 2:"), "Create a .csv file containing the information about your primary reference (the references you are examining).
                   The columns from citations to downloads are information taken from places
                   like Altmetric, and if they are not available for your paper, leave them as 0 (mentionsO = mentions in other places,
                   mentionsP = mentions in policy sources). A template for this CSV file can be downloaded below the image describing the data.
                   You can leave the id, type and primary columns as they appear in the template."),
                 downloadButton("primarytemp", "Download Primary Reference Template"),
                 br(),
                 br(),
                 img(src="assets/primary1.png", height = "100%", width = "100%"),
                 br(),
                 br(),
                 p(strong("Step 3:"), "Upload the primary reference file and the other citation data using the fields provided below. There is one
                   field per data source, so be careful to upload each one to the specified box."),
                 p(strong("Step 4:"), "After uploading the data, click the 'Tidy Data' button. The data tidying will take some time, and once the data has been tidied,
                   it will be displayed in a table below the upload fields. You can then download the data using the download button below the table."),
                 p(strong("Step 5:"), "Due to Google Scholar downloads not including the country the first author is affiliated with or the type of literature,
                   these must be entered manually before uploading the data to LitReach. Below is a list of the types of literature and
                   their associated codes. Please enter the letter code associated with each type in the type column:"),
                 p("JA: Journal Article, A: Article, GR: Government Report, NR: Non-Governmental Report, B: Book, LE: Letter to the Editor, T: Thesis, P: Patent, CP: Conference Proceedings, UK: Unknown"),
                 fileInput("rawuploadpri", "Upload CSV Data from Primary Source",
                           accept = ".csv", buttonLabel = list(icon("upload"), "Upload"), multiple = FALSE),
                 tableOutput("rawtabpri"),
                 fileInput("rawuploadgo", "Upload CSV Data from Google",
                           accept = ".csv", buttonLabel = list(icon("upload"), "Upload"), multiple = FALSE),
                 tableOutput("rawtabgo"),
                 fileInput("rawuploadpub", "Upload CSV Data from PubMed",
                           accept = ".csv", buttonLabel = list(icon("upload"), "Upload"), multiple = FALSE),
                 tableOutput("rawtabpub"),
                 fileInput("rawuploadwos", "Upload CSV Data from Web of Science",
                           accept = ".csv", buttonLabel = list(icon("upload"), "Upload"), multiple = FALSE),
                 tableOutput("rawtabwos"),
                 fileInput("rawuploadsco", "Upload CSV Data from Scopus",
                           accept = ".csv", buttonLabel = list(icon("upload"), "Upload"), multiple = FALSE),
                 tableOutput("rawtabsco"),
                 br(),
                 actionButton("tidy", "Tidy Data", icon = icon("wand-magic-sparkles")),
          ),
        ),
        fluidRow(
          column(12,
                 tableOutput("citetidy"),
                 br(),
                 downloadButton("datadownload", "Download Data")
          ),
        ),
        fluidRow(
          column(12, h3("Uploading your Data to LitReach"),
                 p("Once you have the data in the correct format and all fields filled in (see the image below for an example), you can upload the data
                  to run LitReach."),
                 img(src="assets/citation.jpg", height = "100%", width = "100%"),
                 br(),
                 fileInput("impupload", "Choose CSV File",
                           accept = ".csv", buttonLabel = list(icon("upload"), "Upload"), multiple = FALSE),
                 tableOutput("impdatatab")
          ),
        ),
        fluidRow(
          column(12,
                 p("LitReach Copyright (C) 2025 Ben Rowland", style = "font-size:10px"),
                 p("This program comes with ABSOLUTELY NO WARRANTY.", style = "font-size:10px"),
                 p("This is free software, and you are welcome to redistribute it.", style = "font-size:10px")
            ),
          ),
        ),

      tabItem(
        tabName = "researchoutput",
        fluidRow(
          column(1)
          ),
        fluidRow(
          column(9,
                  withSpinner(plotOutput("primaryplot"))
          ),
        fluidRow(
          column(9,
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 downloadButton("downloadPlot", "Download Plot")),
          ),
        ),
      ),
      tabItem(
        tabName = "how",
        fluidRow(
          column(12,
                 withSpinner(wordcloud2Output("word_cloud")),
                 p("*Top 200 words form the titles of the papers citing IID1 and IID2 output."),
          ),
          downloadButton("saveword", "Download word cloud"),
        ),
        fluidRow(
          br()
        ),
        fluidRow(
          column(12,
                 withSpinner(plotlyOutput("tree1")),
          )
        ),
        fluidRow(
          column(12,
                 textOutput("legend"),
                 p(strong("JA:"), HTML('&nbsp;'), "Journal Article", HTML('&emsp;'), strong("A:"), HTML('&nbsp;'), "Article", HTML('&emsp;'), strong("GR:"), HTML('&nbsp;'), "Government Report",
                   HTML('&emsp;'), strong("NR:"), HTML('&nbsp;'), "Non-Governmental Report", HTML('&emsp;'), strong("B:"), HTML('&nbsp;'), "Book", HTML('&emsp;'),
                   HTML('&emsp;'), strong("LE:"), HTML('&nbsp;'), "Letter to the Editor", HTML('&emsp;'), HTML('&emsp;'), strong("T:"), HTML('&nbsp;'), "Thesis",
                   HTML('&emsp;'), strong("P:"), HTML('&nbsp;'), "Patent", HTML('&emsp;'), strong("CP:"), HTML('&nbsp;'), "Conference Proceedings", HTML('&emsp;'), strong("UK:"), HTML('&nbsp;'), "Unknown"),
                 p("Note that without data from Google Scholar the second figure on tab three will only show journal articles as the data from the
                   other sources do not extend past journal articles into grey literature."),
          ),
          downloadButton("treedownload", "Download literature sources tree diagram"),
          br(),
        ),
        fluidRow(
          withSpinner(plotlyOutput("timecs")),
          downloadButton("download_timecs", "Download citations over time plot"),
          br(),
          withSpinner(plotlyOutput("timepy")),
          downloadButton("download_timepy", "Download citations by year plot")
        )
      ),
      tabItem(
        tabName = "where",
        withSpinner(leafletOutput("map", height = 1000)),
        "The shapefile for the interactive map can be found ",
        tags$a(href = "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/table/", "here", target = "_blank"),
        br(),
        br(),
        actionButton("DataTable", "View Data as a Table", icon = icon("table")),
        bsModal("mapdatamodal", "Map Data as Table", "DataTable", size = "large", dataTableOutput("maptbl"),
                downloadButton("downloadtable", "Download")),
        downloadButton("download_map", "Download map"),
      ),
      tabItem(
        tabName = "FAQ",
        br(),
        fluidRow(
          column(1),
          column(4,
                 br(),
                 p(strong("How were duplicate references removed?"), style = "font-size:18px"),
                 p("Duplicate referneces were removed from the data by removing exact matches between author, title and the iid project being refernced. Further duplicates
                   were removed if the reference title had 15 or fewer characters differnt to another title.", style = "font-size:16px"),
                 br(),
                 p(strong("How was the country associated with each piece of literaure decided?"), style = "font-size:18px"),
                 p("The country associated with each with piece of literature was determined based on the country of affiliation for the first author",
                   style = "font-size:16px"),
                 br(),
                 p(strong("What constitutes a journal article?"), style = "font-size:18px"),
                 p("A journal article consisted of but was not lmited to research articles, reviews and published protocols", style = "font-size:16px"),
                 br(),
                 p(strong("What constitutes an article?"), style = "font-size:18px"),
                 p("An article consisted of but was not lmited to features, editorials and news articles", style = "font-size:16px"),
                 br(),
                 p(strong("Why do some of the reseatch outputs have 0 in some of the catagories?"), style = "font-size:18px"),
                 p("The research output metrics were collected from the publishers. Not all publishers collect the same metric data so the data displayed here is what was available.", style = "font-size:16px"),
                 br(),
                 p(strong("Why are there fewer boxes then the number of citations, views etc?"), style = "font-size:18px"),
                 p("To make them viewable in the waffle plot the metrics were divided by 5.", style = "font-size:16px"),
                 br(),
                 p(strong("When was the app last updated?"), style = "font-size:18px"),
                 p("31/01/2025", style = "font-size:16px"),
                 p(strong("References"), style = "font-size:24px"),
                 p(strong("Harzing, A.W. (2007) Publish or Perish, available from https://harzing.com/resources/publish-or-perish"), style = "font-size:18px"),

          )
        )
      )
    )
  )
)

server <- function(input, output) {

  output$primarytemp <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Primary Template", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write_csv(primarytemp, file)
    }
  )

  set.seed(1234)

  #### Tidy Data ####

  output$rawtabpri <- renderTable(input$rawuploadpri)
  output$rawtabgo <- renderTable(input$rawuploadgo)
  output$rawtabpub <- renderTable(input$rawuploadpub)
  output$rawtabwos <- renderTable(input$rawuploadwos)
  output$rawtabsco <- renderTable(input$rawuploadsco)

observeEvent(input$tidy, {

    inFilepri <- input$rawuploadpri
    pri.df <- read_csv(inFilepri$datapath)
    print("1")

    if(is.null(input$rawuploadgo) == TRUE){

      go.df
      print("2")

    } else{

      inFilego <- input$rawuploadgo
      go.df <- read_csv(inFilego$datapath)
      print("2")

    }

    if(is.null(input$rawuploadpub) == TRUE){

      pub.df
      print("3")

    } else{

      inFilepub <- input$rawuploadpub
      pub.df <- read_csv(inFilepub$datapath)
      print("3")

    }

    if(is.null(input$rawuploadwos) == TRUE){

      wos.df
      print("4")

    } else{

      inFilewos <- input$rawuploadwos
      wos.df <- read_csv(inFilewos$datapath)
      print("4")

    }

    if(is.null(input$rawuploadsco) == TRUE){

      sco.df
      print("5")

    } else{

      inFilesco <- input$rawuploadsco
      sco.df <- read_csv(inFilesco$datapath)
      print("5")

    }

    #Google
    if(nrow(go.df) > 0) {

    google.data.tidy <- go.df %>%
      dplyr::select(Authors, Title, Year) %>% #Select wanted columns
      mutate(Authors = str_replace(Authors, "\\,", "|")) %>% #Replace , with |
      separate(Authors, into = c("author", "trash"), sep = "\\|") %>% #Separate column at |
      separate(author, into = c("junk", "author"), " ") %>% #Separate new author column at the space
      dplyr::select(-c(junk, trash)) %>% #Remove junk columns
      rename(title = Title, year = Year) %>%
      na.omit() %>% #Remove all NAs
      mutate(language = detect_language(title)) %>% #Detect the language of the titles
      rowwise() %>%
      mutate(n = google_translate(title, target_language = "en", source_language = "auto")) %>% #Translate each title
      ungroup() %>%
      mutate(language1 = detect_language(n)) %>%  #Double check newly translated columns language
      dplyr::select(author, "title" = n, year, language1) %>% #Select wanted columns
      na.omit() %>% #Remove all NAs
      group_by(title) %>%
      mutate(id = cur_group_id()) %>% #Group identical titles
      ungroup() %>%
      arrange(id) %>%
      distinct() %>% #Remove duplicates
      dplyr::select(-c(language1, id))

    } else {

      google.data.tidy <- NULL

    }

    #PubMed
    if(nrow(pub.df) > 0) {

    citations <- pub.df %>%
      slice(-1) %>% #Remove top one which is the reference for the output in question
      rowid_to_column(var = "id") #Create and new id column from row ids

    all.articles <- NULL

    for (ttl in 1:nrow(citations)) {#For each row

      ID <- citations %>%
        filter(id == ttl) %>%
        pull(PMID) #Get PubMed id

      batch_pubmed_download(paste0(ID), dest_dir = here::here("data")) #Download PubMed data

      readLines(here::here("data", "easyPubMed_data_01.txt")) %>% #Read in the new file
        str_replace_all("\\&[[:graph:]]*\\;", "") %>% #Remove non alphanumeric characters
        writeLines(here::here("Data", "pubfile.txt")) #Write the file back out to avoid it ever being in dataframe format which caused issues with special characters

      pubmed.info.list <- articles_to_list(here::here("data", "pubfile.txt")) #Convert PubMed data to list

      if(length(pubmed.info.list) > 1) stop() #If there is more then one citation in the list stop the loop
      if(is.null(article_to_df(pubmed.info.list))) { #If there is no citation in the list

        title <- citations %>%
          filter(id == ttl) %>%
          pull(Title) #Get current title

        all.articles <- all.articles %>%
          add_row(author = NA, title = title, year = NA, country = NA) #Add it to data frame without PubMed data to be done manually

        next()

      }

      article.df <- article_to_df(pubmed.info.list, getAuthors = TRUE, autofill = TRUE) %>%
        slice(1) %>% #Take the first row
        mutate(country = str_which(address, paste0(countrystatelist, "$|", countrystatelist, "\\.|", countrystatelist, "\\ |", countrystatelist,
                                                   "\\,"))[1],
               country = countrylist[country]) %>% #Identify what the country of affiliation is using the country list created above
        dplyr::select("author" = lastname, title, year, country)

      all.articles <- all.articles %>%
        bind_rows(article.df) #Join data all references from one citation

      file.remove(here::here("data", "easyPubMed_data_01.txt")) #Remove any files created
      file.remove(here::here("data", "pubfile.txt")) #Remove any files created

    }

    pubmed.data.tidy <- all.articles %>%
      mutate(year = as.numeric(year),
             type = "JA")

    } else {

      pubmed.data.tidy <- NULL

    }

    print("6")

    #Scopus
    if(nrow(sco.df) > 0) {

    scopus.data.tidy <- sco.df %>%
      mutate(Authors = str_replace(Authors, "\\,", "|")) %>% #Replace , with |
      separate(Authors, into = c("author", "trash"), sep = "\\|") %>% #Separate at |
      select(author, "title" = Title, "year" = Year, affiliation = Affiliations) %>%
      mutate(country = str_extract(affiliation, str_c(countrylist, collapse="|")),
             type = "JA") %>%
      select(-affiliation)

    } else {

      scopus.data.tidy <- NULL

    }

    #Web of Science
    if(nrow(wos.df) > 0) {

      wos.data.tidy <- wos.df %>%
        select(type = "Publication Type", Authors, title = "Article Title", Addresses, year = "Publication Year", address = "Addresses") %>%
        mutate(type = case_when(type == "J" ~ "JA", #If the literature type is J make it JA
                                TRUE ~ type)) %>%
        mutate(Authors = str_replace(Authors, "\\,", "|"),
               country = str_extract(address, str_c(countrylist, collapse="|"))) %>%
        separate(Authors, into = c("author", "trash"), sep = "\\|") %>%
        select(type, author, title, year, country)

    } else {

      wos.data.tidy <- NULL

    }

    #All

    all.data <- pri.df %>%
      bind_rows(pubmed.data.tidy) %>%
      bind_rows(google.data.tidy) %>%
      distinct(author, title, year, .keep_all = TRUE) %>%
      bind_rows(scopus.data.tidy) %>%
      distinct(author, title, year, .keep_all = TRUE) %>%
      bind_rows(wos.data.tidy) %>%
      distinct(author, title, year, .keep_all = TRUE) %>% #Join each data and remove duplicates
      mutate(country = case_when(country %in% c(statelist, "US", "USA") ~ "United States of America",
                                 country %in% c("England", "Scotland", "Wales", "Northern Ireland", "UK", "United Kingdom") ~
                                   "United Kingdom",
                                 country == "Russia" ~ "Russian Federation",
                                 country %in% c("South Korea", "Korea") ~ "Republic of Korea",
                                 country == "Tiapei" ~ "Taiwan",
                                 country == "Iran" ~ "Iran (Islamic Republic of)",
                                 TRUE ~ country),
             title = str_remove(title, "</i>")) %>% #Enter the correct country names
      mutate(author = str_replace_all(author, "[[:punct:]]", ""), #remove all punctuation in author column
             title = str_replace_all(title, "[[:punct:]]", ""), #remove all punctuation in title column
             title = tolower(title), #Make title lowercase
             title_temp = str_replace_all(title, "\\ ", ""), #remove all spaces in title to create one lone string or compressed titles
             title_temp = replace_non_ascii(title_temp, ""), #Remove non ascii text
             title_temp = str_replace_all(title_temp, pattern="\\(C\\)", replacement=""), #Remove copyright sign
             title1 = title,
             title1 = replace_non_ascii(title1, "")) %>%
      mutate(primary = case_when(primary != 1 ~ 0,
                                 TRUE ~ primary)) %>%
      distinct(author, title_temp, year, .keep_all = TRUE) %>% #Remove duplicates
      distinct(author, title_temp, .keep_all = TRUE) %>% #Remove duplicates
      rowid_to_column(var = "ref_id") %>%
      mutate(type = case_when(type == "C" ~ "CP",
                              TRUE ~ type))

    print("7")

    strings <- c(all.data$title_temp) #vector of all the compressed titles
    new.data <- all.data

    for(ii in 1:length(strings)){ #For each string

      string <- strings[ii]

      if(is.na(string) == TRUE) break()

      new.data <- new.data %>%
        rowwise() %>%
        mutate(score = adist(title_temp, string)[,1]) %>% #Identify how many characters would need to be changed to turn one title to another
        ungroup() %>%
        filter(score > 10 | score == 0) #Remove very similar titles

      strings <- c(new.data$title_temp)

    }

    print("8")

    all.data1 <- new.data %>%
      select(-c(score, title_temp, title1, id)) %>%
      rowid_to_column(var = "id")

    all.data2 <- all.data1 %>%
      distinct(title, .keep_all = TRUE) %>%
      mutate(title_temp = str_replace_all(title, "\\ ", ""))


    strings <- c(all.data2$title_temp)
    new.data <- all.data2

    print("9")

    #Repeat above process but cut off for similar titles is not 15 characters
    for(ii in 1:length(strings)){

      string <- strings[ii]

      #if(is.na(string) == TRUE) stop("Reached end of strings")
      if(is.na(string) == TRUE) break()
      new.data <- new.data %>%
        rowwise() %>%
        mutate(score = adist(title_temp, string)[,1]) %>%
        ungroup() %>%
        filter(score > 15 | score == 0) %>%
        distinct(title_temp, .keep_all = TRUE)

      strings <- c(new.data$title_temp)

    }

    new.data1 <- new.data %>%
      select(-c(title_temp, score, id)) %>%
      mutate(year = as.character(year))

    print("10")

    output$citetidy <- renderTable(head(new.data1))

    output$datadownload <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("Citation Data", ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write_csv(new.data1, file)
      }
    )

  })



  #### Results ####

  output$impdatatab <- renderTable(input$impupload)

  impdata <- reactive({
    req(input$impupload)

    inFile <- input$impupload

    if(is.null(inFile)) return(NULL)

    df <- readLines(inFile$datapath)

    data1 <- df %>%
      mutate(type = case_when(type %in% c("CP", "D") ~ "CP",
                              type %in% c("LE", "R") ~ "LE",
                              type %in% c("T", "DI") ~ "T",
                              type %in% c("B", "BC") ~ "B",
                              TRUE ~ type),
             title = str_replace_all(title, "[^[:alnum:][:whitespace:]]", ""))

    return(data1)

  })

  output$primaryplot <- renderPlot(

   impdata() %>%
        filter(primary == 1) %>% #Filter primary literature
        rowid_to_column(var = "id1") %>% #Create new column from row ids
        filter(id1 == 1) %>% #Filter first one
        select(title, citations, mentionsO, mentionsP, views, downloads) %>%
        pivot_longer(citations:downloads, names_to = "met", values_to = "num") %>% #Convert data to long
        mutate(num1 = ceiling(num/5), #divide metrics by 5 for easier viewing
               met = as.factor(met)) %>%
        select(-title) %>%
        replace(is.na(.), 0) %>%
        ggplot(aes(fill = met, values = num1)) +
        geom_waffle(n_rows = 25, size = .033, colour = "white", flip = TRUE, na.rm = TRUE) +
        scale_fill_manual(name = NULL,
                          values = c(citations = "tomato", mentionsP = "purple", mentionsO = "cyan", views = "limegreen", downloads = "gold2"),
                          labels = c(citations = paste0("Citations ", "(", impdata() %>% filter(primary == 1) %>% select(citations) %>% pull(citations), ")"),
                                     mentionsP = paste0("Policy Mentions ", "(", impdata() %>% filter(primary == 1) %>% select(mentionsP) %>% pull(mentionsP), ")"),
                                     mentionsO = paste0("Other Mentions ", "(", impdata() %>% filter(primary == 1) %>% select(mentionsO) %>% pull(mentionsO), ")"),
                                     views = paste0("Views ", "(", impdata() %>% filter(primary == 1) %>% select(views) %>% pull(views), ")"),
                                     downloads = paste0("Downloads ", "(", impdata() %>% filter(primary == 1) %>% select(downloads) %>% pull(downloads), ")")),
                          drop = FALSE) +
        coord_equal() +
        theme_ipsum_rc(grid="") +
        theme_enhance_waffle() +
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
              legend.position = "bottom",
              text = element_text(size = 20)) +
        guides(fill = guide_legend(nrow = 3)), height = 800, width = 1200
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Research outputs", ".png", sep = "")
    },
    content = function(file) {
      # Save the plot to the specified file
      ggsave(file, plot =    impdata() %>%
               filter(primary == 1) %>% #Filter primary literature
               rowid_to_column(var = "id1") %>% #Create new column from row ids
               filter(id1 == 1) %>% #Filter first one
               select(title, citations, mentionsO, mentionsP, views, downloads) %>%
               pivot_longer(citations:downloads, names_to = "met", values_to = "num") %>% #Convert data to long
               mutate(num1 = ceiling(num/5), #divide metrics by 5 for easier viewing
                      met = as.factor(met)) %>%
               select(-title) %>%
               replace(is.na(.), 0) %>%
               ggplot(aes(fill = met, values = num1)) +
               geom_waffle(n_rows = 25, size = .033, colour = "white", flip = TRUE, na.rm = TRUE) +
               scale_fill_manual(name = NULL,
                                 values = c(citations = "tomato", mentionsP = "purple", mentionsO = "cyan", views = "limegreen", downloads = "gold2"),
                                 labels = c(citations = paste0("Citations ", "(", impdata() %>% filter(primary == 1) %>% select(citations) %>% pull(citations), ")"),
                                            mentionsP = paste0("Policy Mentions ", "(", impdata() %>% filter(primary == 1) %>% select(mentionsP) %>% pull(mentionsP), ")"),
                                            mentionsO = paste0("Other Mentions ", "(", impdata() %>% filter(primary == 1) %>% select(mentionsO) %>% pull(mentionsO), ")"),
                                            views = paste0("Views ", "(", impdata() %>% filter(primary == 1) %>% select(views) %>% pull(views), ")"),
                                            downloads = paste0("Downloads ", "(", impdata() %>% filter(primary == 1) %>% select(downloads) %>% pull(downloads), ")")),
                                 drop = FALSE) +
               coord_equal() +
               theme_ipsum_rc(grid="") +
               theme_enhance_waffle() +
               theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
                     legend.position = "bottom",
                     text = element_text(size = 20)) +
               guides(fill = guide_legend(nrow = 3)), device = "png")
    }
  )

  #Store the word cloud in a reactiveValues object
  values <- reactiveValues(wordcloud = NULL)

  #Generate word cloud once and store it
  observe({
    set.seed(1234)  # Ensure reproducibility
    colours <- randomColor(200)
    terms <- termscreate(impdata())
    values$wordcloud <- wordcloud2(terms, color = colours)
  })

  output$word_cloud <- renderWordcloud2({
    values$wordcloud
  })

  output$saveword <- downloadHandler(
    filename = "wordcloud.html",
    content = function(file){
      saveWidget(
        widget = values$wordcloud,
        file = file
      )
    }
  )

  #Tab3- Tree Diagrams
  output$tree1 <- renderPlotly(
    #Plotly tree diagram

    impdata() %>%
      group_by(type) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      plot_ly(
      type='treemap',
      labels=.$type,
      parents = "",
      values = .$count,
      marker = list(colors = brewer.pal(9, "Set3")),
      domain=list(column=0)) %>%
      layout(legend = TRUE)

  )

  output$treedownload <- downloadHandler(
    filename = "tree.html",
    content = function(file){
      saveWidget(
        widget = impdata() %>%
          group_by(type) %>%
          summarise(count = n()) %>%
          ungroup() %>%
          plot_ly(
            type='treemap',
            labels=.$type,
            parents = "",
            values = .$count,
            marker = list(colors = brewer.pal(9, "Set3")),
            domain=list(column=0)) %>%
          layout(legend = TRUE),
        file = file
      )
    }
  )

  #Tab 3- Citations Over Time

  plot_timecs <- reactive({
    impdata() %>%
    mutate(number =  1) %>%
    group_by(year) %>%
    summarise(cpy = sum(number)) %>%
    mutate(ccs = cumsum(cpy)) %>%
    ungroup() %>%
    ggplot(aes(x = year, y = ccs)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Citations over Time") +
    theme_bw() +
    theme(text = element_text(size = 16))
  })

  output$timecs <- renderPlotly({

    plot_timecs()
    ggplotly(source = "timecs")

  })

  output$download_timecs <- downloadHandler(
    filename = function() { paste("citations_time", '.png', sep='') },
    content = function(file) {
      ggsave(file,  plot=plot_timecs(), width = 500, height = 200, units = "mm", device = "png")
    }
  )


  plot_timepy <- reactive({
    impdata() %>%
      mutate(number =  1) %>%
      group_by(year) %>%
      summarise(cpy = sum(number)) %>%
      mutate(ccs = cumsum(cpy)) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = cpy)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Citations per Year") +
      theme_bw() +
      theme(text = element_text(size = 16))

  })

  output$timepy <- renderPlotly({

      plot_timepy()
      ggplotly(source = "timepy")

  })

  output$download_timepy <- downloadHandler(
    filename = function() { paste("citations_year", '.png', sep='') },
    content = function(file) {
      ggsave(file,  plot=plot_timepy(), width = 500, height = 200, units = "mm", device = "png")
    }
  )

  #Tab 4- Map

  map_plot <- reactive({

    to.choropleth(worlddataplot(impdata(), world), world, worldplots(impdata(), world))

  })

  output$map <- renderLeaflet(

    map_plot()

  )

  output$maptbl <- DT::renderDT(

    worlddatatable(impdata(), world),
    options = list(lengthChange = FALSE)

    )

  output$downloadtable <- downloadHandler(
    filename = function() {
      paste0("Map Data", ".csv")
    },
    content = function(file) {
      write_csv(worlddatatable(impdata(), world), file)
    }
  )

  output$download_map <- downloadHandler(
    filename = function() {
      paste("map", ".html", sep = "")
    },
    content = function(file) {
      saveWidget(map_plot(), file)  # Ensure you're passing the reactive map here
    }
  )

}

# Run the application
#shinyApp(ui = ui, server = server)
