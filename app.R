#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rjson)
library(lubridate)
library(dplyr)
library(stringr)
library(shinyjs)
library(ggplot2)
library(plotly)
library(tidyverse)

### TRANSFORMATION OF THE DATA AS IT'S READ FROM THE CSV
t <- read.csv("cards.csv", encoding="UTF-8", stringsAsFactors=FALSE)
t$CMC <- as.numeric(t$CMC) # CMC to number
t$power <- ifelse(is.na(as.numeric(t$power)), NA, as.numeric(t$power)) # coerce P/T to number
t$toughness <- ifelse(is.na(as.numeric(t$toughness)), NA, as.numeric(t$toughness))
t$color <- gsub(", ", "", t$color, fixed=T) # keep color as only "WUBRG" format
t$setdate <- ymd(t$setdate) # turn date column into date objects
t$name_raw <- t$name # keep the name without HTML as a variable

#html anchor tag to oracle page for printing
#https://gatherer.wizards.com/pages/card/details.aspx?multiverseid=1672
t$name <- ifelse(is.na(t$multi_id) == FALSE, paste("<a href='https://gatherer.wizards.com/pages/card/details.aspx?multiverseid=", t$multi_id, "'>", t$name, "</a>", sep=""), paste(t$name))

#html image tag to image at oracle page for printing
#https://gatherer.wizards.com/Handlers/Image.ashx?multiverseid=489562&type=card
t$Card <- ifelse(is.na(t$multi_id) == FALSE, paste("<img width = 230 src='https://gatherer.wizards.com/Handlers/Image.ashx?multiverseid=", t$multi_id, "&type=card'>", sep=""), paste("Image not found."))

# capitalize column names
colnames(t) <- str_to_title(colnames(t))

# edit column names
colnames(t)[c(6:8, 13)] <- c("Set_Name", "CMC", "MC", "Date_Released")

# set order of table columns
t <- t %>% select("Name", "Card", "Color", "Power", "Toughness", "MC", "CMC", "Type", 
                  "Set_Name", "Date_Released", "Rarity", "Number", "Multi_id", "Id", "Name_raw")

# arrange by date. this is the table that the shiny app will be drawing from
t <- t %>% arrange(Date_Released)

### set colors for later use in REGEX
colors <- c("W", "U", "B", "R", "G", "Colorless")

### COLUMNS FOR FILTER CHECKBOX
col_list <- colnames(t)[-c(1:2, 13:15)]
col_selected <- c("Color", "MC", "CMC", "Power", "Toughness", 
                  "Rarity", "Type", "Set_Name", "Date_Released")

### COLUMNS FOR GRAPH DROPDOWN
var_col <- c("Color", "Power", "Toughness", "CMC", "Type", "Rarity")

# UI
ui <- fluidPage(
    
    useShinyjs(),
    
    # set navbar
    navbarPage("Magic Cards",
               # primary card search area
               tabPanel("Card Search",
                        # sidebar where filter options can be set
                        sidebarLayout(
                            div(
                                id="form",
                                sidebarPanel(
                                    h2("Here's my MTG card search tool."),
                                    "There isn't much here right now. All card info courtesy of MTGJson. 
                        All cards and images belong to Wizards of the Coast. I am not affiliated with any group 
                        representing MTG.", 
                                    br(), br(),
                                    "To build a decklist, click the row of the card you want to add.",
                                    # columns the user can filter by, default determined by col_selected global var
                                    checkboxGroupInput("checkGroup", label=h4("Columns"),
                                                       choices=col_list, selected = col_selected, inline=TRUE),
                                    # color filter
                                    checkboxGroupInput("checkColor", label=h4("Colors"),
                                                       choices=colors,
                                                       selected=colors, inline=TRUE),
                                    # whether or not the search is exclusive
                                    checkboxGroupInput("exclusiveSearch", label=span("Exclusive Search (Only Cards with ALL colors)"),
                                                       choices="On"),
                                    # remove dupicates
                                    checkboxGroupInput("noDuplicateSearch", label=span("No Duplicates"),
                                                       choices="On"),
                                    # reset filters and table
                                    actionButton("reset", "Reset Filter"))
                            ),
                            
                            # shows the cards
                            mainPanel(
                                DT::dataTableOutput("preview")
                            )
                        )
               ),
               
               # deck builder
               tabPanel("Deck List",
                        DT::dataTableOutput("deckList")
               ),
               
               # informational graphs
               tabPanel("Graphs",
                        sidebarLayout(
                            sidebarPanel(
                                # all cards or just the selected cards to make info from
                                selectInput("graphSource", label=h3("Graph Source"),
                                            choices = c("All Cards", "Deck List")),
                                # type of graph to be made
                                selectInput("graphType", label=h3("Graph Type"),
                                            choices = c("Count", "Other")),
                                # give date range of cards to look at
                                dateRangeInput('cardDates',
                                               label="Cards",
                                               start=min(t$Date_Released),
                                               end=max(t$Date_Released),
                                               min=min(t$Date_Released),
                                               max=max(t$Date_Released)),
                                # which variable to look at when making the graph
                                selectInput("variableSelected", label=span("What variable to look at"),
                                            choices=var_col)
                            ),
                            # the plot
                            mainPanel(
                                plotlyOutput("plot")
                            ),
                        )
               )
    ),
)

server <- function(input, output, session) {
    
    # the full set of cards.
    data <- reactive({
        
        toReturn <- t
        
        if(!is.null(input$noDuplicateSearch)) {
            toReturn <- toReturn %>% distinct(Name_raw, .keep_all = TRUE)
        }
        
        toReturn
    })
    
    # printing the full set of cards
    output$preview <- DT::renderDataTable({
        
        # if the exclusive search filter is on with colors selected,
        # only choose cards that have ALL listed colors
        if(!is.null(input$checkColor) & !is.null(input$exclusiveSearch)) {
            
            # colors that we don't want
            anti_colors <- colors[!colors %in% input$checkColor]
            
            # regex to be built for filtering
            regex <- "^"
            
            for(i in 1:length(input$checkColor)) {
                regex <- paste(regex, "(?=.*", input$checkColor[i], ")", sep = "")
            }
            regex <- paste(regex, ".*$", sep="")
            
            # use regex to filter out cards
            return(data() %>% filter(str_detect(Color, regex) & !str_detect(Color, paste(anti_colors, collapse="|"))) %>%
                       select(Name, Card, input$checkGroup))
        }
        
        # if exclusive filter isn't checked, return all cards that have any of the colors listed
        else if(!is.null(input$checkColor)) {
            return(data() %>% filter(str_detect(Color, paste(input$checkColor, collapse="|"))) %>%
                       select(Name, Card, input$checkGroup))
        }
        
        # if no filter active, return all.
        else {
            return(data() %>% select(Name, Card, input$checkGroup))
        }
    }, escape=FALSE)
    
    deckList <- reactive({
        
        req(input$preview_rows_selected)

        t %>% slice(input$preview_rows_selected)
    })
    
    output$deckList <- DT::renderDataTable({
        
        input <- paste0("<input id='a", 1:nrow(deckList()), "' class='shiny-bound-input' type='number' max = '4' min = '1' value ='1' style='width: 50px;'>")
        
        
        cbind(Quantity=input, deckList()) %>% select(Quantity, Name)
    }, escape=FALSE)
    
    observeEvent(input$reset, {
        reset("form")
    })
    
    output$plot <- renderPlotly({
        
        var <- input$variableSelected
        
        if(input$graphSource == "Deck List") {
            plotData <- deckList()
        }
        
        else {
            plotData <- t
        }

        plotData <- plotData %>% filter(Date_Released >= input$cardDates[1] & Date_Released <= input$cardDates[2]) %>% 
            drop_na(var) %>%
            distinct(Name, .keep_all = TRUE)
        
        subtitle <- paste("From", as.character(input$cardDates[1]), "To", as.character(input$cardDates[2], sep=""))
        
        ggplot(data=plotData, aes(x=factor(get(var)))) + 
            geom_bar(stat="count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            labs(title = paste("Counts of ", var, " in Magic: The Gathering", "<br><sup>", subtitle, "</sup><br>", sep=""),
                 subtitle = "J",
                 x=paste(var),
                 y="Counts")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
