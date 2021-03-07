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

copyButtonJS <- "
shinyjs.copyDL = function(toCopy) {
                    var range = document.createRange();
                    let el = document.getElementById(toCopy);
                    range.selectNode(el);
                    window.getSelection().removeAllRanges(); // clear current selection
                    window.getSelection().addRange(range); // to select text
                    document.execCommand('copy');
                    window.getSelection().removeAllRanges();// to deselect
                }"

# UI
ui <- fluidPage(
    
    useShinyjs(),
    extendShinyjs(text = copyButtonJS, functions=c("copyDL")),
    
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
                                    "To build a decklist, click the row of the card you want to add. Then copy the input below and paste it into the box in the Decklist tab.",
                                    br(),
                                    h5("Note: pressing any of the filter buttons will reset the decklist text below. You can filter with the table, but make sure to do all sidebar filtering before selecting cards."),
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
                                    actionButton("reset", "Reset Filter"),
                                    htmlOutput("deckListText"),
                                    actionButton("copyDL", "Copy Decklist to Clipboard"))
                            ),
                            
                            # shows the cards
                            mainPanel(
                                DT::dataTableOutput("preview")
                            )
                        )
               ),
               
               # deck builder
               tabPanel("Deck List",
                        sidebarLayout(
                            sidebarPanel(
                                span("The decklist needs to follow the format of: (Quantity)x Card Name {MTGJson ID}, where (Quantity) is a number between 1 and 4 without the parentheses."),
                                textAreaInput("deckListText", height='300px', label=h3("Put Decklist Here"))
                            ),
                            mainPanel(
                                DT::dataTableOutput("deckList")
                            )
                        )
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
        toReturn <- cbind(toReturn, ID=(1:nrow(toReturn)))
        
        if(!is.null(input$noDuplicateSearch)) {
            toReturn <- toReturn %>% distinct(Name_raw, .keep_all = TRUE)
        }
        
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
            toReturn <- toReturn %>% filter(str_detect(Color, regex) & !str_detect(Color, paste(anti_colors, collapse="|"))) %>%
                       select(Name, Card, input$checkGroup, ID)
        }
        
        # if exclusive filter isn't checked, return all cards that have any of the colors listed
        else if(!is.null(input$checkColor)) {
            toReturn <- toReturn %>% filter(str_detect(Color, paste(input$checkColor, collapse="|"))) %>%
                       select(Name, Card, input$checkGroup, ID)
        }
        
        # if no filter active, return all.
        else {
            toReturn <-  toReturn %>% select(Name, Card, input$checkGroup, ID)
        }
        
        toReturn
    })
    
    # printing the full set of cards
    output$preview <- DT::renderDataTable({
        
        data();
    }, escape=FALSE)
    
    output$deckListText <- renderUI ({ 
        
        dl_t <- "<br><div style = 'border: 1px solid #929292; border-radius: 5px; background-color: #eaeaea; padding-left: 20px; padding-right: 20px;' id=decklist_sidebar><span style='font-size: 19px; font-weight: 700; text-decoration: underline;'>Deck List</span><br><div id=toCopy>"
        
        if(is.null(input$preview_rows_selected)) {
            return(HTML(paste(dl_t), "</div></div><br>"))
        }
        
        rows <- input$preview_rows_selected
        print(rows)
        
        
        for(i in 1:length(rows)) {
            real_ind <- data()[rows[i],]$ID
            dl_t <- paste(dl_t, "1x ", t[real_ind,]$Name_raw, " {", t[real_ind,]$Id, "}",  "<br>", sep="")
        }
        
        dl_t <- paste(dl_t, "</div></div><br>")
        
        HTML(dl_t)
        
        })

    onclick("copyDL", {
        js$copyDL('toCopy')
    })
    
    deckList <- reactive({
        
        req(input$deckListText)
        
        cardList <- input$deckListText
        cardList <- str_split(cardList, "\n")
        print(cardList)
        ids <- str_extract_all(cardList[[1]], "\\{[\\w\\d\\-]*\\}") %>% str_sub(2, -2)
        quant <- str_extract_all(cardList[[1]], "\\dx\\s") %>% str_sub(end = -3) %>% as.numeric()
        
        deckListDF_orig <- t %>% filter(Id %in% ids)
        deckListDF_new <- deckListDF_orig
        
        for(i in 1:nrow(deckListDF_orig)) {
            rows <- deckListDF_orig %>% filter(Id == ids[i]) %>% slice(rep(1:n(), each=quant[i]-1))
            print(rows)
            
            if(nrow(rows) > 0) {
                deckListDF_new <- rbind(deckListDF_new, rows)
            }
        }
        deckListDF_new %>% arrange(Name)
    })
    
    output$deckList <- DT::renderDataTable({
 
        print(colnames(deckList()))
        deckList() %>% select(Name, Color, Power, Toughness, MC, CMC, Type, Set_Name, Date_Released, Rarity)
        
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
