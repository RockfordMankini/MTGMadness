library(rjson)
library(lubridate)
setwd("D:/Projects/R/Magic Cards/MagicCards/MTGMadness")

cardsFromJson <- function(filename) {
  df2 <- fromJSON(file = filename)
  
  CardsDF <- data.frame(Name = character(0),
                        Number = character(0),
                        Rarity = character(0),
                        ID = character(0),
                        Multi_ID <- character(0),
                        Set = character(0),
                        CMC = character(0),
                        MC = character(0),
                        Type = character(0),
                        Color = character(0),
                        Power = character(0),
                        Toughness = character(0),
                        Date = character(0))
  
  card_data <- df2[[2]]
  for(set in 1:(length(card_data))) {
    
    setname <- card_data[[set]]$name
    setdate <- card_data[[set]]$releaseDate
    
    cards <- card_data[[set]]$cards
    if(length(cards) == 0) {
      next
    }
    
    name <- character(0)
    number <- character(0)
    rarity <- character(0)
    ID <- character(0)
    CMC <- numeric(0)
    MC <- character(0)
    type <- character(0)
    color <- character(0)
    power <- character(0)
    toughness <- character(0)
    multi_id <- character(0)
    
      for(i in 1:length(cards)) {
        #print(i)
        card <- cards[[i]]
        name[i] <- card$name
        number[i] <- card$number
        rarity[i] <- card$rarity
        ID[i] <- card$uuid
        
        
        
        if(is.null(card$identifiers$multiverseId)) {
          multi_id[i] <- NA  
        }
        
        else{
          multi_id[i] <- card$identifiers$multiverseId
        }
        
        if(is.null(card$power)) {
          power[i] <- NA
        }
        
        else {
          power[i] <- card$power
        }
        
        if(is.null(card$toughness)) {
          toughness[i] <- NA
        }
        
        else {
          toughness[i] <- card$toughness
        }
        
        if(is.null(card$manaCost)) {
          MC[i] <- NA
        }
        
        else {
          MC[i] <- card$manaCost
        }
  
        if(length(card$colorIdentity) == 0) {
          color[i] <- "Colorless"
        }
        
        else if(length(card$colorIdentity) > 1) {
          color[i] <- paste(card$colorIdentity, collapse=", ")
        }
        
        else {
          color[i] <- card$colorIdentity
        }
        
        CMC[i] <- card$convertedManaCost
        
        if(length(card$types) > 1) {
          type[i] <- paste(card$types, collapse=", ")
        }
        
        else {
          type[i] <- card$types 
        }
      }
    
      rows <- cbind(name, number, rarity, ID, multi_id, setname, CMC, MC, type, color, power, toughness, setdate)
      CardsDF <- rbind(CardsDF, rows)
  
  }
  
  return(CardsDF)
}

#f[[2]][[1]]$cards[[1]]$identifiers$multiverseId

# AS OF 3-2-2021, there should be 56,946 cards. Write down if you notice anything different.
download.file("https://mtgjson.com/api/v5/AllPrintings.json","test_2.json")
cards <- cardsFromJson("test_2.json")

#df2 <- fromJSON(file = "Prices.json")
#View(df2)

#for(i in 1:length(df2)) {
  
#  if(names(df2[i]) %in% ID) {
    
#    card <- df2[[i]]
#    print(median(unlist(card$paper$tcgplayer$retail$foil)))
 #   UUID <- append(UUID, names(df2[i]))
 #   if( !is.null(median(unlist(card$paper$tcgplayer$retail$foil))) )
 #   {
#      meanprice <- append(meanprice, median(unlist(card$paper$tcgplayer$retail$foil)))
#    }
    
#    else {
      #meanprice <- append(meanprice, 0)
#    }
    
 # }
  
#}



write.csv(cards, "cards.csv", row.names = FALSE, fileEncoding="UTF-8")
#cards[[3]]$colorIdentity
library(rsconnect)
rsconnect::deployApp(getwd(), launch.browser = F, forceUpdate = T)
getwd()
