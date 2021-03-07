# MTGMadness
A Shiny R app for filtering through and performing statistical analysis on Magic Cards.
https://rockfordmankini.shinyapps.io/MagicCards/

None of this data belongs to me: it is the property of Wizards of the Coast. I retrieve the data from https://mtgjson.com daily.

## Files
### CardDF.R
R script that's sourced each day in order to grab the .Json file of card data from MTGJson and deploy the app. This is done so that the burden of downloading and transforming the data isn't on the server. I change this if I want to change the way the data is being retrieved and written to cards.csv.

### cards.csv
The dataset of cards.

### app.R
The main Shiny R file. While some data is transformed here due to the limitations of cards.csv, most of this file is dedicated to displaying this data.
