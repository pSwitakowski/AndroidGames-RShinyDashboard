library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggwordcloud)
library(scales)

getwd()
setwd("C:\\Users\\Piotr\\Desktop\\STUDIA_SEM6\\AWD\\PROJEKT")

# loading the data file
data = read.csv("android-games.csv")

# data head and summary
head(data)
summary(data)


# --------------------------- fixing the data ----------------------------------

# check for N/A values and omit them
sum(is.na(data))
data <- na.omit(data)

# check for outliers
summary(data)

# fixing 'installs' column
data$installs <- gsub('\\.', '', data$installs)
data$installs <- gsub(" ", "", data$installs, fixed = TRUE)
data$installs <- gsub("M", "000000", data$installs)
data$installs <- gsub("k", "000", data$installs)

data$installs = as.numeric(data$installs)

# fixing 'price' column
data$paid = factor(data$paid, levels = c('True','False'), labels = c(1,0))

# fixing 'category' column - removing 'GAME' before each category
data$category <- gsub("GAME ", "", data$category)

# --------------------- data visualization functions ---------------------------

getGamesWordcloud = function(top_how_many) {
  
  top_games = head(data, top_how_many)
  
  games_wordcloud <- ggplot(top_games, aes(label = title, size =  total.ratings, color = installs)) +
    geom_text_wordcloud_area(shape='triangle-upright', grid_size=25) +
    scale_size_area(max_size = 16) +
    theme_minimal() + ggtitle("Top Games word cloud") +
    theme(plot.title = element_text(hjust = 0.5))
  return(games_wordcloud)
}



getBestAndWorstRatedGames = function(x_star_rated_games) {
  switch(x_star_rated_games,
         '5'={
           top_rated_games = head(data[order(-data$X1.star.ratings),], 5)

           g1 <- ggplot(top_rated_games, aes(title, X1.star.ratings)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
           print(g1)
           },
         '4'={
           top_rated_games = head(data[order(-data$X2.star.ratings),], 5)

           g1 <- ggplot(top_rated_games, aes(title, X2.star.ratings)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
         },
         '3'={
           top_rated_games = head(data[order(-data$X3.star.ratings),], 5)

           g1 <- ggplot(top_rated_games, aes(title, X3.star.ratings)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
         },
         '2'={
           top_rated_games = head(data[order(-data$X4.star.ratings),], 5)

           g1 <- ggplot(top_rated_games, aes(title, X4.star.ratings)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
         },
         '1'={
           top_rated_games = head(data[order(-data$X5.star.ratings),], 5)

           g1 <- ggplot(top_rated_games, aes(title, X5.star.ratings)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
         }
  )

  return(g1)
}


getTopRatedGamesByCategory = function(game_category) {
  switch(game_category,
         'ACTION'={
           action_games = data[data$category == 'ACTION',]
           top_rated_games = head(action_games[order(-action_games$average.rating),], 5)
           
           g2 <- ggplot(top_rated_games, aes(title, average.rating)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
             scale_y_continuous(limits=c(4,5),oob = rescale_none) +
             ggtitle('ACTION')
         },
         'ADVENTURE'={
           adventure_games = data[data$category == 'ADVENTURE',]
           top_rated_games = head(adventure_games[order(-adventure_games$average.rating),], 5)
           
           g2 <- ggplot(top_rated_games, aes(title, average.rating)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
             scale_y_continuous(limits=c(4,5),oob = rescale_none) +
             ggtitle('ADVENTURE')
         },
         'ARCADE'={
           arcade_games = data[data$category == 'ARCADE',]
           top_rated_games = head(arcade_games[order(-arcade_games$average.rating),], 5)
           
           g2 <- ggplot(top_rated_games, aes(title, average.rating)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
             scale_y_continuous(limits=c(4,5),oob = rescale_none) +
             ggtitle('ARCADE')
         },
         'BOARD'={
           board_games = data[data$category == 'BOARD',]
           top_rated_games = head(board_games[order(-board_games$average.rating),], 5)
           
           g2 <- ggplot(top_rated_games, aes(title, average.rating)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
             scale_y_continuous(limits=c(4,5),oob = rescale_none) +
             ggtitle('BOARD')
         },
         'CASUAL'={
           casual_games = data[data$category == 'CASUAL',]
           top_rated_games = head(casual_games[order(-casual_games$average.rating),], 5)
           
           g2 <- ggplot(top_rated_games, aes(title, average.rating)) + geom_col() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
             scale_y_continuous(limits=c(4,5),oob = rescale_none) +
             ggtitle('CASUAL')
         }
  )
  return(g2)
}


getCountOfPaidAndFreeGames = function() {
  g2 <- ggplot(data, aes(x=factor(paid))) + geom_bar() + ggtitle("Paid and free games comparison") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "magenta")
  return(g2)
}

getGamesCountByPrice = function() {
  g3 <- ggplot(data, aes(x=factor(price))) + geom_bar() + ggtitle("Total Number of Games by Price") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "magenta")
  return(g3)
}

getRatingsCountByGameCategory = function() {
  
  g4 <- ggplot(data, aes(x=category, y=total.ratings)) + 
    geom_col() +
    ggtitle("Total ratings by game category") +
    coord_flip()

  return(g4)
}

ui <- fluidPage(
  titlePanel(h1("Most popular Android Games", align="center")),
  fluidRow(
    column(12, align="center",
           sliderTextInput(inputId = "top_how_many", label = "How many top games?", choices = data$rank[5:50], selected = data$rank[20], grid = TRUE),
           plotOutput(outputId = "wordCloud"),
           
           sliderTextInput(inputId = "x_star_rated_games", label = "Pick game rating", choices = c(1,2,3,4,5), selected = 1, grid=TRUE),
           plotOutput(outputId = "xRatedGames"),
           
           sliderTextInput(inputId = "game_category", label = "Pick Game Category", choices = c('ACTION', 'ADVENTURE', 'ARCADE', 'BOARD', 'CASUAL'), selected = 'ACTION', grid=FALSE),
           plotOutput(outputId = "gameCategories"),
           
           plotOutput(outputId = "gamesCountByPrice"),
           
           plotOutput(outputId = "paidFree"),
           
           plotOutput(outputId = "getRatingsCountByGameCategory")
           
  )
)
)

server = function(input, output) {
  output$wordCloud = renderPlot({
    getGamesWordcloud(input$top_how_many)
  })
  
  output$xRatedGames = renderPlot({
    getBestAndWorstRatedGames(input$x_star_rated_games)
  })
  
  output$gameCategories = renderPlot({
    getTopRatedGamesByCategory(input$game_category)
  })
  
  output$gamesCountByPrice = renderPlot({
    getGamesCountByPrice()
  })
  
  output$paidFree = renderPlot({
    getCountOfPaidAndFreeGames()
  })
  
  output$  getRatingsCountByGameCategory = renderPlot({
    getRatingsCountByGameCategory()
  })

  
}

shinyApp(ui=ui, server=server)

