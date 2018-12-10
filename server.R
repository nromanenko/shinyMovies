# @author: Nataliia Romanenko


library(shiny)
library(ggplot2)

function(input, output, session) {
  
  
  ### Movie search tabPanel   ###
  
  ## Movie summary  ###
  output$info <- renderText({
    index <- match(input$title, movies$title)
    movie <- movies[index,]
    movie.info <- ""
    
    #genres, if any
    if (nchar(movie$genres) >2 ) {
      genre <- fromJSON(movie$genres) %>% select(name)
      movie.info <- paste(movie.info, "Genres:\t\t", sep = "")
      for (g in genre[[1]])
        movie.info <- paste(movie.info, g, "\n\t\t", sep = "")
    }
    
    # production companies, if any
    if (nchar(movie$production_companies) >2 ) {
      companies <- str_split(movie$production_companies, "\\},")
      cstr <- "\nProduction:\t"
      for (c in companies[[1]]) {
        name <- str_split(c, "'")
        cstr <- paste(cstr, name[[1]][4], "\n\t\t", sep = "")
      }
      movie.info <- paste(movie.info, cstr)
    }
    
    # actors, if any
    if (nchar(movie$cast) >2 ) {
      cast <- str_split(movie$cast, "'name': '")
      str <- "\nMain cast:\t"
      for (c in cast[[1]][2:min(11,length(cast[[1]]))]) {
        name <- str_split(c, "'")
        str <- paste(str, name[[1]][1], "\n\t\t", sep="" )
      }
      movie.info <- paste(movie.info, str )
    }
    
    # director, if any
    if (nchar(movie$crew) >2 ) {
      crew <- str_split(movie$crew, '\\},')
      for (c in crew[[1]]) {
        if (grepl("'job': 'Director'", c)) {
          name <- str_split(c, "'name': '")
          name2 <- str_split(name[[1]][2], "'")
          break()
        }
      }
      movie.info <- paste(movie.info, "\nDirector:\t", name2[[1]][1], sep = "" )
    }
    
    # Release Date
    if (!is.na(movie$release_date))
      movie.info <- paste(movie.info, "\nRelease Date:\t", format(movie$release_date, '%B %d, %Y'), "\n", sep = "")
    # Runtime
    if (!is.na(movie$runtime))
      movie.info <- paste(movie.info, "Runtime:\t", movie$runtime, " min.\n", sep = "")
    # Rating
    if (!is.na(movie$vote_average))
      movie.info <- paste(movie.info, "Rating: \t", movie$vote_average, "/10", sep = "")
    # budget
    if (!is.na(movie$budget))
      movie.info = paste(movie.info, "\nBudget: \t$",prettyNum(movie$budget,big.mark=",",scientific=FALSE),sep = "")
    # revenue
    if (!is.na(movie$revenue))
      movie.info = paste(movie.info, "\nRevenue:\t$", prettyNum(movie$revenue,big.mark=",",scientific=FALSE),sep = "")

    movie.info
  })
  

  ## Movie overview
  output$descr <- renderText({
    index <- match(input$title, movies$title)
    movies[index, 4]
  })
  
  ## Movie poster
  output$poster <- renderText({
    url <- movies$poster_path[match(input$title, movies$title)]
    img <-  paste('<img src="https://image.tmdb.org/t/p/w400/', url, '" width="100%">', sep='')
    img
  })
  
  ## Movie title
  output$title <- renderText({
    index <- match(input$title, movies$title)
    year <- format(movies$release_date[index], "%Y")
    paste(input$title, " (", year, ")", sep = "")
  })
  
  ### End of Movie search tabPanel   ###
  
  
  
  ### Exploratory analysis tabPanel ###
  
  ## Scatterplot output
  output$scPlot <- renderPlot({
    main <- paste("Linear regression of", input$xvar, "and", input$yvar)

    # scatterplot without regression line
    plot1 <- ggplot(movies, aes_string(input$xvar,input$yvar)) + geom_point() +
    labs(x = input$xvar, y = input$yvar) 
    
    # scatterplot with regression line  
    if (input$lm) {
      plot1 <- ggplot(movies, aes_string(input$xvar,input$yvar)) + geom_point() +
      geom_smooth(method = "lm", color = "darkred", se=FALSE) +  
      labs(x = input$xvar, y = input$yvar)       }
    
    plot(plot1)
  })
  
  ## Boxplot output
  output$boxPlot <- renderPlot({
    ggplot(movies.full, aes_string(x = "genre", y = input$boxvar, fill = "genre")) + geom_boxplot() +
      ylab(input$boxvar) + theme_classic() + theme(legend.position = "none") + coord_flip() # flip axes
  })
  
  ## Scatterplot title
  output$plotTitle1 <- renderText({
    toupper(paste(input$xvar, "and", input$yvar))
  })
  
  ## Boxplot title
  output$plotTitle2 <- renderText({
    toupper(paste(input$boxvar, "and genre"))
  })
  
  ### End of Exploratory analysis tabPanel ###
  
 
  
  ### Movie Genres tabPanel ###
  
  ## Genres by year Bar Graph
  output$genres <- renderPlot({
    # filter by year
    df <- genre.by.year[genre.by.year$year == input$year,]
    ggplot(df) + geom_col(aes(x=genre, y=count, fill = genre)) +
      labs(x = "genres", y = "Frequency") +
      # get rid of x-labels
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) 
  })
  
  ## Genre over time histogram
  output$histGenre <- renderPlot({
    df <- genre.by.year[genre.by.year$genre == input$genre,]
    main <- paste("Frequency of", input$genre, "movies over time")
    barplot(df$count, names.arg = df$year, col = "lightgreen",
             xlab = "year", ylab = "Number of movies", ylim = c(0,800),
             main = main)
  })
  
  ## Genre over time summary
  output$genreFr <- renderText({
    df <- genre.by.year[genre.by.year$genre == input$genre,]
    w <- which.max(df$count)
    paste0("The most popular year for ", input$genre, " genre was ", df$year[w], ", when ",
          max(df$count), " ", input$genre, " movies were released")
  })
  
  ### End of Movie Genres tabPanel ###

}

