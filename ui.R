# @author: Nataliia Romanenko


library(shinythemes)

tagList(
  navbarPage(
    theme = shinytheme("darkly"),  
    "Shiny Movies",
    tabPanel("Movie search",
             sidebarPanel(
               selectInput("title", "MOVIE TITLE:", choices=movies$title, selected = "The Matrix"),
               hr(),
               htmlOutput("poster")
             ),
             mainPanel(
               h4(textOutput("title")),
               textOutput("descr"),
               hr(),
               strong(verbatimTextOutput("info"))
             )
    ),
    tabPanel("Exploratory analysis", 
             sidebarPanel(
               h4("Trends and Connections"),
               br(), 
               # get input for scatterplot
               selectInput("yvar", "Select outcome variable:", choices=plot.vars[-2], selected = "revenue"),               
               selectInput("xvar", "Select predictor variable:", choices=plot.vars),
               checkboxInput("lm", "add Regression line", FALSE),
               hr(), hr(),
               br(), br(), br(), br(), br(), 
               hr(), hr(),
               h4("Data distribution"),
               # get input for boxplot
               selectInput("boxvar", "Select variable for boxplot:", choices=plot.vars[-2], selected = "vote_average")
             ),
             mainPanel(
               h5(textOutput("plotTitle1")),
               plotOutput("scPlot"),
               hr(),
               h5(textOutput("plotTitle2")),
               plotOutput("boxPlot")
             )
          ),
    tabPanel("Movie Genres" , 
       sidebarPanel(
         selectInput("genre", "Choose a genre:", choices=all.genres, selected = "Drama"),
         br(),
         hr()
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Frequency over time", 
              plotOutput("histGenre"),
              strong(verbatimTextOutput("genreFr"))
           ),
           tabPanel("Genres by year", br(),
              sliderInput("year", "Year:", width = "100%",
                     min = 1874, max = 2015, value = 2010),
              h4("Genres by year"),
              plotOutput("genres")
           )
         )
       )
    )
  )
)