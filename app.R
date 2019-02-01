#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require('dplyr')
require('ggplot2')

# data generation ---------------------------------------------------------

dataTypes <- c("genre", "ambiance", "sexe", "age", "horaire")

genre <- c("pop", "rock", "vintage")

ambiance <- c("triste", "joyeux", "entrainant")

sexe <- c("H", "F")

age<- c("-20","20-40","40-60","60-80","80+")

horaires <- c("00h-04h","04h-08h","08h-12h","12h-16h","16h-20h","20h-00h")

generateData<- function (size){
  return(data.frame( genre=sample(genre, size, replace=TRUE), 
                     ambiance=sample(ambiance, size, replace=TRUE),
                     sexe=sample(sexe, size, replace=TRUE),
                     age=sample(age, size, replace=TRUE),
                     horaires=sample(horaires, size, replace=TRUE)))
}

db_data <- generateData(1000)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("Analyse"),
   
  sidebarPanel(width=1,
     
     checkboxGroupInput("genre",
                        "Genre musical",
                        selected= genre,
                        choices = genre),
     checkboxGroupInput("ambiance",
                        "Ambiance",
                        selected= ambiance,
                        choices = ambiance),
     checkboxGroupInput("sexe",
                        "Sexe",
                        selected= sexe,
                        choices = sexe),
     checkboxGroupInput("age",
                        "Tranche d'age",
                        selected= age,
                        choices = age),
     checkboxGroupInput("horaires",
                        "Horaires d'écoute",
                        selected= horaires,
                        choices = horaires)
  
  ),
  
  navlistPanel(widths = c(1, 8),well=F,
   
   
   tabPanel("Par genre",

# Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
              tabPanel("Ambiance", plotOutput("genrePlot1"), dataTableOutput("genreTable1")),
              tabPanel("Sexe", plotOutput("genrePlot2"),dataTableOutput("genreTable2")),
              tabPanel("Tranche d'age", plotOutput("genrePlot3"),dataTableOutput("genreTable3")),
              tabPanel("Horaires d'écoute", plotOutput("genrePlot4"),dataTableOutput("genreTable4"))
    
    )
  )
)
,tabPanel("Par ambiance",


# Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Genre", plotOutput("ambiancePlot1"), dataTableOutput("ambianceTable1")),
                tabPanel("Sexe", plotOutput("ambiancePlot2"),dataTableOutput("ambianceTable2")),
                tabPanel("Tranche d'age", plotOutput("ambiancePlot3"),dataTableOutput("ambianceTable3")),
                tabPanel("Horaires d'écoute", plotOutput("ambiancePlot4"),dataTableOutput("ambianceTable4"))
                
    )
  )
)
,tabPanel("Par sexe",
# Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Genre", plotOutput("sexePlot1"), dataTableOutput("sexeTable1")),
                tabPanel("Ambiance", plotOutput("sexePlot2"),dataTableOutput("sexeTable2")),
                tabPanel("Tranche d'age", plotOutput("sexePlot3"),dataTableOutput("sexeTable3")),
                tabPanel("Horaires d'écoute", plotOutput("sexePlot4"),dataTableOutput("sexeTable4"))
                
    )
  )
)
,tabPanel("Par tranche d'âge",
# Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Genre", plotOutput("agePlot1"), dataTableOutput("ageTable1")),
                tabPanel("Ambiance", plotOutput("agePlot2"),dataTableOutput("ageTable2")),
                tabPanel("Sexe", plotOutput("agePlot3"),dataTableOutput("ageTable3")),
                tabPanel("Horaires d'écoute", plotOutput("agePlot4"),dataTableOutput("ageTable4"))
                
    )
  )
)
,tabPanel("Par horaires",

# Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Genre", plotOutput("horairesPlot1"), dataTableOutput("horairesTable1")),
                tabPanel("Ambiance", plotOutput("horairesPlot2"),dataTableOutput("horairesTable2")),
                tabPanel("Sexe", plotOutput("horairesPlot3"),dataTableOutput("horairesTable3")),
                tabPanel("Tranche d'age", plotOutput("horairesPlot4"),dataTableOutput("horairesTable4"))
                
    )
  )
)
)
                
   # Sidebar with a slider input for number of bins 
   
)

normalizeData <- function (frame, var1, var2){
  frame <- frame %>% group_by_at(var1) %>% mutate(Freq / sum(Freq)) 
  frame <- frame[, c(1,2,4)]
  colnames(frame)<- c(var1, var2, "freq")
  print("data normalized")
  return(frame[!(is.na(frame$freq)) , ])
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  data <- reactive({ db_data[db_data$genre %in% input$genre 
                                    & db_data$ambiance %in% input$ambiance 
                                    & db_data$sexe %in% input$sexe 
                                    & db_data$age %in% input$age 
                                    & db_data$horaires %in% input$horaires, ]})
  genreFrame1 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(genre, ambiance))),
    "genre",
    "ambiance") }) 
  genreFrame2 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(genre, sexe))),
    "genre",
    "sexe") }) 
  genreFrame3 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(genre, age))),
    "genre",
    "age") }) 
  genreFrame4 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(genre, horaires))),
    "genre",
    "horaires") }) 
  
  
  ambianceFrame1 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(ambiance, genre))),
    "ambiance",
    "genre") }) 
  ambianceFrame2 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(ambiance, sexe))),
    "ambiance",
    "sexe") }) 
  ambianceFrame3 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(ambiance, age))),
    "ambiance",
    "age") }) 
  ambianceFrame4 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(ambiance, horaires))),
    "ambiance",
    "horaires") }) 
  
  
  sexeFrame1 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(sexe, genre))),
    "sexe",
    "genre") }) 
  sexeFrame2 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(sexe, ambiance))),
    "sexe",
    "ambiance") }) 
  sexeFrame3 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(sexe, age))),
    "sexe",
    "age") }) 
  sexeFrame4 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(sexe, horaires))),
    "sexe",
    "horaires") }) 
  
  
  ageFrame1 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(age, genre))),
    "age",
    "genre") }) 
  ageFrame2 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(age, ambiance))),
    "age",
    "ambiance") }) 
  ageFrame3 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(age, sexe))),
    "age",
    "sexe") }) 
  ageFrame4 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(age, horaires))),
    "age",
    "horaires") }) 
  
  
  horairesFrame1 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(horaires, genre))),
    "horaires",
    "genre") }) 
  horairesFrame2 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(horaires, ambiance))),
    "horaires",
    "ambiance") }) 
  horairesFrame3 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(horaires, sexe))),
    "horaires",
    "sexe") }) 
  horairesFrame4 <- reactive({ normalizeData(
    as.data.frame(with(data(), table(horaires, age))),
    "horaires",
    "age") }) 
  
  
  output$genreTable1 <- renderDataTable({genreFrame1()})
  output$genreTable2 <- renderDataTable({genreFrame2()})
  output$genreTable3 <- renderDataTable({genreFrame3()})
  output$genreTable4 <- renderDataTable({genreFrame4()})
  
  output$ambianceTable1 <- renderDataTable({ambianceFrame1()})
  output$ambianceTable2 <- renderDataTable({ambianceFrame2()})
  output$ambianceTable3 <- renderDataTable({ambianceFrame3()})
  output$ambianceTable4 <- renderDataTable({ambianceFrame4()})
  
  output$sexeTable1 <- renderDataTable({sexeFrame1()})
  output$sexeTable2 <- renderDataTable({sexeFrame2()})
  output$sexeTable3 <- renderDataTable({sexeFrame3()})
  output$sexeTable4 <- renderDataTable({sexeFrame4()})
  
  output$ageTable1 <- renderDataTable({ageFrame1()})
  output$ageTable2 <- renderDataTable({ageFrame2()})
  output$ageTable3 <- renderDataTable({ageFrame3()})
  output$ageTable4 <- renderDataTable({ageFrame4()})
  
  output$horairesTable1 <- renderDataTable({horairesFrame1()})
  output$horairesTable2 <- renderDataTable({horairesFrame2()})
  output$horairesTable3 <- renderDataTable({horairesFrame3()})
  output$horairesTable4 <- renderDataTable({horairesFrame4()})
  
  
  
  
  output$genrePlot1 <- renderPlot({ ggplot()+geom_tile(data=genreFrame1(), aes(y=genre, x=ambiance, fill=freq)) })
  output$genrePlot2 <- renderPlot({ ggplot()+geom_tile(data=genreFrame2(), aes(y=genre, x=sexe, fill=freq))})
  output$genrePlot3 <- renderPlot({ ggplot()+geom_tile(data=genreFrame3(), aes(y=genre, x=age, fill=freq))})
  output$genrePlot4 <- renderPlot({ ggplot()+geom_tile(data=genreFrame4(), aes(y=genre, x=horaires, fill=freq))})
  
  output$ambiancePlot1 <- renderPlot({ ggplot()+geom_tile(data=ambianceFrame1(), aes(y=ambiance, x=genre, fill=freq)) })
  output$ambiancePlot2 <- renderPlot({ ggplot()+geom_tile(data=ambianceFrame2(), aes(y=ambiance, x=sexe, fill=freq))})
  output$ambiancePlot3 <- renderPlot({ ggplot()+geom_tile(data=ambianceFrame3(), aes(y=ambiance, x=age, fill=freq))})
  output$ambiancePlot4 <- renderPlot({ ggplot()+geom_tile(data=ambianceFrame4(), aes(y=ambiance, x=horaires, fill=freq))})
  
  output$sexePlot1 <- renderPlot({ ggplot()+geom_tile(data=sexeFrame1(), aes(y=sexe, x=genre, fill=freq)) })
  output$sexePlot2 <- renderPlot({ ggplot()+geom_tile(data=sexeFrame2(), aes(y=sexe, x=ambiance, fill=freq))})
  output$sexePlot3 <- renderPlot({ ggplot()+geom_tile(data=sexeFrame3(), aes(y=sexe, x=age, fill=freq))})
  output$sexePlot4 <- renderPlot({ ggplot()+geom_tile(data=sexeFrame4(), aes(y=sexe, x=horaires, fill=freq))})
  
  output$agePlot1 <- renderPlot({ ggplot()+geom_tile(data=ageFrame1(), aes(y=age, x=genre, fill=freq)) })
  output$agePlot2 <- renderPlot({ ggplot()+geom_tile(data=ageFrame2(), aes(y=age, x=ambiance, fill=freq))})
  output$agePlot3 <- renderPlot({ ggplot()+geom_tile(data=ageFrame3(), aes(y=age, x=sexe, fill=freq))})
  output$agePlot4 <- renderPlot({ ggplot()+geom_tile(data=ageFrame4(), aes(y=age, x=horaires, fill=freq))})
  
  output$horairesPlot1 <- renderPlot({ ggplot()+geom_tile(data=horairesFrame1(), aes(y=horaires, x=genre, fill=freq)) })
  output$horairesPlot2 <- renderPlot({ ggplot()+geom_tile(data=horairesFrame2(), aes(y=horaires, x=ambiance, fill=freq))})
  output$horairesPlot3 <- renderPlot({ ggplot()+geom_tile(data=horairesFrame3(), aes(y=horaires, x=sexe, fill=freq))})
  output$horairesPlot4 <- renderPlot({ ggplot()+geom_tile(data=horairesFrame4(), aes(y=horaires, x=age, fill=freq))})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

