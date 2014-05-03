shinyUI(fluidPage(
  titlePanel("Измерение производительности решений задачи pollutantmean()"),

#  sidebarLayout(sidebarPanel(
  fluidRow(
  column(5, wellPanel(  
     h4("Параметры")
   , sliderInput("loopnum", h5("Количество запусков"), 1, 17, 1, step=1)
   , h5("Вариант решения:")   
   , selectInput("script"
        , label = ""
        , choices = as.list(variants.list)
        , selected = NULL
        , multiple = FALSE
        , selectize = TRUE)
   , radioButtons("pollutant"
        , label = h5("Загрязнитель:")
        , choices = list("нитраты" = 1, "сульфаты" = 2)
        , selected = 1)
   , h5("Мониторы:")   
   , checkboxInput('allmons', label = "Все", value = TRUE)
   , conditionalPanel("!input.allmons"
#     , helpText("Можно выбрать несколько позиций", "удерживая ctrl или shift")   
     , selectInput("monitors"
          , label = ""
          , choices = as.list(1:332)
          , selected = NULL
          , multiple = TRUE
          , selectize = TRUE)
          )
    , actionButton("goButton"
        , "Обновить результаты"
        , icon("refresh"))
#   , submitButton("Обновить результаты", icon("refresh"))
    ))
  
#  , mainPanel(
  , column(2, wellPanel(
#      h4("Результаты")
     h4("Статистика длительности исполнения")
    , tableOutput("statistic")
    ))
#    , verbatimTextOutput("statistic")
  , column(5, wellPanel(
     h4("Выполняемый код")
    , p(htmlOutput("postlink"))
    , verbatimTextOutput("code")
    ))
  )

))