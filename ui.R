shinyUI(fluidPage(
  titlePanel("Измерение производительности решений задачи pollutantmean()"),

  fluidRow(
  column(4, wellPanel(  
     h4("Параметры")
   , sliderInput("loopnum", h5("Количество запусков"), 3, 17, 5, step=1)
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
   , checkboxInput('allmons', label = "Все", value = FALSE)
   , conditionalPanel("!input.allmons"  
     , selectInput("monitors"
          , label = ""
          , choices = monitors.list
          , selected = "001"
          , multiple = TRUE
          , selectize = TRUE)
          )
    , actionButton("goButton"
        , "Обновить результаты"
        , icon("refresh"))
    ))
  
  , column(3, wellPanel(
     h4("Статистика длительности исполнения")
    , verbatimTextOutput("calltext") 
    , tableOutput("statistic")
    ))
  , column(5, wellPanel(
     h4("Выполняемый код")
    , p(htmlOutput("postlink"))
    , verbatimTextOutput("code")
    ))
  )

))