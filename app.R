library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinythemes)
library(prophet)
library(tseries)
library(DT)
library(stats)

russia <- read.csv("Russia.csv", header = TRUE, row.names = 'X', sep = ',')
moldova <- read.csv("Moldova.csv", header = TRUE, row.names = 'X', sep = ',')
belarus <- read.csv("Belarus.csv", header = TRUE, row.names = 'X', sep = ',')
description <- read.csv("description.csv", header = TRUE, row.names = "X", sep = ",")
all_countries <- read.csv('all_countries.csv', header = TRUE, sep = ',')


for(i in 1:ncol(russia)) {      
    russia[ , i] <- as.numeric(as.character(russia[,i])) 
    moldova[ , i] <- as.numeric(as.character(moldova[,i])) 
    belarus[ , i] <- as.numeric(as.character(belarus[,i])) 
}
russia$year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
moldova$year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
belarus$year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)


colnames(description) <- c('5-bank asset concentration', 'Bank capital to total assets (%)', 'Bank cost to income ratio (%)', 
                           'Bank deposits to GDP(%)', 'Bank lending-deposit spread', 'Bank net interest margin (%)',  'Bank non-performing loans to gross loans (%)',  
                           'Bank regulatory capital to risk-weighted assets','Central bank assets to GDP(%)',  'Remittance inflows to GDP(%)')
colnames(russia) <- c('5-bank asset concentration', 'Bank capital to total assets (%)', 'Bank cost to income ratio (%)', 
                      'Bank deposits to GDP(%)', 'Bank lending-deposit spread', 'Bank net interest margin (%)',  'Bank non-performing loans to gross loans (%)',  
                      'Bank regulatory capital to risk-weighted assets','Central bank assets to GDP(%)',  'Remittance inflows to GDP(%)', 'year')

colnames(moldova) <- c('5-bank asset concentration', 'Bank capital to total assets (%)', 'Bank cost to income ratio (%)', 
                       'Bank deposits to GDP(%)', 'Bank lending-deposit spread', 'Bank net interest margin (%)',  'Bank non-performing loans to gross loans (%)',  
                       'Bank regulatory capital to risk-weighted assets','Central bank assets to GDP(%)',  'Remittance inflows to GDP(%)', 'year')

colnames(belarus) <- c('5-bank asset concentration', 'Bank capital to total assets (%)', 'Bank cost to income ratio (%)', 
                       'Bank deposits to GDP(%)', 'Bank lending-deposit spread', 'Bank net interest margin (%)',  'Bank non-performing loans to gross loans (%)',  
                       'Bank regulatory capital to risk-weighted assets','Central bank assets to GDP(%)',  'Remittance inflows to GDP(%)', 'year')

forecast <- predict(arima(ts(russia$`5-bank asset concentration`, frequency = 1, start = 2006, end = 2020, deltat = 12), order = c(1,1,1)), n.ahead = 5)
plot(c(2021, 2022, 2023, 2024, 2025), forecast$pred)
length(forecast$pred)

datasets <- list("Russia" = russia, "Moldova" = moldova, "Belarus" = belarus)

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Dashboard",
                           tabPanel(icon("home"),
                                    fluidRow(
                                        column(tags$img(src="WDI.png",width="300px",height="310px"),width=3),
                                        column(
                                          p("World Development Indicators - основной сборник показателей развития Всемирного банка, составленный из официально 
                                            признанных международных источников. В нем представлены самые последние и точные доступные данные о глобальном развитии, а также национальные, региональные и глобальные оценки.", 
                                            style = "text-align:center; color:black; background-color:#8cd9ff;padding:15px;border-radius:10px"),
                                          br(),
                                          p("Данные состоят из экономических показателей трех стран: Российская Федерация, Республика Беларусь, Республика Молдова. 
                                            ",style="text-align:center;color:black;background-color:#8cd9ff;padding:15px;border-radius:10px"),
                                          br(),
                                          varSelectInput("features", label = "Описание признаков", description), tableOutput("descrip"), align = "center", width = 8
                                        )
                                    ),
                                    fluidRow(column(DTOutput("RawData"),
                                                    width = 12)),
                                    
                           ),
                           tabPanel("EDA",
                                    fluidRow(
                                        column(4, align = "center", h4("Российская Федерация")),
                                        column(4, align = "center", h4("Республика Беларусь")),
                                        column(4, align = "center", h4("Республика Молдова"))
                                    ),
                                    fluidRow(
                                        column(4, align = "center", varSelectInput("variable_rus", label = "Признаки", russia), plotOutput("plotRussia"), verbatimTextOutput("eda_rus")), 
                                        column(4, align = "center", varSelectInput("variable_bel", label = "Признаки", belarus), plotOutput("plotBelarus"), verbatimTextOutput("eda_bel")),
                                        column(4, align = "center", varSelectInput("variable_mol", label = "Признаки", moldova), plotOutput("plotMoldova"), verbatimTextOutput("eda_mol"))
                                    ),
                                    fluidRow(
                                        column(4, align = "center", ), 
                                        column(4, align = "center"),
                                        column(4, align = "center")
                                    )
                           ),
                           tabPanel("Prophet",
                              fluidRow(
                                column(tags$img(src="prophet.png", width = "290px", height="200px"), width =3),
                                column(
                                  h3("Prophet - библиотека для прогнозирования данных временных рядов на 
                                    основе аддитивной модели. В ней нелинейные тренды соответствуют годовой, еженедельной и ежедневной сезонности, а также праздничным эффектам. ", 
                                     style="text-align:center;color:black;background-color:#8cd9ff;padding:15px;border-radius:10px"),
                                  align = "center", width = 8)
                                ),
                              column(3,
                                fluidRow(br()),
                                fluidRow(br()),
                                fluidRow(align = 'center', h3('Предсказанные значения'),  br()),
                                fluidRow(br()),
                                fluidRow(
                                  align = "center", h4("Выберите данные для предсказания", br())
                                ),
                                fluidRow(br()),
                                fluidRow(
                                  align = "center", selectInput("country", label = "Страна", names(datasets), br())
                                ),
                                fluidRow(
                                  align = "center", varSelectInput("feature", label = "Признаки", russia, br())
                                ),
                                fluidRow(
                                  align = "center", sliderInput("year_amount", label = "Количество лет", min = 1, max = 15, value = 1, ticks = FALSE), br()
                                ),
                              ),
                              column(9,
                                fluidRow(h4("Прогноз включает в себя предсказанное значение, а также интервал неопределенности", style="text-align:center;color:black;background-color:#8cd9ff;padding:15px;border-radius:10px"),
                                         align = "center", plotOutput("plotProphet")
                                         )
                              )
                              ),
                           tabPanel("ARIMA",
                             column(9,
                               fluidRow(
                                 h4('ARIMA является обобщением модели ARMA, чтобы также включить случай нестационарности.', 
                                    style="text-align:center;color:black;background-color:#8cd9ff;padding:10px;border-radius:5px"
                                 ), align = "center"),
                               fluidRow(
                                 plotOutput("plotArima")
                               ),
                               fluidRow(
                                 
                               )
                             ),
                             column(3,
                               fluidRow(align = "center", h4("Выберите данные для предсказания")),
                               fluidRow(align = "center", selectInput('country', label = "Страна", names(datasets))),
                               fluidRow(align = 'center', varSelectInput('feature', label = "Признаки", russia)),
                               fluidRow(align = 'center', sliderInput('years_arima', label = "Количество лет", min = 1, max = 15, value = 1, ticks = FALSE)),
                               fluidRow(align = 'center', sliderInput('p_value', label = 'Порядок AR', min = 1, max = 4, value = 1, ticks = FALSE)),
                               fluidRow(align = 'center', sliderInput('d_value', label = 'Степень различия', min = 1, max = 4, value = 1, ticks = FALSE)),
                               fluidRow(align = 'center', sliderInput('q_value', label = 'Порядок МА', min = 1, max = 4, value = 1, ticks = FALSE))
                             )
                           )
                                    
                           )
                )
server <- function(input, output, session) {
  output$RawData <- renderDT(
    datatable({
      all_countries
    },
    options = list(lengthMenu=list(c(5,10,15),c('5','10','15')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#8cd9ff', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "none",
    selection = 'single',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Year", "Country", '5-bank asset concentration', 'Bank capital to total assets (%)', 'Bank cost to income ratio (%)',
                 'Bank deposits to GDP(%)', 'Bank lending-deposit spread', 'Bank net interest margin (%)',  'Bank non-performing loans to gross loans (%)',
                 'Bank regulatory capital to risk-weighted assets','Central bank assets to GDP(%)',  'Remittance inflows to GDP(%)')
    )
  )
  
    variable_rus <- reactive({
        input$variable_rus
    })
    variable_bel <- reactive({
        input$variable_bel
    })
    variable_mol <- reactive({
        input$variable_mol
    })
    features <- reactive({ 
      input$features
    })
    output$descrip <- renderTable({ 
      description[[features()]]
    })
    output$plotRussia <- renderPlot({
        ggplot(russia, aes(year, !!variable_rus() )) + geom_line()
    }, res = 96)
    output$plotBelarus <- renderPlot({
        ggplot(belarus, aes(year, !!input$variable_bel)) + geom_line()
    }, res = 96)
    output$plotMoldova <- renderPlot({
        ggplot(moldova, aes(year, !!input$variable_mol)) + geom_line()
    }, res = 96)
    output$eda_rus <- renderPrint({
        summary(russia[[variable_rus()]])
    })
    output$eda_bel <- renderPrint({
        summary(belarus[[variable_bel()]])
    })
    output$eda_mol <- renderPrint({
        summary(moldova[[variable_mol()]])
    })
    year_amount <- reactive({ 
      input$year_amount
    })
    datasets <- list('Russia' = russia, 'Moldova' = moldova, 'Belarus' = belarus) 
    country <- reactive({    
      datasets[[input$country]]
    })
    feature <- reactive({
      input$feature
    })
    ds <- c('2006-01-01', '2007-01-01', '2008-01-01', '2009-01-01', '2010-01-01', '2011-01-01', '2012-01-01', '2013-01-01', '2014-01-01',
            '2015-01-01', '2016-01-01', '2017-01-01', '2018-01-01', '2019-01-01', '2020-01-01') 
    ds <- base::as.Date(ds, format="%Y-%m-%d", tz="UTC") 
    df <- reactive({
      setNames(data.frame(ds, country()[[feature()]]), c("ds", "y"))
    })
    m <- reactive({  
      prophet(df())
    })
    future <- reactive({ 
      make_future_dataframe(m(), periods = year_amount(), freq = 'year')
    })
    forecast <- reactive({ 
      predict(m(), future())
    })
    output$plotProphet <- renderPlot({plot(m(), forecast())}, width = 800, height = 800) 
    
    years_arima <- reactive({
      input$years_arima
    })
    
    p_value <- reactive({
      input$p_value
    })
    
    d_value <- reactive({
      input$d_value
    })
    q_value <- reactive({
      input$q_value
    })
    
    df_ts <- reactive({
      ts(country()[[feature()]], frequency = 1, start = 2006, end = 2020, deltat = 12)
    })
    
    AR_forecast <- reactive({
      predict(arima(df_ts(), order = c(p_value(),d_value(),q_value())), n.ahead = years_arima())
    })
    
    output$plotArima <- renderPlot({ 
      ts.plot(df_ts(),  xlim = c(2006, 2020 + years_arima()), xlab = 'Year', ylab = 'Feature')
      points(AR_forecast()$pred, type = 'l', col = 2, lty = 2)
    })
    
    
}
shinyApp(ui, server)


