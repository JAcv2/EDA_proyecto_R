# Cargar bibliotecas
library(shiny)
library(shinydashboard)
library(readr)
library(lubridate)
library(plotly)
library(dplyr)
library(forecast)
library(TSstudio)
library(xts)
library(tidyverse)
library(tseries)



# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Data Viz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("dashboard")),
      menuItem("SERIES DE TIEMPO", tabName = "ts", icon = icon("bar-chart-o")),
      menuItem("Tablas", tabName = "tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "eda",
        tabBox(
          width = 12,
          fluidRow(
            column(
              width = 3,
              selectInput(inputId = "año" , label = "Año: ",
                          choices = c("Total", "2020","2021","2022","2023"),
                          selected = "Total")
            ),
            column(
              width = 3,
              selectInput(
                inputId = "grupo", label = "A grupado por:",
                choices = c("Día" = "FECHA","Mes" = "mes" , "Semana" = "semana" ),
                selected = "Día"
              )
            ),
            column(
              width = 3,
              checkboxInput(
                inputId = "tipo_grafico" ,label = "Mod Gráfico" ,
                value =  TRUE  
              )
            )
          ),
          tabPanel(
            title = "Serie de tiempo",
            plotlyOutput("grafico_ts")
          ),
          tabPanel(
            title = "Grafico densidad",
            plotlyOutput("grafico_density")
          ),
          tabPanel(
            title = "Boxplots",
            fluidRow(
              column(
                width = 4,
                plotlyOutput("boxplots_general")
              ),
              column(
                width = 8,
                plotlyOutput("boxplots")
              )
            )
          )
        )
      ),
      tabItem(
        # Serie de tiempo ####
        tabName = "ts",
        tabBox(
          width = 12,
          title = "Serie de tiempo",
          tabPanel(
            title = "Descomposicion",
            uiOutput("plot_desomposition")
          )
        ),
        box(
          width = 12,
          numericInput(inputId = "diff_number" ,min = 1,max = 5,value = 0,label = "Diferencias"),
          fluidPage(
            plotlyOutput("plot_seriediff"),
            plotOutput("plot_seriediffACF"),
            textOutput("ressultados")
          )
        )
      ),
      tabItem(
        tabName = "Modelos",
        box(
          # aqui modelos
        )
      )
    )
  )
)

# SERVER ####
server <- function(input, output) {
  # Código del servidor (puedes agregar funciones y lógica aquí)
  # Carga de data ####
  data <- reactive({
    data <- read_delim("ts_dash.csv", delim = ";", 
                              escape_double = FALSE, trim_ws = TRUE)
    data$FECHA <- dmy(data$FECHA)
    return(data)
  })

  output$grafico_ts <- renderPlotly({
    df <- data()
    #df <- data
    filtro <- input$año
    
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA)))
    if(filtro == "Total"){
      df <- df 
    }else{
      df <- df %>% filter(año == filtro) 
    }
    grupo <- input$grupo
    df <- df %>% group_by(grupo = get(grupo)) %>% 
      summarise(promedio_calls = mean(`REAL CALLS`,na.rm = T))
    
    if(filtro == "Total"){
      titulo <- "Serie Temporal Calls"   
    }else(
      titulo <- paste0("Seire Temporal calls : ", filtro )
    )
    
    
    if(input$tipo_grafico == TRUE){
      fig1 <- plot_ly(data = df , x = ~ grupo ,
                      y = ~promedio_calls , 
                      type = "scatter" , mode = "lines") %>%
        layout(xaxis = list(title = grupo),
               title = titulo, yaxis = list(title = "Promedio Calls")) 
      
    }else{
      data <- df$promedio_calls
      x <- density(data)
      df_1 <- data.frame(y = x$y , x = x$x)
      fig1 <-  plot_ly(df_1, x = ~ x , y = ~y , type = "scatter", mode= "lines")
    }
    return(fig1)
  })
  
  
  
  output$grafico_density <- renderPlotly({
    df <- data()
    #df <- data
    filtro <- input$año
    
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA)))
    if(filtro == "Total"){
      df <- df 
    }else{
      df <- df %>% filter(año == filtro) 
    }
    grupo <- input$grupo
    df <- df %>% group_by(grupo = get(grupo)) %>% 
      summarise(promedio_calls = mean(`REAL CALLS`,na.rm = T))
    
    if(filtro == "Total"){
      titulo <- "Serie Temporal Calls"   
    }else(
      titulo <- paste0("Seire Temporal calls : ", filtro )
    )
    
    

    data <- df$promedio_calls
    x <- density(data)
    df_1 <- data.frame(y = x$y , x = x$x)
    df_1 <- df_1 %>% filter(x >= 0)
    fig1 <-  plot_ly(df_1, x = ~ x , y = ~y , type = "scatter", mode= "lines")
    return(fig1)
  })
  
  
  output$boxplots <- renderPlotly({
    
    df <- data()
    filtro <- input$año
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA)))
    if(filtro == "Total"){
      df <- df 
    }else{
      df <- df %>% filter(año == filtro) 
    }

    grupo = input$grupo
    fig1 <- plot_ly(data = df , x = ~ get(grupo) ,
                    y = ~`REAL CALLS` , 
                    type = "box" )
  
  })
  
  output$boxplots_general <- renderPlotly({
    df <- data()
    filtro <- input$año
    
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA)))
    df <- df %>% filter(año == filtro)
    fig1 <- plot_ly(data = df  ,x = "REAL CALLS",
                    y = ~`REAL CALLS` , 
                    type = "box" )
  }) 
  
  data_serie <- reactive({
    df_proyect <- data()
    df_proyect <- df_proyect %>% 
      complete(FECHA = seq(min(df_proyect$FECHA) , max(df_proyect$FECHA) , "days" ))
    sum(is.na(df_proyect$`REAL CALLS`)) 
    ts_proyect <- ts(df_proyect$`REAL CALLS`,frequency=90)
  })
  
  
  # Serie de tiempo ####
  output$plot_desomposition <- renderUI({
    df <- data_serie()
    ts_decompose(df)
  })
  
  output$plot_seriediff <- renderPlotly({
    df <- data()
    diff.order = input$diff_number
    data.diff <- diff(df$`REAL CALLS`, differences = diff.order)
    data_aux <- data.frame(`REAL CALLS` = data.diff ,
                           FECHA = df$FECHA[diff.order+1:length(data.diff)] )
    title.prefix = diff.order
    # Crear gráfico de la serie de tiempo diferenciada
    fig1 <- plot_ly(data = data_aux, x = ~ FECHA ,
                    y = ~`REAL.CALLS`, 
                    type = "scatter" , mode = "lines") %>%
      layout(xaxis = list(title = "Fecha"),
             title = paste(title.prefix, "Order Differencing ACF"), yaxis = list(title = "Real Calls"))
  })
  
  
  output$plot_seriediffACF <- renderPlot({
    data <- data()
    diff.order = input$diff_number
    data.diff <- diff(data$`REAL CALLS`, differences = diff.order)
    title.prefix <- as.character(diff.order)
    Acf(data.diff, lag.max = 90, main = paste(title.prefix, "Order Differencing ACF"))

  })
  
  output$ressultados <- renderPrint({
    diff.order = input$diff_number
    # Realizar la diferencia de la serie y eliminar valores NA
    df_proyect <- data()
    df_proyect_diff <- na.omit(diff(df_proyect$`REAL CALLS`, differences = diff.order))
    
    # Realizar la prueba de Dickey-Fuller aumentada en la serie diferenciada
    dickey_fuller_result <- adf.test(df_proyect_diff)
    
    # Extracción de los resultados
    test_stat <- dickey_fuller_result$statistic
    p_value <- dickey_fuller_result$p.value
    alpha <- 0.05
    
    cat("Hipotesis:\n")
    cat("H0: Serie no estacionaria\n")
    cat("H1: Serie estacionaria\n")
    cat(sprintf("Estadistico de prueba: %.4f\n", test_stat))
    cat(sprintf("p-value: %.4f\n", p_value))
    cat(sprintf("Alpha: %f\n", alpha))
    if (p_value > alpha) {
      cat("No se rechaza H0, por lo tanto la serie no es estacionaria.\n")
    } else {
      cat("Se rechaza H0, por lo tanto la serie es estacionaria.\n")
    }
  })
  
  
  #output$modelos_estado 
  
  
}

# Crear la aplicación Shiny
shinyApp(ui, server)