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
library(tsibble)




# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Data Viz"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("dashboard")),
      menuItem("SERIES DE TIEMPO", tabName = "ts", icon = icon("bar-chart-o")),
      menuItem("Modelos", tabName = "Modelos", icon = icon("table"),
               menuSubItem("Auto.Arima",tabName = "autoarima"),
               menuSubItem("Arima",tabName = "arima"),
               menuSubItem("Suavización Exponencial",tabName = "SuavExp"),
               menuSubItem("Red Neuronal",tabName = "res_n"))
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
        fluidRow(
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
            numericInput(inputId = "diff_number" ,min = 1,max = 5,value = 1,width = "150px",
                         label = "Diferencias"),
            fluidPage(
              plotlyOutput("plot_seriediff"),
              plotOutput("plot_seriediffACF"),
              textOutput("ressultados")
            )
          )
        )
      ),
      tabItem(
        # UI: ARIMA ####
        tabName = "arima",
        valueBoxOutput("info_modarima"),
        textOutput("tes_d"),
        tabBox(
          # aqui modelos
          title = "Arima",
          width =  12,
          fluidRow(
            column(
              width = 3,
              numericInput(inputId = "p" ,label = "p" ,value = 0)
            ),
            column(
              width = 3,
              numericInput(inputId = "d" ,label = "d" ,value = 0)
            ),
            column(
              width = 3,
              numericInput(inputId = "q" ,label = "q" ,value = 0)
            )
          ),
          tabPanel(
            title = "Fitted",
            plotlyOutput("model_arima_fitted")
          ),
          tabPanel(
            title = "Predicciones",
            plotlyOutput("model_arima_pred")
          )
        )
      ),
      tabItem(
        # UI: AutoArima ####
        tabName = "autoarima",
        valueBoxOutput("info_autoarima"),
        tabBox(
          # aqui modelos
          title = "Auto Arima",
          width =  12,
          numericInput("n_pred",min = 1,max = 100,label = "N° de días para la predicción: ",
                       value = 30,step = 5,width = "150px"), 
          tabPanel(
            title = "fitted",
            plotlyOutput("auto_arima_fitted")
          ),
          tabPanel(
            title = "Predicción",
            plotlyOutput("predicciones_autoArima")
          )
        ),
      ),
      
      #Ini Suavización
      tabItem(
        # UI: Suavización ####
        tabName = "SuavExp",
        valueBoxOutput("Suav_Exp"),
        tabBox(
          # aqui modelos
          title = "Suavización Exponencial",
          width =  12,
          #numericInput("n_pred",min = 1,max = 100,label = "N° de días para la predicción: ",
          #             value = 30,step = 5,width = "150px"), 
          tabPanel(
            title = "fitted",
            #plotlyOutput("auto_arima_fitted")
          ),
          tabPanel(
            title = "Predicción",
            #plotlyOutput("predicciones_autoArima")
          )
        ),
      ),
      #Fin suavización
      
      tabItem(
        # UI: res_neuronal ####
        tabName = "res_n",
        valueBoxOutput("info_red"),
        tabBox(
          # aqui modelos
          title = "Red Neuronal",
          width =  12,
          tabPanel(
            title = "Fitted",
            plotlyOutput("redes_fitted")
          ),
          tabPanel(
            title = "Predicción",
            fluidRow(
              column(
                width = 3,
                numericInput("size_red",label = "Capas" , min = 1,max = 10,value = 1)
              ),
              column(
                width = 3,
                numericInput("Rep",label = "Epocas" , min = 10,max = 100,value = 10,step = 10)
              )
            ),
            plotlyOutput("red_neuronal_predic")
          )
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
  # grafico de serie
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
  # fin grafica de serie
  
  # grafica de densidad
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
  
  # SECCION: MODELOS ####
  
  output$info_autoarima <- renderValueBox({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- auto.arima(train)
    predicciones <- forecast(model_train, h = input$n_pred)
    mse <- accuracy(predicciones$mean,  test$value )[2]
    valueBox(value = round(mse,2), subtitle = "RMSE (Root Mean Squared Error) en Test")
  })
  
  
   
   ## Auto arima ####
   output$auto_arima_fitted <- renderPlotly({

     df <- data()
     df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
     df <- df %>% select(FECHA,`REAL_CALLS`)
     
     df <- tsibble(date = df$FECHA , value = df$REAL_CALLS ) # ts(df$x1)
     df_list <- ts_split(df, sample.out = input$n_pred)
     
     train <- df_list$train
     test <-  df_list$test
     

     model_train <- auto.arima(train)
     
     df_residuales <- data.frame(fecha = train$date , 
                                 REAL_CALLS = train$value , Fitted = model_train$fitted )
     
     fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                     y = ~REAL_CALLS,type = "scatter",
                     mode = "lines", name = "Train")
     fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                       y = df_residuales$Fitted,type = "scatter", mode = "lines",
                       name = "Fitted")
     fig1
     
   })
  
  
  output$predicciones_autoArima <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    model_train <- auto.arima(train)
    predicciones <- forecast(model_train, h = input$n_pred)
    
    df_pred <- data.frame(fecha = test$date , real_calls = test$value ,
                          prediccion = predicciones$mean  )
    
    fig1 <- plot_ly(df_pred , x = ~ fecha ,
                    y = ~real_calls,type = "scatter",
                    mode = "lines", name = "Test")
    fig1 <- add_trace(fig1 , x = df_pred$fecha , 
                      y = df_pred$prediccion,type = "scatter", mode = "lines",
                      name = "prediccion")
    fig1
    
  })
  
  
  
  
  ## red neuronal ####
  output$info_red <- renderValueBox({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- forecast::nnetar(train$value ,size = input$size_red,repeats = input$Rep)
    prediction <- forecast(model_train , h = input$n_pred)
    mse <- accuracy(prediction$mean,  test$value )[2]
    valueBox(value = round(mse,2), subtitle = "RMSE (Root Mean Squared Error) en Test")
  })
  
  
  
  output$redes_fitted <- renderPlotly({
    
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    
    model_train <- forecast::nnetar(train$value ,size = input$size_red,repeats = input$Rep)
    
    df_residuales <- data.frame(fecha = train$date , 
                                REAL_CALLS = train$value , Fitted = model_train$fitted )
    
    fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                      y = df_residuales$Fitted,type = "scatter", mode = "lines",
                      name = "Fitted")
    fig1
    
  })
  
  
  
  
  
  
  
  output$red_neuronal_predic <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
  
    model_train <- forecast::nnetar(train$value ,size = input$size_red,repeats = input$Rep)
    prediction <- forecast(model_train , h = input$n_pred)
    
    df_pred <- data.frame(fecha = test$date , 
                                REAL_CALLS = test$value , prediction = prediction$mean)
    
    fig1 <- plot_ly(df_pred, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_pred$fecha , 
                      y = df_pred$prediction,type = "scatter", mode = "lines",
                      name = "Fitted")
    fig1
    
  })
  
  
  ## Arima ####   
  
  output$tes_d <- renderPrint({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    
    
    # Realizar la diferencia de la serie y eliminar valores NA
    if(input$d == 0){
      df_proyect_diff <- df$REAL_CALLS
      
    }else{
      df_proyect_diff <- na.omit(diff(df$REAL_CALLS, differences = input$d))
    }
    
    
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
  
  
  output$info_modarima <- renderValueBox({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- forecast::Arima(ts(train$value ,frequency = 7),order = c(p,d,q) )
    prediction <- forecast(model_train , h = input$n_pred)
    mse <- accuracy(prediction$mean,  test$value )[2]
    valueBox(value = round(mse,2), subtitle = "RMSE (Root Mean Squared Error) en Test")
  })
  
  
  
  output$model_arima_fitted <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    p <- input$p
    d <- input$d  
    q <- input$q
    P <- input$P
    D <- input$D
    Q <- input$Q
    model_train <- forecast::Arima(ts(train$value ,frequency = 7),
                                   order = c(p,d,q)  )
    
    #title <- paste0("MOdelo Arima :", "(",p,",", d , ",",q   )
    df_residuales <- data.frame(fecha = train$date , 
                          REAL_CALLS = train$value , Fitted = model_train$fitted )
    
    fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                      y = df_residuales$Fitted,type = "scatter", mode = "lines",
                      name = "Fitted")
    fig1
    
  })
  
  
  output$model_arima_pred <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    p <- input$p
    d <- input$d  
    q <- input$q
    
    
    model_train <- forecast::Arima(ts(train$value ,frequency = 7),order = c(p,d,q) )
    
    prediction <- forecast(model_train , h = input$n_pred)
    df_pred <- data.frame(fecha = test$date , 
                          REAL_CALLS = test$value , prediction = prediction$mean)
    
    fig1 <- plot_ly(df_pred, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Test")
    fig1 <- add_trace(fig1 , x = df_pred$fecha , 
                      y = df_pred$prediction,type = "scatter", mode = "lines",
                      name = "Prediccion")
    fig1
    
  })
  
  
  

}

shinyApp(ui, server)