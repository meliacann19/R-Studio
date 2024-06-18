library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(shinyAce)


ui<-fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(
    h3(HTML("<b>HOLT<b> EXPONENTIAL <b>SMOOTHING<b>"), 
       style = "
       font-family:'Lobster',cursive;
      color:#48D1CC;
      text-align:center")
  ),
  br(),
  navbarPage(h3(" "),
             tabPanel(h4("Data", style="color:white"), 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("data",
                                    label = "Pilih File Data",
                                    multiple = FALSE,
                                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt")),
                          checkboxInput("header", "Header", TRUE),
                          tags$hr(),
                          radioButtons("separator", 
                                       label = "Separator",
                                       choices = c(Comma=",",Semicolon=";",Tab="\t"),inline=TRUE),       
                          tags$hr(),
                          radioButtons("disp", 
                                       label = "Display",
                                       choices = c(Head = "head", All = "all"),inline=TRUE, selected = "head")),
                        
                        mainPanel(
                          h4(tags$b("Data Peramalan"),style="text-align:center"),
                          DTOutput("file"),
                          br(),
                          h5(tags$b("Catatan: pastikan format tanggal dalam bentuk %d/%m/%y !"), style="color:black")
                          )
                        )),
             
             tabPanel(h4("Summary", style="color:white"), 
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel(h5("Plot Data Time Series"), plotOutput("plot_data")),
                                      tabPanel(h5("Statistik Deskriptif"),verbatimTextOutput("statistik_deskriptif"))
                          )
                        )),
             
            tabPanel(h4("Forecasting", style="color:white"),
             sidebarLayout(
               sidebarPanel(
                 h4("alpha",style="font-family: 'Gill Sans,sans-serif';color:#008B8B;text-align:left", align="left"),
                 sliderInput("a","", 0.05,0.95,0.2, step=0.005),
                 h4("beta",style="font-family: 'Gill Sans,sans-serif';color:#008B8B;text-align:left", align="left"),
                 sliderInput("b","", 0.05,0.95,0.3, step=0.005),
                 h4("periode ramalan",style="font-family: 'Gill Sans,sans-serif';color:#008B8B;text-align:left", align="left"),
                 sliderInput("n","", 1,30,6, step=1),
                 br(),br(),
                 actionButton("hitung","HITUNG",class="btn-primary")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel(h5("Forecasting Data"), verbatimTextOutput("model_hw"), verbatimTextOutput("pred")),
                             tabPanel(h5("Akurasi Peramalan"), verbatimTextOutput("metrics"))
                 )
               )
             )
          )
  )
)

server <- function(input,output){
  
  #output tabel data
  output$file <- renderDT({
    data <- read.csv(input$data$datapath, header = input$header, sep = input$separator)
    if(input$disp == "head") { 
      return(head(data))
    }
    else {
      return(data)
    }
  })
  
  #output plot data
  output$plot_data <- renderPlot({
    req(input$data)
    file <- read.csv(input$data$datapath, header = input$header, sep = input$separator)
    file[[1]] <- as.Date(file[[1]], format="%d/%m/%Y")
    x <- colnames(file)[1]  
    y <- colnames(file)[2] 
    
    ggplot(file, aes(x = !!sym(x), y = !!sym(y))) +
      geom_line(color = "red") +
      xlab(x) + ylab(y) + 
      theme_minimal() +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=1))  #
  })
  #output statistik deskriptif
  output$statistik_deskriptif<-renderPrint({
    data<-input$data
    if(is.null(data)){return()}
    file<-read.csv(data$datapath,header = TRUE ,sep =input$separator)
    file_without_first_column <- file[,-1]
    data.ts<-ts(file_without_first_column)
    summary(data.ts)
  })
  
  #Forcasting
  observeEvent(input$hitung, {
    req(input$data)
    file <- read.csv(input$data$datapath, header = input$header, sep = input$separator)
    file$Month <- as.Date(file[[1]], format="%d/%m/%Y")  # Mengubah kolom pertama menjadi Date
    colnames(file)[1] <- "Periode"  # Beri nama kolom pertama sebagai "Periode"
    y <- colnames(file)[2]
    data.ts <- ts(file[[y]], frequency = 12)  # Sesuaikan frekuensi sesuai kebutuhan
    af <- input$a
    bt <- input$b
    hw <- HoltWinters(data.ts, alpha = af, beta = bt, gamma = FALSE)
    
    output$model_hw <- renderPrint({
      hw
    })
    
    n <- input$n
    prediksi <- predict(hw, n.ahead = n)
    
    output$pred <- renderPrint({
      prediksi
    })
    
    #Akurasi Peramalan
    output$metrics <- renderPrint({
      fitted_values <- hw$fitted[, 1]
      error_values <- data.ts - fitted_values
      MAE <- mean(abs(error_values), na.rm = TRUE)
      MSE <- mean(error_values^2, na.rm = TRUE)
      MAPE <- mean(abs((error_values / data.ts) * 100), na.rm = TRUE)
      
      cat(" Mean Absolute Error (MAE) = ", MAE, "\n")
      cat(" Mean Squared Error (MSE) = ", MSE, "\n")
      cat(" Mean Absolute Percentage Error (MAPE) = ", MAPE, "% \n")
    })
  })

}
shinyApp(ui=ui,server=server)
