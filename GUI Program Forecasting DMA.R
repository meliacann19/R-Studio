########################### PROGRAM FORECASTING ###########################
###################### DOUBLE EXPONENTIAL SMOOTHING ######################

#AKTIFKAN PACKAGES
library(shiny)
library(ggplot2)
library(DT)
library(shinyAce)

#SYNTAX UI
ui<-fluidPage(
  h2("Double Exponential Smoothing", style="
      font-family:'Lobster',cursive;
      color:#48D1CC;
      text-align:center"),
  br(),
  tabsetPanel(
    tabPanel("Input Data",
      br(),
      h4("Silahkan Upload Data",
         style="font-family: 'Gill Sans,sans-serif';
                color:#008B8B;
                text-align: center"),
      h5("Pastikan file anda hanya memuat satu kolom data yang ingin anda forcasting, agar diperoleh output yang sesuai.",style="
                color:black;
                text-align: left"),
      
      fileInput("dapatkan_file", "Choose CSV File", 
                accept = c(
                  "text/csv",
                  "test/comma-separated-values,text/plain",
                  ".csv")),
      
      radioButtons("pemisah", "Separator",
                   choices=c(Comma=",",
                             Semicolon=";",
                             Tab="\t"),
                   selected=",",inline=TRUE),
      br(),
      h4("Berikut ini adalah data anda",style="
                  font-family:'Gill Sans,sans-serif';
                  color:#008B8B;
                  text-align:center"),
      
      DTOutput("cetak_data")
    ),
    
    tabPanel("Pola Data",
      br(),
      h5("Perlu diketahui bahwa metode Double Exponential Smoothing atau Holt's Exponential Smoothing, digunakan untuk meramalkan data yang memiliki tren dan komponen musiman.", style="color:#black; text-align:left"),
      h5("Note: pola data akan terlihat setelah anda menginput data yang akan dianalisis!", style="color:blue; text-align:left"),
      br(),
      plotOutput("plot")
    ),
  
    tabPanel("Forecasting",
             sidebarLayout(
               sidebarPanel(
                 h4("Alpha",style="font-family: 'Gill Sans,sans-serif';color:#008B8B;text-align:left", align="left"),
                 sliderInput("a","", 0.05,0.95,0.2, step=0.005),
                 h4("Beta",style="font-family: 'Gill Sans,sans-serif';color:#008B8B;text-align:left", align="left"),
                 sliderInput("b","", 0.05,0.95,0.3, step=0.005),
                 h4("Periode ramalan",style="font-family: 'Gill Sans,sans-serif';color:#008B8B;text-align:left", align="left"),
                 sliderInput("n","", 1,30,6, step=1)
               ),
               
               mainPanel(
                 br(),
                 h4("Berikut hasil forecasting untuk beberapa periode kedepan",style="
                  font-family:'Gill Sans,sans-serif';
                  color:#008B8B;
                  text-align:center"),
                 h5("Note : output akan muncul setelah anda menginput data!", style="color:blue; text-align:left"),
                 verbatimTextOutput("model_hw"),
                 verbatimTextOutput("pred"),
                 br()
               )
             )
    )
  )
)

#SYNTAX SERVER
server <- function(input,output){
  #Input Data
  data_pilihan<-reactive({
    dapatkan_file<-input$dapatkan_file
    if(is.null(dapatkan_file))
      return(NULL)
    dapatkan_file<-input$dapatkan_file
    p=read.csv(dapatkan_file$datapath, sep=input$pemisah)
    return(p)
  })
  output$cetak_data<-DT::renderDT({
    ambil_data<-data_pilihan()
    DT::datatable(ambil_data)
  })
  
  #Plot Data
  output$plot <-renderPlot({
    dapatkan_file<-input$dapatkan_file
    if (is.null(dapatkan_file))
      return(NULL)
    data=ts(data_pilihan())
    ts.plot(data, xlab="time" , ylab="frequency", main="POLA DATA")
  })
  
  #Forcasting
  observe({
    dapatkan_file<-input$dapatkan_file
    if (is.null(dapatkan_file))
      return(NULL)
    data=ts(data_pilihan())
    dt=as.numeric(as.matrix(data[,1]))
    af=input$a
    bt=input$b
    data=ts(dt)
    hw=HoltWinters(data,alpha = af, beta = bt, gamma = FALSE)
    output$model_hw=renderPrint(hw)
    n=input$n
    prediksi=predict(hw, n.ahead=n)
    output$pred=renderPrint(prediksi)
  })
}
shinyApp(ui=ui,server=server)
