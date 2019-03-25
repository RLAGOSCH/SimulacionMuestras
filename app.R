

library(shiny)
library(ggplot2)
library(sjstats)
library(e1071)   


ui <- fluidPage(
  

  tags$h4("Simulador de muestras aleatorias aleatorio simple sobre una población con distribuciones conocidas"),
  tags$h5(strong(em("Instruciones:"))),
  tags$p("Escoja el tipo de distribución, el tamaño de la muestra y los parametros poblacionales"),
  
  
  
  
  tabsetPanel(
    tabPanel(title = "Gaussiana",
  fluidRow(
    column(3,
           wellPanel(sliderInput(inputId = "num",
                       label = "Simulaciones",
                       value = 500, min = 1, max = 1000),
           
           
           actionButton(inputId = "go",
                        label = "Simular")
           
    )),
    
    
    column(3,
           
           sliderInput(inputId = "media",
                       label = "Media Poblacional",
                       value = 0, min = -100, max = 100),
           
           sliderInput(inputId = "var",
                       label = "Varianza Poblacional",
                       value = 1, min = 1, max = 100)
           
           
           
    )
    
    
  ),
  # outputs gaussiana 
  fluidRow(
    column(4,
           plotOutput("hist")
    ),
    column(1)
    ,    column(2,
                tableOutput("stats")
    ),
    column(1),
    column(3,
           plotOutput("box")
    )
  ) 
           ), # final del panel gausiana 
  tabPanel(title = "Exponencial",
           fluidRow(
             column(3,
                    wellPanel(
                    sliderInput(inputId = "num2",
                                label = "Simulaciones",
                                value = 500, min = 1, max = 1000),
                    
                    
                    actionButton(inputId = "go2",
                                 label = "Simular")
                    )
             ),
             
             
             column(3,
                    
                    sliderInput(inputId = "lambda",
                                label = "Lambda",
                                value = 0.1, min = 0.01, step = 0.01, max = 10)
                    

                    
                    
                    
             )
             
             
           ),
           # outputs Exponencial 
           fluidRow(
             column(4,
                    plotOutput("histExp")
             ),
             column(1)
             ,    column(2,
                         tableOutput("statsExp")
             ),
             column(1),
             column(3,
                    plotOutput("boxExpo")
             )
           )
  ), #final tab1
  
  tabPanel(title = "Uniforme",
           fluidRow(
             column(3,
                    wellPanel(sliderInput(inputId = "num3",
                                          label = "Simulaciones",
                                          value = 500, min = 1, max = 1000),
                              
                              
                              actionButton(inputId = "go3",
                                           label = "Simular")
                              
                    )),
             
             
             column(3,
                    
                    sliderInput(inputId = "minimo",
                                label = "Máximo",
                                value = 0, min = -100, max = 100),
                    
                    sliderInput(inputId = "maximo",
                                label = "MÃ¡ximo",
                                value = 1, min = -100, max = 100)
                    
                    
                    
             )
             
             
           ),
           # outputs uniforme 
           fluidRow(
             column(4,
                    plotOutput("histUni")
             ),
             column(1)
             ,    column(2,
                         tableOutput("statsUni")
             ),
             column(1),
             column(3,
                    plotOutput("boxUni")
             )
           ) 
  ),
  
  # inputs posisson
  
  tabPanel(title = "Poisson",
           fluidRow(
             column(3,
                    wellPanel(
                      sliderInput(inputId = "num4",
                                  label = "Simulaciones",
                                  value = 500, min = 1, max = 1000),
                      
                      
                      actionButton(inputId = "go4",
                                   label = "Simular")
                    )
             ),
             
             
             column(3,
                    
                    sliderInput(inputId = "lambda2",
                                label = "Lambda",
                                value = 0.1, min = 0.01, step = 0.01, max = 10)
                    
                    
                    
                    
                    
             )
             
             
           ),
           # outputs Poisson
           fluidRow(
             column(4,
                    plotOutput("histPo")
             ),
             column(1)
             ,    column(2,
                         tableOutput("statsPo")
             ),
             column(1),
             column(3,
                    plotOutput("boxPo")
             )
           )
  ),
  tabPanel(title = "Binomial",
           fluidRow(
             column(3,
                    wellPanel(sliderInput(inputId = "num5",
                                          label = "Simulaciones",
                                          value = 500, min = 1, max = 1000),
                              
                              
                              actionButton(inputId = "go5",
                                           label = "Simular")
                              
                    )),
             
             
             column(3,
                    
                    wellPanel(sliderInput(inputId = "p",
                                label = "P",
                                value = 0.5, min = 0, max = 1, step = 0.01),
                    
                    textOutput("Q"),
                    
                    sliderInput(inputId = "n",
                                label = "Ensayos (N)",
                                value = 1, min = 1, max = 500)
                               )
                    
                    
             )
             
           ),
           # outputs Binomial  
          fluidRow(
            column(4,
                   plotOutput("histBi")
            ),
            column(1)
            ,    column(2,
                        tableOutput("statsBi")
            ),
            column(1),
            column(3,
                   plotOutput("boxBi")),
            
            column(1
                   
                   
                   )
                   
              
            
            
          ) 
  ) # final Binomial
  
           ) # final de tabset 
  
  
            ) #final de funcion UI




server <- function(input,output){
  
  #simulaciÃ³n de los datos
  
  rv <-  eventReactive(input$go,
    {rnorm(input$num, mean = input$media, sd = sqrt(input$var))}
  )  

  rv2 <- eventReactive(input$go2,
                      {rexp(input$num2, input$lambda)}
  )
  
  rv3 <- eventReactive(input$go3,
                       {runif(input$num3, input$minimo, input$maximo)}
  )
  
  rv4 <- eventReactive(input$go4,
                       {rpois(input$num4, input$lambda2)}
  )
  
  rv5 <- eventReactive(input$go5,
                       {rbinom(input$num5, input$n, input$p)}
  )
  
  output$Q <-renderText({
    strQ <-as.character(1-input$p)
    strQ <- paste("Q =", strQ, sep = " ")
    
    
    print(strQ)
    })
  
  
  
  # output gaussiana
  output$hist <- renderPlot({ 
    
    gplot1 <-ggplot() + aes(rv())+ geom_histogram(binwidth=1, colour="black", fill="white") + xlab("Variable aleatoria") + ylab("Frecuencia Abosoluta")
    
    gplot1 <- gplot1 + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold"))
    
    
    gplot1 <- gplot1 + ggtitle("Histograma muestra simulada") 
    
    print(gplot1)
  })
  output$stats <- renderTable({
    
    tEsta <-as.data.frame(as.array(summary(rv())))
    
    names(tEsta) <- c("Descriptivo", "Valor")
    
    levels(tEsta$Descriptivo) <- c(levels(tEsta$Descriptivo),"Range","Variance","SD", "CV","Kurtosis","Skewness")
    
    tEsta[7,1] <- "Range"
    tEsta[7,2] <- tEsta[6,2] - tEsta[1,2]
    
    tEsta[8,1] <- "Variance"
    tEsta[8,2] <- sd(rv())^2
    
    tEsta[9,1] <- "SD"
    tEsta[9,2] <- sd(rv())
    
    tEsta[10,1] <- "CV"
    tEsta[10,2] <- cv(rv())
    
    tEsta[11,1] <- "Skewness"
    tEsta[11,2] <- skewness(rv())
    
    tEsta[12,1] <- "Kurtosis"
    tEsta[12,2] <- kurtosis(rv())
    
    print(tEsta)
  })   
  output$box <- renderPlot({
    
    boxggplot <-  ggplot() + aes(y = rv() ) + geom_boxplot(fill="gray") +  labs(title="Box plot",x="", y = "Variable Aleatoria")
    
    boxggplot <- boxggplot + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=14,face="bold"))
    
    
    print(boxggplot)
    
    
  })
  
  # output exponencial 
  output$histExp <- renderPlot({ 
    
    gplot1 <-ggplot() + aes(rv2())+ geom_histogram(binwidth=1, colour="black", fill="white") + xlab("Variable aleatoria") + ylab("Frecuencia Abosoluta")
    
    gplot1 <- gplot1 + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold"))
    
    
    gplot1 <- gplot1 + ggtitle("Histograma muestra simulada") 
    
    print(gplot1)
  })
  output$statsExp <- renderTable({
    
    tEsta <-as.data.frame(as.array(summary(rv2())))
    
    names(tEsta) <- c("Descriptivo", "Valor")
    
    levels(tEsta$Descriptivo) <- c(levels(tEsta$Descriptivo),"Range","Variance","SD", "CV","Kurtosis","Skewness")
    
    tEsta[7,1] <- "Range"
    tEsta[7,2] <- tEsta[6,2] - tEsta[1,2]
    
    tEsta[8,1] <- "Variance"
    tEsta[8,2] <- sd(rv2())^2
    
    tEsta[9,1] <- "SD"
    tEsta[9,2] <- sd(rv2())
    
    tEsta[10,1] <- "CV"
    tEsta[10,2] <- cv(rv2())
    
    tEsta[11,1] <- "Skewness"
    tEsta[11,2] <- skewness(rv2())
    
    tEsta[12,1] <- "Kurtosis"
    tEsta[12,2] <- kurtosis(rv2())
    
    print(tEsta)
  })   
  output$boxExpo <- renderPlot({
    
    boxggplot <-  ggplot() + aes(y = rv2() ) + geom_boxplot(fill="gray") +  labs(title="Box plot",x="", y = "Variable Aleatoria")
    
    boxggplot <- boxggplot + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=14,face="bold"))
    
    
    print(boxggplot)
    
    
  })
  
  # output uniforme 
  
  output$histUni <- renderPlot({ 
    
    gplot1 <-ggplot() + aes(rv3())+ geom_histogram(binwidth=1, colour="black", fill="white") + xlab("Variable aleatoria") + ylab("Frecuencia Abosoluta")
    
    gplot1 <- gplot1 + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold"))
    
    
    gplot1 <- gplot1 + ggtitle("Histograma muestra simulada") 
    
    print(gplot1)
  })
  output$statsUni <- renderTable({
    
    tEsta <-as.data.frame(as.array(summary(rv3())))
    
    names(tEsta) <- c("Descriptivo", "Valor")
    
    levels(tEsta$Descriptivo) <- c(levels(tEsta$Descriptivo),"Range","Variance","SD", "CV","Kurtosis","Skewness")
    
    tEsta[7,1] <- "Range"
    tEsta[7,2] <- tEsta[6,2] - tEsta[1,2]
    
    tEsta[8,1] <- "Variance"
    tEsta[8,2] <- sd(rv3())^2
    
    tEsta[9,1] <- "SD"
    tEsta[9,2] <- sd(rv3())
    
    tEsta[10,1] <- "CV"
    tEsta[10,2] <- cv(rv3())
    
    tEsta[11,1] <- "Skewness"
    tEsta[11,2] <- skewness(rv3())
    
    tEsta[12,1] <- "Kurtosis"
    tEsta[12,2] <- kurtosis(rv3())
    
    print(tEsta)
  })   
  output$boxUni <- renderPlot({
    
    boxggplot <-  ggplot() + aes(y = rv3() ) + geom_boxplot(fill="gray") +  labs(title="Box plot",x="", y = "Variable Aleatoria")
    
    boxggplot <- boxggplot + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=14,face="bold"))
    
    
    print(boxggplot)
    
    
  })
  
  # output poison
  
  output$histPo <- renderPlot({ 
    
    gplot1 <-ggplot() + aes( x = rv4())+ geom_bar(colour="black",fill="white") + xlab("Variable aleatoria") + ylab("")
    
    
    gplot1 <- gplot1 + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold"))
    
    
    gplot1 <- gplot1 + ggtitle("Grafico de Barras") 
    
    print(gplot1)
    
    
  })
  output$statsPo <- renderTable({
    
    tEsta <-as.data.frame(as.array(summary(rv4())))
    
    names(tEsta) <- c("Descriptivo", "Valor")
    
    levels(tEsta$Descriptivo) <- c(levels(tEsta$Descriptivo),"Range","Variance","SD", "CV","Kurtosis","Skewness")
    
    tEsta[7,1] <- "Range"
    tEsta[7,2] <- tEsta[6,2] - tEsta[1,2]
    
    tEsta[8,1] <- "Variance"
    tEsta[8,2] <- sd(rv4())^2
    
    tEsta[9,1] <- "SD"
    tEsta[9,2] <- sd(rv4())
    
    tEsta[10,1] <- "CV"
    tEsta[10,2] <- cv(rv4())
    
    tEsta[11,1] <- "Skewness"
    tEsta[11,2] <- skewness(rv4())
    
    tEsta[12,1] <- "Kurtosis"
    tEsta[12,2] <- kurtosis(rv4())
    
    print(tEsta)
  })   
  output$boxPo <- renderPlot({
    
    boxggplot <-  ggplot() + aes(y = rv4() ) + geom_boxplot(fill="gray") +  labs(title="Box plot",x="", y = "Variable Aleatoria")
    
    boxggplot <- boxggplot + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=14,face="bold"))
    
    
    print(boxggplot)
    
    
  })
  
  #Output Binomial
  output$histBi<- renderPlot({ 
    
    gplot1 <-ggplot() + aes( x = rv5())+ geom_bar(colour="black",fill="white") + xlab("Variable aleatoria") + ylab("")
    
    
    gplot1 <- gplot1 + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold"))
    
    
    gplot1 <- gplot1 + ggtitle("Grafico de Barras") 
    
    print(gplot1)
    
    
  })
  output$statsBi<- renderTable({
    
    tEsta <-as.data.frame(as.array(summary(rv5())))
    
    names(tEsta) <- c("Descriptivo", "Valor")
    
    levels(tEsta$Descriptivo) <- c(levels(tEsta$Descriptivo),"Range","Variance","SD", "CV","Kurtosis","Skewness")
    
    tEsta[7,1] <- "Range"
    tEsta[7,2] <- tEsta[6,2] - tEsta[1,2]
    
    tEsta[8,1] <- "Variance"
    tEsta[8,2] <- sd(rv5())^2
    
    tEsta[9,1] <- "SD"
    tEsta[9,2] <- sd(rv5())
    
    tEsta[10,1] <- "CV"
    tEsta[10,2] <- cv(rv5())
    
    tEsta[11,1] <- "Skewness"
    tEsta[11,2] <- skewness(rv5())
    
    tEsta[12,1] <- "Kurtosis"
    tEsta[12,2] <- kurtosis(rv5())
    
    print(tEsta)
  })   
  output$boxBi<- renderPlot({
    
    boxggplot <-  ggplot() + aes(y = rv5() ) + geom_boxplot(fill="gray") +  labs(title="Box plot",x="", y = "Variable Aleatoria")
    
    boxggplot <- boxggplot + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=14,face="bold"))
    
    
    print(boxggplot)
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)


