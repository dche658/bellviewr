#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# bellviewr: An application for indirect determination of reference intervals
# using laboratory data.
#
# Copyright (C) 2022  Douglas Chesher
#   
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

library(shiny)
library(dplyr)
library(ggplot2)
library(refineR)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Indirect Reference Intervals"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("file1","Choose CSV file", 
                             accept = "csv",
                             multiple = FALSE),
          selectInput("dataColumn",
                               "Data column",
                               choices = character(0)),
          fluidRow(
            column(6,numericInput("binwidth",
                                  label = "Bin width",value = NA))
          ),
          strong("X-axis Limits"),
          fluidRow(
            column(6,numericInput("xlimLo","Low", value = NA)),
            column(6,numericInput("xlimHi","High", value = NA))
          ),
          strong("Bhattacharya Segment"),
          fluidRow(
            column(6,numericInput("bhatFrom","From", value = NA)),
            column(6,numericInput("bhatTo", "To", value = NA))
          ),
          strong("RefineR Algorithm"),
          fluidRow(
            column(12,numericInput("nbootstrap","Bootstrap N", value=1),
                   actionButton("refinerbtn","Analyse"))
          ),
        width = 3),

        # Show a plot of the generated distribution
        mainPanel(
          div(class="panel panel-default",
            div(class="panel-heading",
                h5("Data Preview")),
            div(class="panel-body",
                tableOutput("sampleData"),
                textOutput("dataDesc")
            )
          ),
          div(class="panel panel-default",
              div(class="panel-heading",
                  h5("Bhattacharya Analysis")),
              div(class="panel-body",
                  tabsetPanel(
                    tabPanel("Report",
                             fluidRow(
                               column(6,plotOutput("bhatHist")),
                               column(6,plotOutput("bhatPlot"))
                             ),
                             tableOutput("bhatTbl")
                    ),
                    tabPanel("Residuals",
                             fluidRow(
                               column(6,plotOutput("bhatResid")),
                               column(6,"")
                             ))
                  )
                
              )
          ),
          div(class="panel panel-default",
              div(class="panel-heading",
                  h5("RefineR Analysis")),
              div(class="panel-body",
                  fluidRow(
                    column(6,plotOutput("refinePlot")),
                    column(6,verbatimTextOutput("refineRpt"))
                  )
              )
          ),
        width = 9)
    ),
    # Application footer
    div(class="footer text-center", includeHTML("footer.html"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  global <- reactiveValues(bhatFit = NULL,
                           bhatDf = NULL,
                           bhatReg = NULL)

  fileData <- reactive({
    # message("Reading file data")
    req(input$file1)
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "CSV file"))
    
    df <- read.csv(file$datapath, header = TRUE)
    csvCols = colnames(df)
    if(length(csvCols)>0) {
      # message("Updating columns")
      updateSelectInput(inputId = "dataColumn",
                        choices = c("Select",csvCols),
                        selected = "Select")
    }
    # message("Read file data")
    df
  })
  
  selectedData <- reactive({
    df <- NULL
    # message(paste("Selected column ",input$dataColumn))
    if(input$dataColumn != "Select" & input$dataColumn != "") {
      data <- fileData()[,input$dataColumn]
      # message(paste("Row count",length(data)))
      df <- na.omit(data.frame(data=data, transformed=data))
      updateNumericInput(inputId = "xlimLo", value = min(df$transformed))
      updateNumericInput(inputId = "xlimHi", value = max(df$transformed))
      updateNumericInput(inputId = "binwidth", 
                         value = (max(df$transformed)-min(df$transformed))/20)
    }
    df
  })
  
  resRI <- eventReactive(input$refinerbtn, {
    res <- NULL
    if(!is.null(selectedData()) & !is.na(input$nbootstrap)) {
      dat <- selectedData()$data
      dat <- dat[!is.na(dat)]
      progress <- Progress$new( min=0, max=1)
      on.exit(progress$close())
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')
      res <- findRI(dat,
                    model="BoxCox",
                    NBootstrap=input$nbootstrap)
    }
    res
  })
  
  output$refinePlot <- renderPlot({
    plot(resRI(),showPathol = FALSE)
  })
  
  output$refineRpt <- renderPrint({
    print(resRI(), RIperc = c(0.025, 0.5, 0.975))
  })
  
  output$sampleData <- renderTable(head(fileData()))
  
  output$dataDesc <- renderText({
    txt <- NULL
    df <- selectedData()
    if(!is.null(df)) {
      txt <- paste("N =",nrow(df))
    }
    txt
  })
  
  output$bhatHist <- renderPlot({
    plt <- NULL
    df <- selectedData()
    if(!is.null(df)) {
      plt <- ggplot(df)+
        geom_histogram(aes(transformed), 
                       binwidth = input$binwidth,
                       na.rm = TRUE)+
        scale_x_continuous("Values",limits = c(input$xlimLo,input$xlimHi))+
        ylab("Count")
      if(!is.null(input$bhatFrom) & 
         !is.null(input$bhatTo) &
         !is.null(global$bhatFit)) {
        brks <- seq(from=input$xlimLo, 
                    to=input$xlimHi,
                    by=input$binwidth)
        dhist <- hist(df$transformed,
                      breaks=brks,
                      plot=FALSE)
        scaling <- max(dhist$counts)/dnorm(global$bhatFit$mu[1],
                                           mean=global$bhatFit$mu[1],
                                           sd=global$bhatFit$sigma[1])
        plt <- plt+geom_function(fun=function(x) global$bhatFit$p[1]*scaling*dnorm(x, mean = global$bhatFit$mu[1], sd = global$bhatFit$sigma[1]),
                                 color="red") 
      }
      
    }
    plt
  })
  
  output$bhatPlot <- renderPlot({
    alpha <- 0.05
    plt <- NULL
    sdf <- selectedData()
    global$bhatFit = NULL
    global$bhatDf = NULL
    global$bhatReg = NULL
    
    if(!is.na(input$xlimLo) & 
       !is.na(input$xlimHi) & 
       !is.null(sdf)) {
      brks <- seq(from=input$xlimLo, 
                    to=input$xlimHi,
                    by=input$binwidth)
      dhist <- hist(selectedData()$transformed,
                    breaks=brks,
                    plot=FALSE)
      if (any(dhist$counts==0)) {
        warning("some bin counts were zero: setting them to 0.1")
        dhist$counts[dhist$counts==0] <- 0.1
      }
      ly <- log(dhist$counts)
      dly <- diff(ly)
      df <- data.frame(xm = dhist$mids[-length(dhist$mids)],
                       ly = dly,
                       counts = dhist$counts[-length(dhist$mids)])
      h <- diff(df$xm)[1]
      
      plt <- ggplot(df)+
        geom_point(aes(x=xm,y=ly), size=2)+
        geom_hline(yintercept = 0)+
        geom_text(aes(x=xm, y=ly, label=rownames(df)),
                  nudge_x = 0.3, 
                  nudge_y = 0.05)+
        xlab("Bin midpoint")+
        ylab("Delta log(y)")
      
      from <- c(input$bhatFrom)
      to <- c(input$bhatTo)
      
      # Calculate the Gaussian parameters
      # Code authors: Dr. Daniel T. Holmes, MD and Dr. Kevin A Buhr, PhD
      if(!all(is.na(from)) & !all(is.na(to)) & length(from) == length(to)){
        mu <- rep(NA,length(from))
        sigma <- rep(NA,length(from))
        N <- rep(NA,length(from))
        p <- rep(NA,length(from))
        for(i in 1:length(from)){
          linear <- subset(df[from[i]:to[i],])
          lin.mod <- lm(ly ~ xm, data = linear, weights = linear$counts)
          plt <- plt+geom_abline(slope = lin.mod$coefficients[2],
                                 intercept = lin.mod$coefficients[1],
                                 color="red")
          lambda <- -coef(lin.mod)[1]/coef(lin.mod)[2]
          mu[i] <- lambda + h/2
          sigma[i] <- sqrt(-h/coef(lin.mod)[2] - h^2/12)
          P <- pnorm((df$xm[from[i]:to[i]] + h/2 - mu[i])/sigma[i]) -
            pnorm((df$xm[from[i]:to[i]] - h/2 - mu[i])/sigma[i])
          N[i] <- sum(df$counts[from[i]:to[i]])/sum(P)
        }
        p <- N/sum(N)
        result <- data.frame(mu,sigma, p,
                             mu + qnorm(alpha/2)*sigma,
                             mu + qnorm(1 - alpha/2)*sigma)
        names(result) <- c("mu","sigma","p","lower", "upper")
        global$bhatFit <- result
        global$bhatDf <- linear
        global$bhatReg <- lin.mod
      }
      
    }
    plt
  })
  
  output$bhatTbl <- renderTable({
    tbl <- global$bhatFit
    if(!is.null(tbl)) {
      names(tbl) <- c("Mean","SD",
                         "P","Lower Ref Limit",
                         "Upper Ref Limit")
    }
    tbl
  })
  
  output$bhatResid <- renderPlot({
    plt <- NULL
    if(!is.null(global$bhatDf) & !is.null(global$bhatReg)) {
      global$bhatDf$pred <- (global$bhatDf$xm * global$bhatReg$coefficients[2])+
        global$bhatReg$coefficients[1]
      global$bhatDf$residual <- global$bhatDf$ly - global$bhatDf$pred
      plt <- ggplot(global$bhatDf)+
        geom_point(aes(x=xm, y=residual))+
        geom_hline(yintercept = 0, color="red")+
        xlab("Bin midpoint")
    }
    plt
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
