#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(shinydashboard)
source("Regression.R")
source("Spline.R")
source("Simplex.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin="green",
    dashboardHeader(title = "Generic Solvers and Simplex Method", titleWidth = 400),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Quadratic Spline Interpolation", tabName = "QSI", icon= icon("th")),
            menuItem("Polynomial Regression", tabName = "REG", icon = icon("th")),
            menuItem("Simplex Method", tabName = "SIM", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "QSI", 
                    h2("Quadratic Spline Interpolation"),
                    sidebarPanel(
                        fileInput("qsi_input", "Choose CSV File", accept =".csv"),
                        tags$hr(),
                        numericInput("degree2", "Numerical Input:", 0, min=0, max=100),
                    ),
                    mainPanel(
                        box(width=13, h4("Polynomial Function(s):"),
                            textOutput("splineOut")),
                        box(width=13, h4("Value at point given: "), textOutput("splineEst")),
                        box(width=13, h4("Initial Table:"), tableOutput("splineInit")),
                        box(width=13, h4("Final Table:"), tableOutput("splineTab"))
                    )
            ),
            tabItem(tabName = "REG", 
                    h2("Polynomial Regression"),
                    sidebarPanel(
                        fileInput("regression_input", "Choose CSV File", accept =".csv"),
                        tags$hr(),
                        numericInput("degree", "Degree:", 0, min=0, max=100),
                    ),
                    mainPanel(
                        box(width=13, h4("Polynomial Function:"),
                            textOutput("regOut")),
                        box(width=13, h4("Value of estimate: "), textOutput("regEst")),
                        box(width=13, h4("Initial Table:"), tableOutput("regInit")),
                        box(width=13, h4("Final Table:"), tableOutput("regTab")),
                    )
            ),
            tabItem(tabName = "SIM",
                    box(width=20,
                        h2("Simplex Method"),
                        h4("The two tables below shows the two main contents of the CIO's spreadsheet."),
                        h4("Table 1 contains the number of products to be shipped to each warehouse, from every plant."),
                        h4("Table 2, on the other hand, contains shipping cost from a plant to a warehouse. It also includes the total number of products needed by each warehouse."),
                        h5("Attention: Fill out tables one and two. Save after filling out all data points.")
                    ),
                    tabBox(
                        title = "User Inputs and Solution",
                        width = 20,

                        tabPanel("Table Inputs",
                                 helpText("Table: Fairway Woods Company Shipment Details"),
                                 h5("Shipping cost from plant to warehouse in USD"),
                                 rHandsontableOutput("table3"),
                                 helpText("The values above are just initializations and are subject to change by the user."),
                                 actionButton("saveBtn3", "Save")),
                        tabPanel("Solution Table",
                                 box(title = "Quantity of Products to Ship from each Plant", width=13,
                                 tableOutput("matrixTab"),
                                 h5("Initial Tableau"),
                                 tableOutput("initTab")),
                                 h5("Final Tableau"),
                                 tableOutput("finalTab")
                                
                        )
                    )
            
            )
        )
    )
)

# The following lines of code are used for the simplex method
Plants <- c( "Plant 1", "Plant 2", "Plant 3", "Demand")
Total_Shipments <- sample(1:4, replace = TRUE)
Warehouse_1 <- sample(0, replace=TRUE)
Warehouse_2 <- sample(0, replace=TRUE)
Warehouse_3 <- sample(0, replace=TRUE)
Warehouse_4 <- sample(0, replace=TRUE)
Warehouse_5 <- sample(0, replace=TRUE)
df1 <- data.frame(Total_Shipments = Total_Shipments, Warehouse_1 = Warehouse_1, Warehouse_2 = Warehouse_2, Warehouse_3 = Warehouse_3, Warehouse_4 = Warehouse_4, Warehouse_5 = Warehouse_5, row.names = Plants)

Demand1 <- sample(0, replace=TRUE)
Demand2 <- sample(0, replace=TRUE)
Demand3 <- sample(0, replace=TRUE)
Demand4 <- sample(0, replace=TRUE)
Demand5 <- sample(0, replace=TRUE)
df2 <- data.frame(Warehouse_1 = Demand1, Warehouse_2 = Demand2, Warehouse_3 = Demand3, Warehouse_4 = Demand4, Warehouse_5 = Demand5, row.names = "Demand")

df3 <- data.frame(Available_Supply = Total_Shipments, Warehouse_1 = Warehouse_1, Warehouse_2 = Warehouse_2, Warehouse_3 = Warehouse_3, Warehouse_4 = Warehouse_4, Warehouse_5 = Warehouse_5, row.names = Plants)


# Define server logic required to draw a matrix
server <- function(input, output) {

    regression = reactive({
        inFile <- input$regression_input
        
        if(is.null(inFile)) return(NULL)
        
        RegData <- read.csv(inFile$datapath, header=FALSE)
        x <- RegData[,1]
        y <- RegData[,2]
        items = list(x,y)
        
        PolynomialRegression(input$degree, items)
    })
    
    spline = reactive({
        inFile2 <- input$qsi_input
        
        if(is.null(inFile2)) return(NULL)
        
        SplineData <- read.csv(inFile2$datapath, header=FALSE)
        x1 <- SplineData[,1]
        y1 <- SplineData[,2]
        
        items2 = list(x1,y1)
        
        QuadraticSpline(input$degree2, items2)
    })
    
    
    output$splineInit <- renderTable({
        spline()$old
    })
    
    output$splineTab <- renderTable({
        spline()$augcoeffmatrix
    })
    
    output$regOut <- renderText({
        regression()$polynomial_string
    })
    
    output$splineOut <- renderText({
        spline()$strings
    })
    
    output$splineEst <- renderText({
        text2 = substring(spline()$formula, 13)
        print(text2)
        eval(parse(text = gsub("x", input$degree2, text2)))
    })
    
    output$regEst <- renderText({
        text1 = substring(regression()$polynomial_string, 13)
        print(text1)
        eval(parse(text = gsub("x", input$degree, text1)))
    })
    
    output$regTab <- renderTable({
        regression()$augcoeffmatrix
    })
    
    output$regInit <- renderTable({
        regression()$old
    })
    
    output$table <- renderRHandsontable({
        rhandsontable(df1)
    })

    output$table2 <- renderRHandsontable({
        rhandsontable(df2)
    })
    
    output$table3 <- renderRHandsontable({
        rhandsontable(df3)
    })
    
    observeEvent(input$saveBtn3,
        write.csv(hot_to_r(input$table3), file = "Regression_3.csv", row.names=TRUE),
    )
    
    # gets data from file saved 
    
    #reg2 = read.csv(file = "Regression_2.csv", header=FALSE)
    filedata <- reactive({
        reg3 = read.csv(file = "Regression_3.csv", header=TRUE)
        
        x1 = c(reg3[1:4,2])
        x2 = c(reg3[1:4,3])
        x3 = c(reg3[1:4,4])
        x4 = c(reg3[1:4,5])
        x5 = c(reg3[1:4,6])
        x6 = c(reg3[1:4,7])
        simp = list(x2,x3,x4,x5,x6,x1)
        print(simp)
        Simplex(simp)
    })
    
    tablesPrint <- reactive({
        for(i in filedata()$counter){
            output$finalTab <- renderTable({
                
                filedata()$tables[[i]]
            })
    })

    }
    output$initTab <- renderTable({
        
        filedata()$initialTableau
    })
    output$matrixTab <- renderTable({
        
        filedata()$final_matrix
    })
    output$finalTab <- renderTable({
        
        filedata()$finalTableau
    })
}

# Run the application 
shinyApp(ui = ui, server = server)