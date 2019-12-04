library(shiny)
library(ERSA)

 ui<- fluidPage(
  # Application title
  titlePanel("Linear Regression"),
  
  # CHOOSE LAYOUT
  sidebarLayout( 
    
    # DEFINE SIDEBAR PANEL  
    sidebarPanel( 
      
      # THE WIDGET
      selectInput("outcome", label = h3("Outcome"), 
                  choices = list("pm" = "pm",
                                "stator_yoke" = "stator_yoke",
                                  "ambient" = "ambient", 
                                  "coolant" = "coolant", 
                                  "i_d" = "i_d", 
                                  "i_q" = "i_q",
                                  "u_d" = "u_d",
                                  "u_q" = "u_q",
                                  "torque" = "torque",
                                  "stator_tooth" = "stator_tooth",
                                  "stator_winding" = "staotr_winding",
                                  "profile_id" = "profile_id",
                                  "motor_speed" = "motor_speed"), selected = 1),
      
      selectInput("indepvar", label = h3("Explanatory variable"),
                  choices = list("ambient" = "ambient", 
                                 "coolant" = "coolant", 
                                 "i_d" = "i_d", 
                                 "i_q" = "i_q",
                                 "u_d" = "u_d",
                                 "u_q" = "u_q",
                                 "torque" = "torque",
                                 "stator_tooth" = "stator_tooth",
                                 "stator_winding" = "staotr_winding",
                                 "profile_id" = "profile_id",
                                 "motor_speed" = "motor_speed",
                                 "pm" = "pm",
                                 "stator_yoke" = "stator_yoke"), selected = 1)
    ),
    
    # DEFINE MAIN PANEL
    mainPanel( # Draw main panel
      tabsetPanel(type = "tabs",
                  
    tabPanel("Scatterplot", plotOutput("scatterplot")),
    tabPanel("Distribution", # Plots of distributions
              fluidRow(
              column(6, plotOutput("distribution1")),
              column(6, plotOutput("distribution2")))
            ),
    tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
    tabPanel("Data", DT::dataTableOutput('tbl'))
      )
    )      
      
    ))


server <- function(input, output) {
  
  motor <- read.csv("C:/Users/reddymv/Desktop/pmsm_temperature_data.csv", nrow = 10000)
  
  output$summary <- renderPrint({
    fit <- lm(motor[,input$outcome] ~ motor[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
    
  })
  
  output$tbl = DT::renderDataTable({
    DT::datatable(motor, options = list(lengthChange = FALSE))
  })
  output$scatterplot <- renderPlot({
    plot(motor[,input$indepvar], motor[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(motor[,input$outcome] ~ motor[,input$indepvar]), col="red")
    lines(lowess(motor[,input$indepvar],motor[,input$outcome]), col="blue")
  }, height=400)
  
  output$distribution1 <- renderPlot({
    hist(motor[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  output$distribution2 <- renderPlot({
    hist(motor[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
  
}

shinyApp(ui = ui, server = server)