# Load requirements
library(tidyverse)
library(prodlim)
library(riskRegression)

# Load the final model outside the app as environmental variables
load(file="fgr_model_final.RData", .GlobalEnv)
times <- seq(0, 120, 1)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Predict the risk of graft nephrectomy due to graft intolerance"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "recipient_age",
                  label = "Age [yrs]:",
                  min = 18,
                  max = 79,
                  value = 50),
      sliderInput(inputId="graft_survival",
                  label="Graft survival [months]",
                  min=6,
                  max=240,
                  value=60),
      radioButtons(inputId = "rej_any",
                   label = "Any rejection after latest transplant",
                   choices= list("No" = 0, "Yes" = 1),
                   selected = 0)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "cumIncPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # load the data from user inputs ----
  dataInput <- reactive({
    data.frame(
      recipient_age = input$recipient_age,
      graft_survival = input$graft_survival,
      rej_any = input$rej_any
    ) %>%
      dplyr::mutate(hs = ifelse(recipient_age > 40, 1, 0),
                    rej_any = as.integer(rej_any))
  })

  # create a cumulative incidence plot from the user inputs
  output$cumIncPlot <- renderPlot({

    data <- data.frame(
      times=times,
      est=predict(fgr_model_final,
                  newdata=dataInput(),
                  times=times) %>% c()
    )
      
    ggplot(data=data,
           aes(x=times,
               y=est)
           ) +
      geom_step() +
      theme_classic() +
      ggtitle("") +
      xlab("Follow-up time (months)") +
      scale_x_continuous(breaks=seq(0, 120, 12)) +
      ylab("Cumulative incidence") +
      scale_y_continuous(limits=c(0, 1),
                         breaks=seq(0, 1, 0.2)) +
      theme(axis.title=element_text(size=16),
            axis.text=element_text(size=12)) +
      coord_cartesian(xlim=c(0, 120))

  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)