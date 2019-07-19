library(shiny)
library(ggplot2)
library(ggrepel)
library(chron)
library(padr)
library(gridExtra)
library(forcats)
library(zoo)
library(bdscale)
library(tibble)
library(tidyverse)
library(R.matlab)
library(stringr)
library(purrr)






###################
########## UI -----
###################


# source(file.path("..", "shiny_app", "all_plots.R"))
# source(file.path("..", "shiny_app", "load_data.R"))

source(file.path("all_plots.R"))
source(file.path("load_data.R"))

ui <- fluidPage(
  navbarPage(
    tabPanel(
      "Delayed comparision protocol",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "plot_type",
            label = "Select plot type",
            choices = c(
              "CP duration",
              "Stage tracking",
              "No. done trials",
              "Missing data"
            )
          ),
          sliderInput(
            inputId = "setdate",
            label = "Date",
            min = min(TRAINING$date),
            max = max(TRAINING$date),
            value = c(min(TRAINING$date), max(TRAINING$date))
          ),

          checkboxGroupInput(
            inputId = "stage",
            label = "Select stages to show",
            choices = TRAINING$stage %>% unique() %>% as.vector(),
            selected = TRAINING$stage %>% unique() %>% as.vector()
          )


          # actionButton(inputId = "gobtn", label = "Draw plot")
        ),

        mainPanel(
          width = 9,
          plotOutput(outputId = "plot", height = 1000)
        )
      )
    ),

    tabPanel(
      "Delayed comparision protocol",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "plot_type",
            label = "Select plot type",
            choices = c(
              "CP duration",
              "Stage tracking",
              "No. done trials",
              "Missing data"
            )
          ),
          sliderInput(
            inputId = "setdate",
            label = "Date",
            min = min(TRAINING$date),
            max = max(TRAINING$date),
            value = c(min(TRAINING$date), max(TRAINING$date))
          ),

          checkboxGroupInput(
            inputId = "stage",
            label = "Select stages to show",
            choices = TRAINING$stage %>% unique() %>% as.vector(),
            selected = TRAINING$stage %>% unique() %>% as.vector()
          )


          # actionButton(inputId = "gobtn", label = "Draw plot")
        ),

        mainPanel(
          width = 9,
          plotOutput(outputId = "plot", height = 1000)
        )
      )
    )
  )
)


###################
###### Server -----
###################

server <- function(input, output, session) {
  create_plot <- reactive({
    # isolate({
    all_plots(
      plottype = input$plot_type,
      datelim = input$setdate,
      stage_filter = input$stage
    )
    # })
  })

  output$plot <- renderPlot({
    create_plot()
  })

  #
  #   observeEvent(input$gobtn, {
  #     output$plot <- renderPlot({
  #       create_plot()
  #     })
  #   })
}

shinyApp(ui, server)
