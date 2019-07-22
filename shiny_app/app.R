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
library(shinyjs)





###################
########## UI -----
###################


# source(file.path("..", "shiny_app", "all_plots.R"))
# source(file.path("..", "shiny_app", "load_data.R"))

source(file.path("all_plots.R"))
source(file.path("load_data.R"))
ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Protocols",
    tabPanel(
      "@AthenaDelayComp",
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
          
          
          radioButtons(
            inputId = "all_animals",
            label = "Plot all animals",
            choices = c("Yes" = T, "No" = F)
          ),
          
          selectInput(
            inputId = "animal_select",
            label = "Select animals to show",
            choices = TRAINING$animal_id %>% unique() %>% as.vector()
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
      "@SoundCategorization",
      sidebarLayout(
        sidebarPanel(
          # width = 3,
          # selectInput(
          #   inputId = "plot_type",
          #   label = "Select plot type",
          #   choices = c(
          #     "CP duration",
          #     "Stage tracking",
          #     "No. done trials",
          #     "Missing data"
          #   )
          # ),
          # sliderInput(
          #   inputId = "setdate",
          #   label = "Date",
          #   min = min(TRAINING$date),
          #   max = max(TRAINING$date),
          #   value = c(min(TRAINING$date), max(TRAINING$date))
          # ),
          # 
          # checkboxGroupInput(
          #   inputId = "stage",
          #   label = "Select stages to show",
          #   choices = TRAINING$stage %>% unique() %>% as.vector(),
          #   selected = TRAINING$stage %>% unique() %>% as.vector()
          # )


          # actionButton(inputId = "gobtn", label = "Draw plot")
        ),

        mainPanel(
          width = 9
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
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      all_animals = input$all_animals
    )
    # })
  })
  

  observe({
    if (input$all_animals == T) disable("animal_select") else enable("animal_select")
  })
  
  

  output$plot <- renderPlot({
    create_plot()
  })

  
  
}

shinyApp(ui, server)
