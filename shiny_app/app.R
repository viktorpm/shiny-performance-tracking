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
          
          selectInput(
            inputId = "animal_select",
            label = "Select animals to show",
            choices = TRAINING$animal_id %>% unique() %>% as.vector()
          ),
          
          
          
          radioButtons(
            inputId = "all_animals",
            label = "Plot all animals",
            choices = c("Yes" = T, "No" = F),
            
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
