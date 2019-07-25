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

source(file.path("load_data.R"))
source(file.path("plots_DelayComp.R"))
source(file.path("plots_SoundCateg.R"))





ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Protocols",
    
    ### @AthenaDelayComp panel ----
    ###############################
    
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
            label = "Date (default: last 3 weeks)",
            min = min(TRAINING$date),
            max = max(TRAINING$date),
            value = c(max(TRAINING$date) - 21, max(TRAINING$date))
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

    
    
    
    ### @SoundCategorization panel ----
    ###################################
    
    tabPanel(
      "@SoundCategorization",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "plot_type_SC",
            label = "Select plot type",
            choices = c(
              "CP duration",
              "Stage tracking",
              "No. done trials",
              "Missing data"
            )
          ),


          radioButtons(
            inputId = "all_animals_SC",
            label = "Plot all animals",
            choices = c("Yes" = T, "No" = F)
          ),

          selectInput(
            inputId = "animal_select_SC",
            label = "Select animals to show",
            choices = TRAINING$animal_id %>% unique() %>% as.vector()
          ),


          sliderInput(
            inputId = "setdate_SC",
            label = "Date (default: last 3 weeks)",
            min = min(TRAINING$date),
            max = max(TRAINING$date),
            value = c(max(TRAINING$date) - 21, max(TRAINING$date))
          ),

          checkboxGroupInput(
            inputId = "stage_SC",
            label = "Select stages to show",
            choices = TRAINING$stage %>% unique() %>% as.vector(),
            selected = TRAINING$stage %>% unique() %>% as.vector()
          )
        ),

        mainPanel(
          width = 9,
          plotOutput(outputId = "plot_SC", height = 1000)
        )
      )
    )
  )
)


###################
###### Server -----
###################

server <- function(input, output, session) {

  ### @AthenaDelayComp plots ----
  ###############################

  create_plot <- reactive({
    # isolate({
    plots_DelayComp(
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





  ### @SoundCategorization plots ----
  ###################################

  create_plot_SC <- reactive({
    # isolate({
    plots_SoundCateg(
      plottype = input$plot_type_SC,
      datelim = input$setdate_SC,
      stage_filter = input$stage_SC,
      animal_filter = input$animal_select_SC,
      all_animals = input$all_animals_SC
    )
    # })
  })

  observe({
    if (input$all_animals_SC == T) disable("animal_select") else enable("animal_select")
  })

  output$plot_SC <- renderPlot({
    create_plot_SC()
  })
}

shinyApp(ui, server)
