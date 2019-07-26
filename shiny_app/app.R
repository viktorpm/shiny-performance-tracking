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
source(file.path("plots_summary.R"))






ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Protocols",

    ### Summary panel ----
    ######################

    tabPanel(
      "Summary",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "plot_type_sum",
            label = "Select plot type",
            choices = c(
              "Stage tracking",
              "Missing data"
            )
          ),

          sliderInput(
            inputId = "setdate_sum",
            label = "Dates to show",
            min = min(TRAINING$date),
            max = max(TRAINING$date),
            value = c(max(TRAINING$date) - 56, max(TRAINING$date))
          ),


          selectInput(
            inputId = "date_to_sum",
            label = "Pick a date to summarize",
            choices = TRAINING$date %>% unique(),
            selected = TRAINING$date %>% unique() %>% max()
          )
        ),

        mainPanel(
          width = 9,
          plotOutput(outputId = "plot_sum", height = 800),
          tableOutput(outputId = "table_sum")
        )
      )
    ),



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
            label = "Dates to show (default: last 3 weeks)",
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
            label = "Dates to show (default: last 3 weeks)",
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
  create_plot_sum <- reactive({
    plots_summary(
      plottype_sum = input$plot_type_sum,
      datelim_sum = input$setdate_sum,
      pick_date = input$date_to_sum
    )
  })


  output$plot_sum <- renderPlot({
    create_plot_sum()
  })



  output$table_sum <- renderTable(
    bind_cols(
      TRAINING %>%
        dplyr::filter(
          choice_direction == "right_trials",
          date == input$date_to_sum
        ) %>%
        group_by(protocol, stage) %>%
        summarize("No. animals" = length(animal_id)),


      tmp <- TRAINING %>%
        dplyr::filter(
          choice_direction == "right_trials",
          date == input$date_to_sum
        ) %>%
        group_by(protocol, stage) %>%
        summarize("Animal names" = paste(animal_id, sep = "", collapse = ", ")) %>%
        pull("Animal names") %>%
        as.tibble() %>%
        rename("Aimal names" = value)
    )
  )



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
      plottype_SC = input$plot_type_SC,
      datelim_SC = input$setdate_SC,
      stage_filter_SC = input$stage_SC,
      animal_filter_SC = input$animal_select_SC,
      all_animals_SC = input$all_animals_SC
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
