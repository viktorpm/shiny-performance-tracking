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
library(DT)
library(plotly)




source(file.path("load_data.R"))
source(file.path("plots_DelayComp.R"))
source(file.path("plots_SoundCateg.R"))
source(file.path("plots_summary.R"))




###################
########## UI -----
###################



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


          dateRangeInput(
            inputId = "setdate_sum",
            label = "Dates to show",
            start = max(TRAINING$date) - 56,
            end = max(TRAINING$date),
            min = min(TRAINING$date),
            max = max(TRAINING$date)
          ),


          dateInput(
            inputId = "date_to_sum",
            label = "Pick a date to summarize",
            value = TRAINING$date %>% unique() %>% max(),
            min = min(TRAINING$date),
            max = max(TRAINING$date)
          )
        ),

        mainPanel(
          width = 9,
          plotOutput(outputId = "plot_sum", height = 800),
          dataTableOutput(outputId = "table_sum")
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
              "No. done trials",
              "No. hit trials",
              "Stage tracking",
              "Missing data"
            )
          ),


          radioButtons(
            inputId = "f_options",
            label = "Filters",
            choices = c("All animals", "Experimenter", "Individual animals"),
            selected = "All animals"
          ),


          selectInput(
            inputId = "exp_select",
            label = "Select experimenter",
            choices = TRAINING$experimenter %>% unique() %>% as.vector()
          ),


          selectInput(
            inputId = "animal_select",
            label = "Select animals to show",
            choices = TRAINING$animal_id %>% unique() %>% as.vector()
          ),


          dateRangeInput(
            inputId = "setdate",
            label = "Dates to show (default: last 3 weeks)",
            start = max(TRAINING$date) - 21,
            end = max(TRAINING$date),
            min = min(TRAINING$date),
            max = max(TRAINING$date)
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
          plotOutput(outputId = "plot", height = 1000),
          dataTableOutput(outputId = "perform")
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
              "No. done trials",
              "No. hit trials",
              "Stage tracking",
              "Missing data"
            )
          ),


          radioButtons(
            inputId = "f_options_SC",
            label = "Filters",
            choices = c("All animals", "Experimenter", "Individual animals"),
            selected = "All animals"
          ),


          selectInput(
            inputId = "exp_select_SC",
            label = "Select experimenter",
            choices = TRAINING$experimenter %>% unique() %>% as.vector()
          ),



          selectInput(
            inputId = "animal_select_SC",
            label = "Select animals to show",
            choices = TRAINING$animal_id %>% unique() %>% as.vector()
          ),



          dateRangeInput(
            inputId = "setdate_SC",
            label = "Dates to show (default: last 3 weeks)",
            start = max(TRAINING$date) - 21,
            end = max(TRAINING$date),
            min = min(TRAINING$date),
            max = max(TRAINING$date)
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
          plotOutput(outputId = "plot_SC", height = 1000),
          dataTableOutput(outputId = "perform_SC")
        )
      )
    )
  )
)


###################
###### Server -----
###################

server <- function(input, output, session) {


  ### Summary panel ----
  ######################

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



  output$table_sum <- DT::renderDataTable(
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
      f_options = input$f_options,
      animal_filter = input$animal_select,
      exp = input$exp_select
    )
    # })
  })

  observe({
    if (input$f_options == "All animals") {
      disable("animal_select")
      disable("exp_select")
      hide("perform")
    }

    if (input$f_options == "Experimenter") {
      enable("exp_select")
      disable("animal_select")
      hide("perform")
    }

    if (input$f_options == "Individual animals") {
      enable("animal_select")
      disable("exp_select")
      show("perform")

      output$perform <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select,
            choice_direction == "right_trials",
            protocol == "@AthenaDelayComp",
            date >= input$setdate[1], 
            date <= input$setdate[2]
          ) %>%
          select(date, done_trials, violation_trials, hit_trials, timeoout_trials) %>% 
          mutate(sum = violation_trials + hit_trials + timeoout_trials) %>% 
          mutate(difference = done_trials-sum)
      )
    }
    
    
  })
  output$plot <- renderPlot({
    create_plot()
  })





  ### @SoundCategorization plots ----
  ###################################

  create_plot_SC <- reactive({
    plots_SoundCateg(
      plottype_SC = input$plot_type_SC,
      datelim_SC = input$setdate_SC,
      stage_filter_SC = input$stage_SC,
      animal_filter_SC = input$animal_select_SC,
      exp_SC = input$exp_select_SC,
      f_options_SC = input$f_options_SC
    )
  })


  observe({
    if (input$f_options_SC == "All animals") {
      disable("animal_select_SC")
      disable("exp_select_SC")
      hide("perform_SC")
    }

    if (input$f_options_SC == "Experimenter") {
      enable("exp_select_SC")
      disable("animal_select_SC")
      hide("perform_SC")
    }

    if (input$f_options_SC == "Individual animals") {
      enable("animal_select_SC")
      disable("exp_select_SC")
      show("perform_SC")
      
      output$perform_SC <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_SC,
            choice_direction == "right_trials",
            protocol == "@SoundCategorization",
            date >= input$setdate_SC[1], 
            date <= input$setdate_SC[2]
          ) %>%
          select(date, done_trials, violation_trials, hit_trials, timeoout_trials) %>% 
          mutate(sum = violation_trials + hit_trials + timeoout_trials) %>% 
          mutate(difference = done_trials-sum)
      )
   
    }
  })


  output$plot_SC <- renderPlot({
    create_plot_SC()
  })
}

shinyApp(ui, server)
