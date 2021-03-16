unit.scale <- function(x) (x / max(x))
unit.scale2 <- function(x) (x / 150)
unit.scale3 <- function(x) (1 / x)
unit.scale4 <- function(x) (x - mean(x) / sd(x))


output$water_table <- DT::renderDataTable({
  time_ranges %>%
    dplyr::filter(
      Experimenter == input$exp_select_w,
      Animal == input$animal_select_w,
      Date >= input$setdate_w[1],
      Date <= input$setdate_w[2]
    ) %>%
    formattable::formattable(
      align = c("c", "c", "r", "c", "c", "c", "c", "l", "l"),
      list(
        `Watering (min)` = color_bar(color = "#AAD6E6", fun = unit.scale2),
        `Training (min)` = color_bar(color = "pink", fun = unit.scale2)
        # `Training` = color_tile("pink", "red")
      )
    ) %>%
    as.datatable(
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        buttons = c("copy", "csv", "excel"),
        dom = "Bfrtip"
      ),
      class = "display"
    )
})


observe({
  x_w <- time_ranges %>%
    dplyr::filter(Experimenter == input$exp_select_w) %>%
    pull(Animal) %>%
    unique()


  updateSelectInput(session,
    inputId = "animal_select_w",
    label = "Select animals to show",
    choices = x_w,
    selected = x_w[1]
  )
  
  
  # if (input$animal_select_w == "All"){
  #   output$water_table <- DT::renderDataTable({
  #     time_ranges %>%
  #       dplyr::filter(
  #         Experimenter == input$exp_select_w,
  #         Date >= input$setdate_w[1],
  #         Date <= input$setdate_w[2]
  #       ) %>%
  #       formattable::formattable(
  #         align = c("c", "c", "r", "c", "c", "c", "c", "l", "l"),
  #         list(
  #           `Watering (min)` = color_bar(color = "#AAD6E6", fun = unit.scale2),
  #           `Training (min)` = color_bar(color = "pink", fun = unit.scale2)
  #           # `Training` = color_tile("pink", "red")
  #         )
  #       ) %>%
  #       as.datatable(
  #         extensions = "Buttons",
  #         options = list(
  #           pageLength = 20,
  #           buttons = c("copy", "csv", "excel"),
  #           dom = "Bfrtip"
  #         ),
  #         class = "display"
  #       )
  #   })
  # }
  
})
