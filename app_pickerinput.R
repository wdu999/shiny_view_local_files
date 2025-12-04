library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  shinyjs::useShinyjs(),

  pickerInput(
    inputId = "my_picker",
    label = "Select an option:",
    choices = c("Option A", "Option B", "Option C"),
    selected = c("Option A", "Option B", "Option C"),
    multiple = TRUE,
    options = pickerOptions(
      `actions-box` = TRUE # Enables "Select All" and "Deselect All" buttons
    )
  ),
  # actionButton("clear_choices_button", "Clear Choices")
  switchInput(inputId = "switch", value = F)
)

server <- function(input, output, session) {
  # observeEvent(input$clear_choices_button, {
  observeEvent(input$switch, {
    if (input$switch) {
      updatePickerInput(
        session = session,
        inputId = "my_picker",
        label = "Select an option:",
        choices = c("Option A", "Option B", "Option C"),
        selected = c("Option A", "Option B", "Option C"),
      )
      shinyjs::enable("my_picker")
    } else {
      updatePickerInput(
        session = session,
        inputId = "my_picker",
        label = "",
        choices = character(0), # Clears the available choices
        # choices = NULL,       # Clears the available choices
        selected = character(0) # Clears the selected values for multiple pickers
      )
      shinyjs::disable("my_picker")
    }
  })
}

shinyApp(ui, server)
