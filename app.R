library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(here)
library(glue)
library(ggplot2)
library(hrbrthemes)
library(shiny)
library(bslib)
library(ragg)
library(shinyWidgets)

local_files <- c("data1.tsv", "data2.tsv")
local_files <- local_files[file.exists(local_files)]

ui <- page_sidebar(
  # theme = bs_theme(version = 5, bootswatch = "yeti"),

  title = "Data Analysis",

  sidebar = sidebar(
    width = 500,
    # title = "input",
    selectizeInput(
      "file",
      "File",
      choices = local_files,
      options = list(
        placeholder = "Select a file",
        onInitialize = I('function() { this.setValue(""); }')
      ),
      width = "100%"
    ),
    selectInput(
      "col1",
      "Col1",
      choices = c(),
      multiple = T,
      width = "100%"
    ),
    selectInput(
      "col2",
      "Col2",
      choices = c(),
      multiple = T,
      width = "100%"
    )
    # switchInput("switch", "Sort by Median", F, labelWidth = "110px")
  ),

  page_navbar(
    # title = "Data Analysis",
    navbar_options = navbar_options(
      bg = "#1c2951", # "#1c2841",  # "#0062cc",
      underline = T
    ),

    nav_panel(
      "Histogram",
      plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 500 * 1024^2)

  observeEvent(input$file, {
    df_raw <- reactive({
      req(input$file)

      read_tsv(
        input$file,
        show_col_types = F
      )
    })

    list_col1 <- unique(df_raw()$col1)
    list_col2 <- unique(df_raw()$col2)

    freezeReactiveValue(input, "col1")
    updateSelectInput(
      session,
      "col1",
      choices = list_col1,
      selected = list_col1[1]
    )

    freezeReactiveValue(input, "col2")
    updateSelectInput(
      session,
      "col2",
      choices = list_col2,
      selected = list_col2[1]
    )

    df <- reactive({
      req(input$col1)
      req(input$col2)
      df_raw() |> filter(col1 %in% input$col1, col2 %in% input$col2)
    })

    p_hist <- reactive({
      ggplot(
        df(),
        aes(x = data, fill = col2),
      ) +
        geom_histogram(color = "white") +
        facet_grid(
          rows = vars(col1),
          cols = vars(col2)
        ) +
        theme_ipsum_rc() +
        scale_color_ipsum() +
        scale_fill_ipsum()
    })

    output$hist <- renderPlot(
      {
        p_hist()
      },
      # height = exprToFunction(150 * length(input$col1))
    )
  })
}

shinyApp(ui, server)
