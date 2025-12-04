library(tidyverse)
library(purrr)
library(glue)
library(ggrepel)
library(hrbrthemes)
library(reactable)
library(reactablefmtr)
library(shiny)
library(bslib)
library(htmlwidgets)
library(forcats)
library(ragg)
library(shinyWidgets)
library(shinyFiles)

ui <- page_sidebar(
  shinyjs::useShinyjs(),
  title = "Data Viewer",
  sidebar = sidebar(
    width = 600,
    shinyDirButton(
      "directory",
      "Select a Directory",
      "Please select a directory"
    ),
    verbatimTextOutput("selected_dir_path", placeholder = T),
    selectizeInput(
      "test",
      "Test",
      choices = c(),
      options = list(
        placeholder = "Select a test",
        onInitialize = I('function() { this.setValue(""); }')
      ),
      width = "100%"
    ),
    radioButtons(
      "test_id",
      "Test Id",
      choices = character(0),
      selected = character(0)
    ),
    checkboxGroupInput(
      inputId = "group",
      label = "Group",
      choices = character(0),
      selected = character(0)
    ),
    checkboxGroupInput(
      inputId = "t",
      label = "T",
      choices = character(0),
      selected = character(0)
    ),
    pickerInput(
      inputId = "index",
      label = "Index",
      choices = character(0),
      selected = character(0),
      multiple = T, # Essential for multi-selection and "Select All"
      options = list(`actions-box` = T) # Enables "Select All" and "Deselect All" buttons
    ) |>
      shinyjs::disabled(),
    pickerInput(
      inputId = "sweep_val",
      label = "",
      choices = character(0),
      selected = character(0),
      multiple = T, # Essential for multi-selection and "Select All"
      options = list(`actions-box` = T) # Enables "Select All" and "Deselect All" buttons
    ) |>
      shinyjs::disabled(),
    textOutput("selectedValue")
  ),
  page_navbar(
    navbar_options = navbar_options(
      bg = "#0062cc",
      underline = T
    ),
    nav_panel(
      title = "Box of %",
      downloadLink("download_pct_box", "Download Plot"),
      plotOutput("pct_box")
    ),
    nav_panel(
      title = "CDF of %",
      downloadLink("download_pct_cdf", "Download Plot"),
      plotOutput("pct_cdf")
    ),
    nav_panel(
      title = "Line of %",
      downloadLink("download_pct_line", "Download Plot"),
      plotOutput("pct_line")
    ),
    nav_panel(
      title = "CDF of Raw",
      downloadLink("download_raw_cdf", "Download Plot"),
      plotOutput("raw_cdf")
    ),
    nav_panel(
      title = "Line of Raw",
      downloadLink("download_raw_line", "Download Plot"),
      plotOutput("raw_line")
    ),
    nav_panel(
      "% Summary Statistics",
      downloadLink("download_pct_data", "Download Data"),
      downloadLink("download_pct_data_as_html", "Download HTML"),
      reactableOutput("pct_table")
    ),
    nav_panel(
      "Raw",
      downloadLink("download_raw_data", "Download Data"),
      downloadLink("download_raw_data_as_html", "Download HTML"),
      reactableOutput("raw_table")
    ),
  )
)

server <- function(input, output, session) {
  options(
    shiny.maxRequestSize = 500 * 1024^2,
    shiny.useragg = T
  )

  volumes = c(
    # Home = fs::path_home(),
    # "R Installation" = R.home(),
    # getVolumes()()
    root = "."
  )

  shinyDirChoose(input, "directory", roots = volumes, session = session)
  selected_dir <- reactive({
    # req(input$directory)  # Ensure a directory is selected
    req(is.list(input$directory)) # ensure that the user has made a selection before the rest of the app's logic tries to access the directory
    parseDirPath(volumes, input$directory)
  })

  # Display the selected directory path
  output$selected_dir_path <- renderPrint({
    selected_dir()
  })

  # Reactive expression to list files in the selected directory
  files_in_dir <- reactive({
    req(selected_dir())
    all_names <- tools::file_path_sans_ext(
      basename(
        tools::list_files_with_exts(selected_dir(), "tsv")
      )
    )

    all_names <- all_names[!startsWith(all_names, "_")]
    all_names
  })

  observeEvent(files_in_dir(), {
    freezeReactiveValue(input, "test")
    updateSelectizeInput(
      session,
      "test",
      choices = files_in_dir(),
      options = list(
        placeholder = "Select a test",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })

  df_raw <- reactive({
    req(input$test)
    read_tsv(
      file.path(selected_dir(), glue("{input$test}.tsv")),
      show_col_types = F
    )
  })

  unit <- reactive(unique(df_raw()$unit))
  sweep_par <- reactive(unique(df_raw()$sweep_par))
  has_par <- reactive(ifelse(all(is.na(df_raw()$sweep_par)), F, T))
  test_name <- reactive(unique(df_raw()$test_name))

  observeEvent(df_raw(), {
    test_id <- reactive(unique(df_raw()$test_id))
    freezeReactiveValue(input, "test_id")
    updateRadioButtons(
      session,
      "test_id",
      choices = test_id(),
      selected = test_id()[1]
    )
  })

  observeEvent(df_raw(), {
    group <- reactive(unique(df_raw()$group))
    freezeReactiveValue(input, "group")
    updateCheckboxGroupInput(
      session,
      "group",
      choices = group(),
      selected = group()
    )
  })

  observeEvent(df_raw(), {
    t <- reactive(unique(df_raw()$t))
    freezeReactiveValue(input, "t")
    updateCheckboxGroupInput(
      session,
      "t",
      choices = t(),
      selected = t()
    )
  })

  observeEvent(df_raw(), {
    index <- reactive(unique(df_raw()$index))
    freezeReactiveValue(input, "index")
    updatePickerInput(
      session,
      "index",
      choices = index(),
      selected = index()
    )
    shinyjs::enable("index")
  })
  # }

  observeEvent(df_raw(), {
    freezeReactiveValue(input, "sweep_val")
    if (!has_par()) {
      updatePickerInput(
        session,
        "sweep_val",
        label = "",
        choices = character(0),
        selected = character(0)
      )
      shinyjs::disable("sweep_val")
    } else {
      sweep_val <- reactive(unique(df_raw()$sweep_val))
      if (length(sweep_val()) < 4) {
        selected <- 1:length(sweep_val())
      } else {
        selected <- c(1, ceiling(length(sweep_val()) / 3), length(sweep_val()))
      }
      updatePickerInput(
        session,
        "sweep_val",
        label = "Sweep Val",
        choices = sweep_val(),
        selected = sweep_val()[selected]
      )
      shinyjs::enable("sweep_val")
    }
  })

  df <- reactive({
    df <- df_raw() |>
      filter(
        test_id %in% input$test_id,
        t %in% input$t,
        group %in% input$group,
        index %in% input$index
      )

    if (has_par()) {
      df <- df |>
        filter(sweep_val %in% input$sweep_val) |>
        rename(!!sym(sweep_par()) := "sweep_val", !!sym(unit()) := "data")
    } else {
      df <- df |> rename(!!sym(unit()) := "data")
    }
    df
  })

  df_stats <- reactive({
    if (!"%" %in% colnames(df()) || (nrow(df()) == 0)) {
      data.frame()
    } else {
      if (has_par()) {
        gps <- c("test_id", "group", sweep_par(), "t")
      } else {
        gps <- c("test_id", "group", "t")
      }
    }

    df() |>
      filter(t > 0) |>
      group_by_at(gps) |>
      summarize(
        Mean = mean(`%`, na.rm = T),
        Median = median(`%`, na.rm = T),
        Min = min(`%`, na.rm = T),
        Max = max(`%`, na.rm = T),
        "Std Dev" = sd(`%`, na.rm = T),
        Num = n(),
        .groups = "drop_last"
      ) |>
      ungroup()
  })

  p_pct_box <- reactive({
    if (nrow(df()) == 0 || (length(input$t) == 1 && input$t == 0)) {
      p <- ggplot() + theme_void()
    } else {
      p <- ggplot(
        df() |>
          filter(t > 0) |>
          mutate(t = factor(t)),
        aes(x = t, y = `%`, fill = t)
      ) +
        geom_boxplot(alpha = 0.5)
      if (has_par()) {
        p <- p +
          facet_grid(
            rows = vars(!!sym(sweep_par())),
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scale = "free_y"
          )
      } else {
        p <- p +
          facet_grid(
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scale = "free_y"
          )
      }

      p <- p +
        stat_summary(
          fun = "median",
          geom = "text",
          color = "black",
          aes(label = sprintf("%.2f", after_stat(x))),
          position = position_nudge(y = 0)
        ) +
        geom_hline(
          yintercept = c(-2, 2),
          linetype = "dashed",
          color = "firebrick"
        ) +
        labs(
          title = input$test_id,
          subtitle = test_name(),
        ) +
        theme_ipsum_rc() +
        scale_color_ft() +
        scale_fill_ft() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0))
    }
    p
  })

  output$pct_box <- renderPlot({
    p_pct_box()
  })
  output$download_pct_box <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_pct_box.png")
    },
    content = function(file) {
      agg_png(
        file,
        width = 6,
        height = ifelse(
          !has_par() || length(input$sweep_val) < 5,
          3.7,
          3.7 / 4 * length(input$sweep_val)
        ),
        units = "in",
        res = 300,
        scaling = 0.5
      )
      print(p_pct_box())
      dev.off()
    }
  )

  p_pct_cdf <- reactive({
    if (nrow(df()) == 0 || (length(input$t) == 1 && input$t == 0)) {
      p <- ggplot() + theme_void()
    } else {
      p <- ggplot(
        df() |>
          filter(t > 0) |>
          mutate(t = factor(t)),
        aes(x = `%`, color = t)
      ) +
        stat_ecdf(geom = "point", pad = F, alpha = 0.5, size = 1)
      if (has_par()) {
        p <- p +
          facet_grid(
            rows = vars(!!sym(sweep_par())),
            cols = vars(group),
            labeller = labeller(.rows = label_both),
          )
      } else {
        p <- p +
          facet_grid(
            cols = vars(group),
            labeller = labeller(.rows = label_both),
          )
      }

      p <- p +
        geom_vline(
          xintercept = c(-2, 2),
          linetype = "dashed",
          color = "firebrick"
        ) +
        labs(
          title = input$test_id,
          subtitle = test_name(),
          y = ""
        ) +
        scale_y_percent() +
        theme_ipsum_rc() +
        scale_color_ft() +
        scale_fill_ft() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0))
    }
    p
  })

  output$pct_cdf <- renderPlot({
    p_pct_cdf()
  })
  output$download_pct_cdf <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_pct_cdf.png")
    },
    content = function(file) {
      agg_png(
        file,
        width = 6,
        height = ifelse(
          !has_par() || length(input$sweep_val) < 5,
          3.7,
          3.7 / 4 * length(input$sweep_val)
        ),
        units = "in",
        res = 300,
        scaling = 0.5
      )
      print(p_pct_cdf())
      dev.off()
    }
  )

  p_pct_line <- reactive({
    if (nrow(df()) == 0 || (length(input$t) == 1 && input$t == 0)) {
      p <- ggplot() + theme_void()
    } else {
      p <- ggplot(
        df() |>
          filter(t > 0) |>
          mutate(t = factor(t)),
        aes(x = index, y = `%`, color = t)
      ) +
        geom_point()
      if (has_par()) {
        p <- p +
          facet_grid(
            rows = vars(!!sym(sweep_par())),
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scales = "free",
            space = "free_x"
          )
      } else {
        p <- p +
          facet_grid(
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scales = "free",
            space = "free_x"
          )
      }

      p <- p +
        geom_hline(
          yintercept = c(-2, 2),
          linetype = "dashed",
          color = "firebrick"
        ) +
        labs(
          title = input$test_id,
          subtitle = test_name(),
          y = ""
        ) +
        theme_ipsum_rc() +
        scale_color_ft() +
        scale_fill_ft() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0))
    }
    p
  })

  output$pct_line <- renderPlot({
    p_pct_line()
  })
  output$download_pct_line <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_pct_line.png")
    },
    content = function(file) {
      agg_png(
        file,
        width = 6,
        height = ifelse(
          !has_par() || length(input$sweep_val) < 5,
          3.7,
          3.7 / 4 * length(input$sweep_val)
        ),
        units = "in",
        res = 300,
        scaling = 0.5
      )
      print(p_pct_line())
      dev.off()
    }
  )

  BuRd <- function(x) {
    rgb(colorRamp(c("#7fb7d7", "#fc8d59"))(x), maxColorValue = 255)
  }

  pct_table <- reactive({
    if (nrow(df_stats()) == 0) {
      reactable(data.frame(test_id = character(0)))
    } else {
      min_median <- min(df_stats()$Median)
      max_median <- max(df_stats()$Median)
      reactable(
        df_stats(),
        defaultColDef = colDef(
          align = "center",
          vAlign = "center"
        ),
        columns = list(
          test_id = colDef(align = "left"),
          t = colDef(filterable = T),
          group = colDef(filterable = T),
          index = colDef(filterable = T),
          Mean = colDef(format = colFormat(digits = 2)),
          Median = colDef(
            format = colFormat(digits = 2),
            style = function(value) {
              normalized <- (value - min_median) /
                ifelse(
                  (max_median - min_median) == 0,
                  1,
                  max_median - min_median
                )
              if (normalized > 1) {
                normalized <- 1
              } else if (normalized < 0) {
                normalized <- 0
              }
              color <- BuRd(normalized)
              list(background = color)
            }
          ),
          Min = colDef(format = colFormat(digits = 2)),
          Max = colDef(format = colFormat(digits = 2)),
          `Std Dev` = colDef(format = colFormat(digits = 2))
        ),
        pagination = F,
        striped = T,
        compact = T,
        filterable = F,
        highlight = T
      )
    }
  })

  output$pct_table <- renderReactable(pct_table())
  output$download_pct_data <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_pct_summary_statistics.tsv")
    },
    content = function(file) {
      write_tsv(df_stats(), file)
    }
  )

  output$download_pct_data_as_html <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_pct_summary_statistics.html")
    },
    content = function(file) {
      saveWidget(
        pct_table() |>
          add_title(glue(
            "{input$test_id} {test_name()} % Summary Statistics"
          )) |>
          add_subtitle(glue("{input$file}")),
        file,
        selfcontained = T
      )
    }
  )

  p_raw_cdf <- reactive({
    if (nrow(df()) == 0) {
      p <- ggplot() + theme_void()
    } else {
      p <- ggplot(
        df() |>
          mutate(t = factor(t)),
        aes(x = !!sym(unit()), color = t)
      ) +
        stat_ecdf(geom = "point", pad = F, alpha = 0.5, size = 1)
      if (has_par()) {
        p <- p +
          ggh4x::facet_grid2(
            rows = vars(!!sym(sweep_par())),
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scales = "free_x",
            independent = "x"
          )
      } else {
        p <- p +
          ggh4x::facet_grid2(
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scales = "free_x",
            independent = "x"
          )
      }
      p <- p +
        labs(
          title = input$test_id,
          subtitle = test_name(),
          y = NULL,
        ) +
        theme_ipsum_rc() +
        scale_color_ft() +
        scale_fill_ft() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0))
    }
    p
  })

  output$raw_cdf <- renderPlot(
    p_raw_cdf()
  )

  output$download_raw_cdf <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_raw_cdf.png")
    },
    content = function(file) {
      agg_png(
        file,
        width = 6,
        height = ifelse(
          !has_par() || length(input$sweep_val) < 5,
          3.7,
          3.7 / 4 * length(input$sweep_val)
        ),
        units = "in",
        res = 300,
        scaling = 0.5
      )
      print(p_raw_cdf())
      dev.off()
    }
  )

  p_raw_line <- reactive({
    if (nrow(df()) == 0 || (length(input$t) == 1 && input$t == 0)) {
      p <- ggplot() + theme_void()
    } else {
      p <- ggplot(
        df() |>
          filter(t > 0) |>
          mutate(t = factor(t)),
        aes(x = index, y = !!sym(unit()), color = t)
      ) +
        geom_point()
      if (has_par()) {
        p <- p +
          facet_grid(
            rows = vars(!!sym(sweep_par())),
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scales = "free",
            space = "free_x"
          )
      } else {
        p <- p +
          facet_grid(
            cols = vars(group),
            labeller = labeller(.rows = label_both),
            scales = "free",
            space = "free_x"
          )
      }

      p <- p +
        labs(
          title = input$test_id,
          subtitle = test_name(),
        ) +
        theme_ipsum_rc() +
        scale_color_ft() +
        scale_fill_ft() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0))
    }
    p
  })

  output$raw_line <- renderPlot({
    p_raw_line()
  })
  output$download_raw_line <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_raw_line.png")
    },
    content = function(file) {
      agg_png(
        file,
        width = 6,
        height = ifelse(
          !has_par() || length(input$sweep_val) < 5,
          3.7,
          3.7 / 4 * length(input$sweep_val)
        ),
        units = "in",
        res = 300,
        scaling = 0.5
      )
      print(p_raw_line())
      dev.off()
    }
  )

  raw_table <- reactive({
    if (nrow(df()) == 0) {
      reactable(data.frame(test_id = character(0)))
    } else {
      if (has_par()) {
        selections <- c(
          "t",
          "test_name",
          "test_id",
          "group",
          "index",
          "sweep_val",
          sweep_par(),
          unit(),
          "ref",
          "delta",
          "%"
        )
      } else {
        selections <- c(
          "t",
          "test_name",
          "test_id",
          "group",
          "index",
          unit(),
          "ref",
          "delta",
          "%"
        )
      }

      df <- df() |> select(any_of(selections)) |> rename(data = unit())

      reactable(
        df,
        defaultColDef = colDef(
          align = "right",
          vAlign = "center"
        ),
        columns = list(
          test_id = colDef(align = "left"),
          test_name = colDef(name = "Test", align = "left"),
          t = colDef(filterable = T),
          group = colDef(filterable = T),
          index = colDef(filterable = T),
          data = colDef(name = unit(), format = colFormat(digits = 3)),
          ref = colDef(format = colFormat(digits = 3)),
          delta = colDef(format = colFormat(digits = 3)),
          `%` = colDef(format = colFormat(digits = 3))
        ),
        wrap = F,
        pagination = T,
        defaultPageSize = 25,
        striped = T,
        compact = T,
        filterable = F,
        highlight = T
      )
    }
  })

  output$raw_table <- renderReactable(raw_table())
  output$download_raw_data <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_raw_summary_statistics.tsv")
    },
    content = function(file) {
      write_tsv(df(), file)
    }
  )

  output$download_raw_data_as_html <- downloadHandler(
    filename = function() {
      glue("{input$test_id}_{input$test}_raw_summary_statistics.html")
    },
    content = function(file) {
      saveWidget(
        raw_table() |>
          add_title(glue("{input$test_id} {test_name()} Raw Data")) |>
          add_subtitle(glue("{input$file}")),
        file,
        selfcontained = T
      )
    }
  )

  output$selectedValue <- renderText(paste("Selected:", nrow(df())))
}

shinyApp(ui, server)
