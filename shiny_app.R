library(tidyverse)
library(jsonlite)
library(R6)
library(rempsyc)
library(officer)
library(flextable)
library(ftExtra)
library(shiny)
library(shinyFiles)
library(shiny)
library(htmltools)
library(webshot)


FileReader <- R6Class("FileReader",
                      public = list(
                        from_csv = function(file_path) {
                          data <- read_csv(file_path, col_types = cols())
                          return(data)
                        },
                        from_json = function(file_path) {
                          data <- jsonlite::fromJSON(file_path)
                          return(data)
                        }
                      ))

style_table <-
  function(ft,
           title = NULL,
           footer = NULL,
           title_size = 15) {
    if (!is.null(title)) {
      title <- paste0("\n\n", title)
      ft <- ft %>%
        set_caption(caption = as_paragraph(as_chunk(title, props = fp_text(font.size = title_size))))
    }

    if (!is.null(footer)) {
      ft <- ft %>%
        add_footer_lines(footer) %>%
        italic(part = "footer")
    }

    ft <-
      ft %>%
      bg(bg = "white") %>%
      bg(bg = "white", part = "header") %>%
      bg(bg = "white", part = "footer")

    return(ft)
  }
ComparisonTableCreator <- R6Class(
  "ComparisonTableCreator",
  public = list(
    comparison_with_bold_max = function(data,
                                        save_path = NULL,
                                        title = NULL,
                                        footer = NULL,
                                        title_size = 12) {
      ft <- flextable(data)
      for (col in colnames(data)) {
        if (is.numeric(data[[col]])) {
          max_val <- max(data[[col]])
          max_row <-
            which(data[[col]] == max_val)
          ft <- ft %>%
            bold(i = max_row, j = col, part = "body")
        }
      }
      ft <-
        style_table(ft,
                    title = title,
                    footer = footer,
                    title_size = title_size)

      if (!is.null(save_path)) {
        save_as_image(ft, save_path)
      }

      return(ft)

    },
    comparison = function(data,
                          save_path = NULL,
                          title = NULL,
                          footer = NULL,
                          title_size = 12) {
      ft <- flextable(data)
      ft <-
        style_table(ft,
                    title = title,
                    footer = footer,
                    title_size = title_size)

      if (!is.null(save_path)) {
        save_as_image(ft, save_path)
      }

      return(ft)
    }
  )
)

SummaryTableCreator <- R6Class(
  "SummaryTableCreator",
  public = list(
    plot = function(data,
                    save_path = NULL,
                    title = NULL,
                    footer = NULL,
                    title_size = 15) {
      df_summary <- data
      # add helper col with const value, since continuous_summary require 'by' argument
      df_summary["Column"] = ""

      ft <- continuous_summary(df_summary, by = "Column")
      ft <-
        style_table(ft,
                    title = title,
                    footer = footer,
                    title_size = title_size)

      if (!is.null(save_path)) {
        ft_img <- as_image(ft)
        image_write(ft_img, path = save_path, format = "png")
      }
      print("DUPA")
      return(ft)
    },
    plot_continuous_summary = function(data,
                                       group_by_col,
                                       save_path = NULL,
                                       title = NULL,
                                       footer = NULL,
                                       title_size = 15) {
      ft <- continuous_summary(data, by = group_by_col)
      ft <-
        style_table(ft,
                    title = title,
                    footer = footer,
                    title_size = title_size)

      if (!is.null(save_path)) {
        ft_img <- as_image(ft)
        image_write(ft_img, path = save_path, format = "png")
      }

      return(ft)
    }
  )


)




PlotCreator <- R6Class(
  "PlotCreator",
  public = list(
    plot_histogram = function(column_name,
                              data,
                              title = NULL,
                              footer = NULL,
                              bins = 5) {
      if (!(column_name %in% names(data))) {
        stop(paste0("Column '", column_name, "' not found in data"))
      }
      bins <- as.integer(bins)
      histogram <-
        ggplot(data, aes_string(column_name)) +
        geom_histogram(fill = 'blue',
                       color = 'black',
                       bins = bins) +
        theme_minimal() +
        theme(plot.background = element_rect(fill = 'white', colour = 'white'))

      if (!is.null(title)) {
        histogram <- histogram + ggtitle(title)
      }
      if (!is.null(footer)) {
        histogram <- histogram + labs(caption = footer)
      }

      return(histogram)
    },
    plot_line = function(x_column_name,
                         y_column_name,
                         data,
                         title = NULL,
                         footer = NULL) {
      if (!(x_column_name %in% names(data))) {
        stop(paste0("Column '", x_column_name, "' not found in data"))
      }
      if (!(y_column_name %in% names(data))) {
        stop(paste0("Column '", y_column_name, "' not found in data"))
      }

      lineplot <-
        ggplot(data, aes_string(x_column_name, y_column_name)) +
        geom_line(color = 'blue') +
        theme_minimal() +
        theme(plot.background = element_rect(fill = 'white', colour = 'white'))

      if (!is.null(title)) {
        lineplot <- lineplot + ggtitle(title)
      }
      if (!is.null(footer)) {
        lineplot <- lineplot + labs(caption = footer)
      }

      return(lineplot)
    },
    plot_bar = function(name_column_name,
                        value_column_name,
                        data,
                        title = NULL,
                        footer = NULL) {
      if (!(name_column_name %in% names(data))) {
        stop(paste0("Column '", name_column_name, "' not found in data"))
      }
      if (!(value_column_name %in% names(data))) {
        stop(paste0("Column '", value_column_name, "' not found in data"))
      }

      barplot <-
        ggplot(data, aes_string(x = name_column_name, y = value_column_name)) +
        geom_bar(stat = 'identity',
                 fill = 'blue') +
        theme_minimal() +
        theme(plot.background = element_rect(fill = 'white', colour = 'white'))

      if (!is.null(title)) {
        barplot <- barplot + ggtitle(title)
      }
      if (!is.null(footer)) {
        barplot <- barplot + labs(caption = footer)
      }

      return(barplot)
    }
  )
)


ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
      .shiny-notification {
        font-size: 20px;        /* Increase font size */
        width: 30%;             /* Set width of the notification */
        height: auto;           /* Set height of the notification */
        position: fixed;        /* Make the position fixed */
        top: 10%;               /* Set position from the top */
        right: 17%;             /* Set position from the right */
        transform: translateY(-50%); /* Adjust the position */
      }
    "
    )
  )),
  titlePanel("Visualization creator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Choose CSV or JSON File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          "application/json",
          "text/json",
          ".json"
        )
      ),
      conditionalPanel(
        condition = "output.fileUploaded",
        radioButtons(
          "plot_type",
          "Choose plot type:",
          choices = c("Summary", "Comparison", "Histogram", "Bar Plot", "Line Plot")
        ),
        h2("Image parameters:"),
        conditionalPanel(
          condition = "input.plot_type == 'Summary'",
          checkboxInput("groupped", "Group by column", value = F),
          conditionalPanel(
            condition = "input.groupped == true",
            textInput("group_by_col", "Group by col:")
          )
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Comparison'",
          checkboxInput("bold_max", "Bold max values", value = F),
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Bar Plot'",
          textInput("name_column_name", "Name column"),
          textInput("value_column_name", "Value column"),
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Line Plot'",
          textInput("x_column_name", "X column"),
          textInput("y_column_name", "Y column"),
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Histogram'",
          textInput("bins", "Bins:", value = 5),
          textInput("column_name", "Column Name:"),
        ),
        textInput("title", "Title:"),
        textInput("footer", "Footer:"),
        actionButton("prepare", "Prepare plot"),
        conditionalPanel(
          condition = "output.plotAvailable",
          downloadButton("downloadPlot", "Save Plot/Table to PNG")
        ),
      )
    ),
    mainPanel(uiOutput("table"))
  )
)



server <- function(input, output) {
  fileReader <- FileReader$new()
  comparisonTableCreator <- ComparisonTableCreator$new()
  summaryTableCreator <- SummaryTableCreator$new()
  plotCreator = PlotCreator$new()

  plot_table <- reactiveVal(NULL)

  fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  output$fileUploaded <- reactive({
    return(fileUploaded())
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)

  plotAvailable <- reactiveVal(FALSE)
  output$plotAvailable <- reactive({
    return(plotAvailable())
  })
  outputOptions(output, 'plotAvailable', suspendWhenHidden = FALSE)

  observeEvent(input$prepare, {
    req(input$file)
    data <- NULL
    if (grepl(".csv$", input$file$name)) {
      data <- fileReader$from_csv(input$file$datapath)
    } else if (grepl(".json$", input$file$name)) {
      data <- fileReader$from_json(input$file$datapath)
    }

    if (!is.null(data)) {
      table <- NULL
      tryCatch({
        if (input$plot_type == "Summary") {
          if (input$groupped == F) {
            table <-
              summaryTableCreator$plot(data,
                                       title = input$title,
                                       footer = input$footer)
          }
          else {
            table <-
              summaryTableCreator$plot_continuous_summary(
                data,
                title = input$title,
                footer = input$footer,
                group_by_col = input$group_by_col
              )
          }
        } else if (input$plot_type == "Comparison") {
          if (input$bold_max == F) {
            table <-
              comparisonTableCreator$comparison(data,
                                                title = input$title,
                                                footer = input$footer)
          } else {
            table <-
              comparisonTableCreator$comparison_with_bold_max(data,
                                                              title = input$title,
                                                              footer = input$footer)
          }
        }
        else if (input$plot_type == "Histogram") {
          table <-
            plotCreator$plot_histogram(
              column_name = input$column_name,
              data = data,
              title = input$title,
              footer = input$footer,
              bins = input$bins
            )
        }
        else if (input$plot_type == "Bar Plot") {
          table <-
            plotCreator$plot_bar(
              input$name_column_name,
              input$value_column_name,
              data = data,
              title = input$title,
              footer = input$footer
            )
        }
        else if (input$plot_type == "Line Plot") {
          table <-
            plotCreator$plot_line(
              x_column_name = input$x_column_name,
              y_column_name = input$y_column_name,
              data = data,
              title = input$title,
              footer = input$footer
            )
        }
        plot_table(table)
        plotAvailable(TRUE)

      }, error = function(e) {
        showNotification(paste("Error:", e$message),
                         type = "error",
                         duration = NULL)
        plotAvailable(FALSE)
      })

      output$table <- renderUI({
        req(table)
        if (is.ggplot(table)) {
          renderPlot({
            table
          })
        } else {
          table %>% autofit() %>% htmltools_value()
        }
      })

      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste("plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          table <- plot_table()

          if (inherits(table, "ggplot")) {
            # It's a plot. Save it using ggsave.
            ggsave(file,
                   plot = table,
                   width = 10,
                   height = 10)
          } else if (inherits(table, "flextable")) {
            # It's a flextable. Save it using save_as_image.
            save_as_image(table, file)
          }
        }
      )


    }
  })
}


shinyApp(ui = ui, server = server)
