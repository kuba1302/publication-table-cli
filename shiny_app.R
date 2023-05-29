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
      ft <- ft %>%
        set_caption(ft, caption = as_paragraph(as_chunk(title, props = fp_text(font.size = title_size))))
    }

    if (!is.null(footer)) {
      ft <- ft %>%
        add_footer_lines(footer) %>%
        italic(part = "footer")
    }

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
      data <- as.data.frame(t(data))

      data <-
        rownames_to_column(data, var = "Feature name")

      #    colnames(data)[-1] <-
      #   c("Count", "Mean", "Std", "Min", "25%", "50%", "75%", "Max")

      ft <- flextable(data)

      ft <- ft %>%
        autofit() %>%
        bold(part = "header") %>%
        bold(j = "Feature name")

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


PlotCreator <- R6Class("PlotCreator",
                       public = list(
                         plot_histogram = function(column_name,
                                                   data,
                                                   title = NULL,
                                                   footer = NULL,
                                                   bins = 5) {
                           # Check if column_name exists in the data
                           if (!(column_name %in% names(data))) {
                             stop(paste0("Column '", column_name, "' not found in data"))
                           }

                           # Create the histogram
                           histogram <-
                             ggplot(data, aes_string(column_name)) +
                             geom_histogram(fill = 'blue',
                                            color = 'black',
                                            bins = bins) +
                             theme_minimal()

                           # Add title and footer if provided
                           if (!is.null(title)) {
                             histogram <- histogram + ggtitle(title)
                           }
                           if (!is.null(footer)) {
                             histogram <- histogram + labs(caption = footer)
                           }

                           return(histogram)
                         }
                       ))

ui <- fluidPage(
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
          choices = c("Summary", "Comparison", "Histogram")
        ),
        h2("Image parameters:"),
        conditionalPanel(
          condition = "input.plot_type == 'Summary'",
          checkboxInput("continuous", "Continuous", value = F),
          textInput("group_by_col", "Group by col:"),
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Comparison'",
          checkboxInput("bold_max", "Bold max values", value = F)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Histogram'",
          textInput("bins", "Bins:", value = 5),
          textInput("column_name", "Column Name:"),
        ),
        textInput("title", "Title:"),
        textInput("footer", "Footer:"),
        actionButton("prepare", "Prepare plot")
      )
    ),
    mainPanel(uiOutput("table"))
  )
)


? textInput
server <- function(input, output) {
  fileReader <- FileReader$new()
  ComparisonTableCreator <- ComparisonTableCreator$new()
  SummaryTableCreator <- SummaryTableCreator$new()
  plotCreator = PlotCreator$new()

  fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  output$fileUploaded <- reactive({
    return(fileUploaded())
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)

  observeEvent(input$prepare, {
    req(input$file)
    data <- NULL
    if (grepl(".csv$", input$file$name)) {
      data <- fileReader$from_csv(input$file$datapath)
    } else if (grepl(".json$", input$file$name)) {
      data <- fileReader$from_json(input$file$datapath)
    }

    if (!is.null(data)) {
      if (input$plot_type == "Summary") {
        if (input$continuous == F) {
          table <-
            SummaryTableCreator$plot(data,
                                     title = input$title,
                                     footer = input$footer)
        }
        else {
          table <-
            SummaryTableCreator$plot_continuous_summary(
              data,
              title = input$title,
              footer = input$footer,
              group_by_col = input$group_by_col
            )
        }
      } else if (input$plot_type == "Comparison") {
        if (input$bold_max == F) {
          table <-
            ComparisonTableCreator$comparison(data,
                                              title = input$title,
                                              footer = input$footer)
        } else {
          table <-
            ComparisonTableCreator$comparison_with_bold_max(data,
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

      output$table <- renderUI({
        if (is.ggplot(table)) {
          renderPlot({
            table
          })
        } else {
          table %>% autofit() %>% htmltools_value()
        }
      })
    }
  })
}

shinyApp(ui = ui, server = server)
