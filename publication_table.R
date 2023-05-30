library(tidyverse)
library(jsonlite)
library(R6)
library(rempsyc)
library(officer)
library(flextable)
library(ftExtra)


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
SummaryTableCreator <- R6Class(
  "SummaryTableCreator",
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

SummaryPlotter <- R6Class("SummaryPlotter",
                          public = list(
                            plot = function(data,
                                            save_path = NULL,
                                            title = NULL,
                                            footer = NULL,
                                            title_size = 15) {
                              data <- as.data.frame(t(data))

                              data <-
                                rownames_to_column(data, var = "Feature name")

                              #colnames(data)[-1] <-
                               # c("Count", "Mean", "Std", "Min", "25%", "50%", "75%", "Max")

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
                            }
                          ))

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
                         },
                         plot_line = function(x_column_name,
                                              y_column_name,
                                              data,
                                              title = NULL,
                                              footer = NULL) {
                           # Check if x_column_name and y_column_name exists in the data
                           if (!(x_column_name %in% names(data))) {
                             stop(paste0("Column '", x_column_name, "' not found in data"))
                           }
                           if (!(y_column_name %in% names(data))) {
                             stop(paste0("Column '", y_column_name, "' not found in data"))
                           }

                           # Create the line plot
                           lineplot <-
                             ggplot(data, aes_string(x_column_name, y_column_name)) +
                             geom_line(color = 'blue') +
                             theme_minimal()

                           # Add title and footer if provided
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
                           # Check if name_column_name and value_column_name exists in the data
                           if (!(name_column_name %in% names(data))) {
                             stop(paste0("Column '", name_column_name, "' not found in data"))
                           }
                           if (!(value_column_name %in% names(data))) {
                             stop(paste0("Column '", value_column_name, "' not found in data"))
                           }

                           # Create the bar plot
                           barplot <-
                             ggplot(data, aes_string(x = name_column_name, y = value_column_name)) +
                             geom_bar(stat = 'identity', fill = 'blue') +
                             theme_minimal()

                           # Add title and footer if provided
                           if (!is.null(title)) {
                             barplot <- barplot + ggtitle(title)
                           }
                           if (!is.null(footer)) {
                             barplot <- barplot + labs(caption = footer)
                           }

                           return(barplot)
                         }
                       ))


?geom_histogram
plot_creator = PlotCreator$new()
is.ggplot(plot_creator$plot_histogram("MAPE", data = cross_data, bins=3, title = "Title"))
plot_creator$plot_histogram("MAPE", data = cross_data, bins=3, title = "Title")
plot_creator$plot_line("MAPE", "MAE", data = cross_data, title = "Title")

data_bar <- file_reader$from_csv("example_bar.csv")
plot_creator$plot_bar("name", "value", data_bar)
? as_chunk

df_describe = file_reader$from_csv("example_describe.csv")
df_describe
names(df_describe)
summary <- continuous_summary(df_describe[, c("mother_body_mass_index", "mother_marital_status", "mother_delivery_weight")])

ft_1 <- continuous_summary(iris, names(iris)[1:4], by = NULL,
                           hide_grouplabel = FALSE)
ft_1
iris
colnames(df_describe)
continuous_summary(df_describe, by="newborn_gender")
summary_plotter <- SummaryPlotter$new()
summary_plotter$plot(df_describe, title = "Dataset summary", footer = "Source: Own work")

cross_data = file_reader$from_csv("example_cross.csv")
cross_data
cross_data[c("Data_Type", "Model", "MAPE")]

cross_data
table <- flextable(cross_data)
table <- add_header_row(
  table,
  header = list(
    " " = 2,
    "Error Measures" = 3
  )
)
cross_data %>%
  kable("html") %>%
  kable_styling() %>%
  add_header_above(c(" " = 2, "Error Measures" = 3))

install.packages("kableExtra")
library(kableExtra)

cross_data[c("Data_Type", "Model", "MAPE")] %>%
  pivot_wider(names_from = c("Data_Type", "Model"),
              values_from = "MAPE") %>%
  as_flextable() %>%
  span_header()

cross_data %>%
  pivot_longer(cols = c("MAPE", "MAE", "RMSE"), names_to = "metric") %>%
  pivot_wider(names_from = "Model", values_from = "metric") %>%
  as_flextable() %>%
  span_header()

cross_data %>%
  pivot_longer(cols = c("MAPE", "MAE", "RMSE"), names_to = "metric")

? pivot_longer
? pivot_wider
multiheader_table(cross_data, "Model", "Data_Type")

ft <- flextable(wide_data)  %>% separate_header()
create_table(cross_data, "Model", "Data_Type")
# Create an instance of FileReader
file_reader <- FileReader$new()

# Read a CSV file with a specific delimiter (e.g., tab-separated)
csv_data <- file_reader$from_csv("example.csv")
nice_table(csv_data)

? bold

summary_table_creator <- SummaryTableCreator$new()
print(
  summary_table_creator$comparison_with_bold_max(csv_data, title = "Regression modele results comparison", footer = "Source: Own work")
)
print(
  summary_table_creator$comparison(
    csv_data,
    title = "Regression modele results comparison",
    footer = "Source: Own work",
    title_size = 13
  )
)
