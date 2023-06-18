# publication-table-creator

### Data Cleaning and Visualization Creator
This project provides an R Shiny application to perform data cleaning and visualization tasks. It provides various features such as data importing, data cleaning, and data visualization using different types of plots and tables. The users can upload their own CSV or JSON files and apply the desired operations.

### Dependencies
The required packages to run this application are as follows:

- tidyverse
- jsonlite
- R6
- rempsyc
- officer
- flextable
- ftExtra
- shiny
- shinyFiles
- htmltools
- webshot
- These dependencies can be installed using install.packages() function in R.

R
#### Copy code
install.packages(c("tidyverse", "jsonlite", "R6", "rempysc", "officer", "flextable", "ftExtra", "shiny", "shinyFiles", "htmltools", "webshot"))
### How to Run
To run the application, you can simply clone the repository, open the R script in RStudio (or your preferred R environment), and then run the script.

The application should automatically launch in a web browser, allowing you to interact with it.

### Features
The main features of the application are:

#### Data Importing: The application supports importing of CSV and JSON files.

#### Data Cleaning: The application provides several data cleaning options, such as removal of NAs/NaNs, specific columns, or duplicate rows.

#### Data Visualization: The application provides several options for visualizing the data, including summary tables, comparison tables, and different types of plots (histogram, bar plot, line plot).

### Code Description
The codebase incorporates four important R programming aspects:

R Shiny: The interactive application is built using the Shiny package, providing an intuitive UI for data cleaning and visualization.

Vectorization: Vectorized operations are used during the data cleaning process to improve performance.

Advanced Functions: The code includes a number of custom advanced functions, incorporating concepts of defensive programming to handle potential errors and edge cases.

Object-Oriented Programming: The code structure is built around several custom classes, methods, and generic functions using the R6 package, providing a modular and maintainable codebase.

Contact
For any issues or suggestions, feel free to reach out.

### License
MIT
