# Analysis of the NHANES dataset

The National Health and Nutrition Examination Survey (NHANES), conducted by the National Centre for Health Statistics (NCHS) under the Centres for Disease Control and Prevention (CDC), is a comprehensive, ongoing programme designed to assess the health and nutritional status of adults and children across the United States. Combining data from interviews, physical examinations, and laboratory tests, NHANES provides a nationally representative dataset that offers critical insights into population health. This report analyses the NHANES dataset within the R environment to explore the relationship between Body Mass Index (BMI)—a widely used measure of body composition—and various health and lifestyle indicators. This analysis within the R environment seeks to explore BMI variance across demogrpahic groups as well as explore the relationship that BMI (as well as physical activity and sleep) has on other health indicators.

## Overview

This repository contains an R project that utilises the `renv` package for dependency management. The project includes:

- An `.Rproj` file for RStudio project setup
- An `renv` folder for managing package dependencies
- An `renv.lock` file to ensure reproducibility
- An `.R` script containing the core data analysis steps
- An `.Rmd` report detailing the analysis and findings

## Getting Started

### Prerequisites

To run this project, ensure you have the following installed:

- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/)
- [`renv` package](https://rstudio.github.io/renv/articles/renv.html) (for dependency management)

### Clone the Repository

```sh
git clone https://github.com/rborg91/nhanes-analysis.git
```

### Open the Project

1. Open RStudio.
2. Use `File > Open Project` and select the `nhanes_analysis.Rproj` file in the cloned repository.

### Restore Dependencies

To ensure all required packages are installed, run the following in R:

```r
install.packages("renv")  # If renv is not installed
renv::restore()
```

This will install the exact package versions specified in `renv.lock`.

### Run the Analysis

- To execute the R script which should generate a .csv file as well as .png images (plots) in a folder named `visualisations`, run the following command in the console in RStudio:

  ```r
  source("nhanes_analysis.R")
  ```

- To knit the R Markdown report (`.Rmd`) and generate an HTML/PDF report, use RStudio's **Knit** button or run:

  ```r
  rmarkdown::render("nhanes_analysis_report.Rmd")
  ```
