# README: BF591 Final Project - Arshiya S

## Overview
This Shiny application is designed for the analysis and visualization of bioinformatics data, specifically focusing on gene expression analysis. The application includes functionalities for loading and exploring sample information, counts data, differential expression results, and gene set enrichment analysis (GSEA) results.

## Features
- **Sample Information Analysis:**
  - Load and explore sample metadata.
  - View summary statistics and data tables.
  - Generate bar plots, violin plots, and histograms for sample characteristics.

- **Counts Exploration:**
  - Load and visualize normalized gene expression counts.
  - Apply filters based on variance and zero count thresholds.
  - Generate scatter plots, PCA plots, and heatmaps.

- **Differential Expression Analysis:**
  - Load differential expression results.
  - Generate a volcano plot with adjustable coloring and axis options.
  - View a filtered results table.

- **GSEA (Gene Set Enrichment Analysis):**
  - Load fgsea results from DESeq2.
  - Generate bar plots for top pathways.
  - View and filter results tables.
  - Generate a scatter plot of NES vs. adjusted p-values.
  - Download filtered results.

## Installation and Requirements
### Dependencies
This application requires the following R packages:
- `shiny`
- `DT`
- `ggplot2`
- `tidyverse`
- `colourpicker`
- `dplyr`
- `shinythemes`

Install these packages if not already installed using:
```r
install.packages(c("shiny", "DT", "ggplot2", "tidyverse", "colourpicker", "dplyr", "shinythemes"))
```

### Running the Application
To run the Shiny application, use the following command in R:
```r
shiny::runApp("path/to/your/shiny/app")
```
Replace `path/to/your/shiny/app` with the actual directory containing the Shiny app files.

## File Upload Guidelines
- **Sample Information File:** CSV format with metadata about samples.
- **Normalized Counts File:** CSV format containing gene expression counts.
- **Differential Expression Results File:** CSV format with DESeq2 results.
- **GSEA Results File:** CSV format with fgsea results.

## Application Structure
- `ui`: Defines the user interface layout and elements.
- `server`: Implements data processing, visualization, and user interactions.
- `shinyApp(ui, server)`: Runs the Shiny application.

## Usage Instructions
1. Upload the respective data files in each tab.
2. Use interactive elements (sliders, radio buttons, etc.) to adjust visualizations.
3. View and explore summary statistics, tables, and plots.
4. Download filtered GSEA results as needed.

## Notes
- The application assumes that uploaded CSV files are properly formatted.
- PCA plots require sample metadata for coloring points based on conditions.
- Ensure all input files use a comma (`,`) as a delimiter.

## Author
**Arshiya S**

This project is part of the BF591 Final Project At Boston University

