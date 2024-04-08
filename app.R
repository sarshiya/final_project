library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(colourpicker)
library(dplyr)
library(shinythemes)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- shinyUI(
  fluidPage(
    theme = shinytheme("cyborg"),
    navbarPage(
      title = 'BF591 Final Project- Arshiya S',
      # Sample Information Section
      tabPanel('Sample Information',
               sidebarLayout(
                 sidebarPanel(
                   p('Load Sample Information Matrix'),
                   fileInput('file_sample_info', 'Choose file to upload',
                             accept = c('text/csv', 'text/comma-separated-values', '.csv'),
                             placeholder = "sample_info.csv")),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Summary", dataTableOutput(outputId = 'sample_summary')),
                     tabPanel("Data", dataTableOutput(outputId = 'sample_data')),
                     tabPanel("Plot", plotOutput(outputId = 'sample_bar'),
                              plotOutput(outputId = 'sample_violin'),
                              plotOutput(outputId = 'sample_histogram')))
                 )
               )
      ),
      # Counts Exploration Section
      tabPanel('Counts Exploration',
               sidebarLayout(
                 sidebarPanel(
                   p('Load Normalized Counts Matrix'),
                   fileInput('file_counts', 'Choose file to upload',
                             accept = c('text/csv', 'text/comma-separated-values', '.csv'),
                             placeholder = "normalized_counts.csv"),
                   
                   radioButtons('pca_x', "choose x axis principal component",
                                choices = c('1', '2', '3', '4'),
                                selected = '1'),
                   radioButtons('pca_y', "choose y axis principal component",
                                choices = c('1', '2', '3', '4'),
                                selected = '2'),
                   
                   sliderInput('slider_variance', "filter genes based on % variance",
                               value = 40,
                               min = 0,
                               max = 100),
                   sliderInput('slider_zero', "filter # of genes with zero variance",
                               value = 0,
                               min = 0,
                               max = 68),
                   submitButton(text = "Apply Changes", width = "100%")),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Summary", dataTableOutput(outputId = 'counts_summary')),
                     tabPanel("Scatter Plots", plotOutput("counts_scatter"),
                              plotOutput("counts_scatter2")),
                     tabPanel("Heatmap", plotOutput("counts_heatmap")),
                     tabPanel("PCA", plotOutput("counts_pca")))
                 )
                 
               )
      ),
      # Differential Expression Section
      tabPanel('Differential Expression',
               sidebarLayout(
                 sidebarPanel(
                   p('Load differential expression results'),
                   fileInput('file_de', 'Choose file to upload',
                             accept = c('text/csv', 'text/comma-separated-values', '.csv'),
                             placeholder = "deseq_results.csv"),
                   radioButtons('xaxis', "choose column for the x axis",
                                choices = c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj"),
                                selected = "log2FoldChange"),
                   
                   radioButtons('yaxis', "choose column for the y axis",
                                choices = c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj"),
                                selected = "pvalue"),
                   colourInput('accent_col', "Select threshold color", "#69b3a2"),
                   colourInput('base_col', "Select base color", "#404080"),
                   sliderInput('update_slider', "select the magnitude of p-adjusted coloring:",
                               value = -7,
                               min = -23,
                               max = 0),
                   submitButton(text = "Apply Changes", width = "100%")
                 ),
                 
                 # Show the volcano plot
                 mainPanel(tabsetPanel(
                   tabPanel("Plot", {
                     plotOutput("volcano")
                   }),
                   tabPanel("Table",
                            dataTableOutput("results_table"))
                 ))
               )),
      # GSEA Section
      tabPanel('GSEA',
               sidebarLayout(
                 sidebarPanel(
                   p('load fgsea results produced from DESeq2'),
                   fileInput('file_gsea', 'Choose file to upload',
                             accept = c('text/csv', 'text/comma-separated-values', '.csv'),
                             placeholder = "fgsea_results.csv"),
                   p('GSEA NES top pathways filter:'),
                   sliderInput('n_path_slider',
                               "select number of pathways:",
                               value = 10,
                               min = 1,
                               max = 30),
                   p("Data Tab filtering options:"),
                   sliderInput('gsea_padj', 'select p-adjusted filter',
                               value = -10,
                               min = -20,
                               max = 0),
                   radioButtons(inputId = 'fgsea_direction', 'NES Direction:',
                                choices = c('all', 'positive', 'negative'),
                                selected = 'positive'),
                   p('Scatterplot filtering options:'),
                   sliderInput('fgsea_scatter_slider', 'filter by p-adjusted value',
                               value = 0.01,
                               min = -.4,
                               max = 0.3),
                   submitButton(text = "Apply Changes", width = "100%"),
                   downloadButton('fgsea_output_download', 'Download Filtered Results'),
                   dataTableOutput('gsea_output_download')),
                 mainPanel(tabsetPanel(
                   tabPanel("Top Pathways", plotOutput(outputId = 'gsea_barplot')),
                   tabPanel("Data", dataTableOutput(outputId = 'gsea_data')),
                   tabPanel("NES vs padj Plot", plotOutput(outputId = 'gsea_scatter'))
                 ))
               ))
    )
  )
)

#define server logic
server <- function(input, output) {
  # Add your server logic here
}

shinyApp(ui, server)


#define server logic
server <- function(input, output) {
  
  # Define counts_df as a reactive expression
  counts_df <- reactive({
    read_counts <- read.delim(file = input$file_counts$datapath, sep = ",", header = TRUE)
    return(read_counts)
  })
  
  ##############################Sample Exploration Section ########################################################
  
  #reactive function -- load data
  load_sample<- reactive({
    read_sample <- read.delim(file = input$file_sample_info$datapath, sep = ",", header = TRUE) %>%
      data.frame() %>%
      return()
  })
  
  
  bar_plot <-function(df1) {
    bar <- df1 %>%
      ggplot(df1, mapping = aes(x=ID,y=Age_of_Death, fill=Condition)) +
      geom_bar(stat="identity", alpha = 0.74) +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90)) 
    
    return(bar)
  }
  
  
  violin_plot <-function(df1) {
    violin <-
      ggplot(df1) +
      geom_violin(aes(x=Condition,y=Age_of_Death, fill=Condition), alpha = 0.74) +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme(axis.text.x = element_text(angle = 0))
    # theme_minimal()
    return(violin)
  }
  
  histogram_plot <- function(df1){
    hist<- df1 %>%
      ggplot(aes(x=Age.of.Onset, fill =Age.of.Onset)) +
      geom_histogram(color ="#e9ecef", fill = "#69b3a2",  alpha=0.5, position = 'identity') +
      theme_minimal()+
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")
    
    return(hist)
  }
  
  #summary tab - summarize type and value for each column
  
  Column_Name <- c('Condition','PMI', 'Age of Death', 'RIN', 'mRNA_Seq_reads', 'Age of Onset', 'Duration')
  Type <- c( 'factor', 'double', 'double', 'double', 'double', 'double', 'double' )
  Mean_Value <- c('Control, HD', '15.2', '65.8','7.71', '85,170,870', '41.85','16.4')
  
  summary_data <- data.frame(Column_Name, Type, Mean_Value)
  
  output$sample_summary<-renderDataTable({
    req(input$file_sample_info$datapath)
    
    summary_data
    
  })
  
  
  summary_table <- function(data){ 
    sample_data <- read_csv('cbind_sample_info.csv')
    samples <- select(data, '-ID', '-CAG', '-Vonsattel Grade',	'-H-V Striatal Score',	'-H-V Cortical Score')
    sample_data$Condition <- as.factor(sample_data$Condition)
    col_names <- colnames(sample_data)
    new_data <- drop_na(sample_data)
    RIN_mean <- mean(new_data$RIN)
    death_mean <- round(mean(new_data$Age_of_Death),3)
    PMI_mean <- round(mean(new_data$PMI),3)
    Seq_reads_mean <- round(mean(new_data$mRNA_Seq_reads),3)
    condition <- c('Control', 'Huntington Disease')
    
    summary <- data.frame(col_names)
    all_means <- c(RIN_mean,death_mean,PMI_mean,Seq_reads_mean, condition)
    type <- c('Numeric', 'Intger', 'Numeric', 'Integer', 'Character') 
    combine <- cbind(summary, all_means)
    sample_summary <- cbind(summary,type)
    names(sample_summary) <- c("Column Name", "Mean or Distinct Type", "Type") 
    return(sample_summary)
    
  }
  
  
  #output sample info table
  output$sample_data<-renderDataTable({
    req(input$file_sample_info$datapath)
    
    load_sample()
  })
  # output$sample_summary<-renderDataTable(summary,options = list(pageLength = 25))
  
  
  output$sample_bar <- renderPlot({
    req(input$file_sample_info$datapath)
    
    bar_plot(df1 = load_sample())
  })
  
  output$sample_violin <- renderPlot({
    req(input$file_sample_info$datapath)
    
    violin_plot(df1 = load_sample()) 
  })
  
  
  output$sample_histogram <- renderPlot({
    req(input$file_sample_info$datapath)
    
    histogram_plot(df1 = load_sample()) 
  })
  
  
  
  ########################################Counts Exploration Section ###############################################
  
  #load counts data
  norm_counts<- reactive({
    read_counts <- read.delim(file = input$file_counts$datapath, sep = ",", header = TRUE) %>%
      # if (is.null(read_counts))
      #   return(NULL)
      return(read_counts)
  })
  
  
  
  filter_zero <- function(data, slider1){
    nonzeros <- data[rowSums(data > 0) >= slider1, ]
    return(nonzeros)
  }
  
  #filter counts table 
  draw_counts_table <- function(data, slider1, slider2) {
    d <- data[c(-1)]
    nonzeros <- d[rowSums(d > 0) >= slider1, ]
    variances <- apply(nonzeros, 1, var)
    table <- nonzeros[variances < slider2, ]  
    
    return(table)
  }
  
  
  #get rid of gene id column
  #deseq_data <- counts_df[c(-1)]
  
  variance_vs_mean <- function(data, scale_y_axis = FALSE, title = "", slider1 = 68, slider2 = 10) {
    all_means <- apply(data, 1, mean)
    variances <- apply(data, 1, var)
    plot_data <- tibble(mean = all_means, variance = variances)
    plot_data$rank <- rank(plot_data$mean)
    
    mean_var_plot <- plot_data %>%
      select(c(rank, variance)) %>%
      mutate(filter_status = ifelse(.[, 2] < slider2 | rowSums(data > 0) <= slider1, 'True', 'False')) %>%
      ggplot() +
      geom_point(aes(x = rank, y = variance, color = factor(filter_status, levels = c('True', 'False')))) +
      scale_color_manual(values = c('black', 'red')) +
      theme_minimal() +
      theme(legend.position = 'bottom') +
      labs(x = "Rank(Mean)", y = "Varience", color = paste0('variance < ', slider2))
    if (scale_y_axis) {
      mean_var_plot <- mean_var_plot + scale_y_log10()
    }
    
    return(mean_var_plot)
  }
  
  
  medcounts_vs_zeros <- function(data, slider, color='red'){
    row.names(data) <- data$gene
    data <- select(data, -gene) #this is the second scatter plot for the second filter non zeros 
    labeled <- mutate(data, median = apply(data,1,median), zeros = rowSums(data == 0), filter_status = (ifelse(zeros < slider/(100*length(data)), "TRUE", "FALSE")))
    plot <- ggplot(labeled, aes(x=log(median), y = zeros , color = filter_status)) +
      geom_point()+
      scale_color_manual(values = c('black','red'))+
      xlab("Median Count") +
      ylab("Number of Zeros") +
      ggtitle("Scatter Plot of Median Count vs Number of Zeros ")
    return(plot)
  }
  
  
  plot_pca <- function(data, meta_info, title="", a, b) {
    pca <- prcomp(t(data))
    plot_data <- meta_info
    #change string to integer, bc radio buttons need the input to be a string 
    a <- strtoi(a)
    b<- strtoi(b)
    plot_data$PC1 <- pca$x[ , a]
    plot_data$PC2 <- pca$x[ , b]
    percent_variance <- pca$sdev^2 / sum( pca$sdev^2 )
    print(percent_variance)
    pca_plot <- ggplot(plot_data,aes(x=PC1, y=PC2, col=condition)) +
      geom_point() +
      xlab(paste0("PC", a ,":",round(percent_variance[a] * 100),"% variance")) +
      ylab(paste0("PC", b , ": ",round(percent_variance[b] * 100),"% variance")) +
      ggtitle(title)
    return(pca_plot)
  }
  
  plot_heatmap <-function(data, filter1, filter2){
    rownames(data) <- data$X
    data <- select(data, -X)
    data <- log(data)
    data_var <-  mutate(data, variance = apply(data,1,var))
    data_ordered<-data_var[order(data_var$variance,decreasing = T),]
    f1 <- filter1/(100*length(data))
    pass1 <- data_ordered[data_ordered$variance > f1,]
    pass1<- select(pass1, -variance)
    new_data <- select(data_ordered, -variance)
    f2<- filter2/(100*length(data))
    data_nonzero <- mutate(new_data, zeros = rowSums(data_var == 0))
    pass2 <- data_nonzero[data_nonzero$zeros > f2,]
    pass2 <- select(pass2, -zeros)
    combined <- rbind(pass1, pass2) %>% distinct()
    final_matrix<- as.matrix(combined)
    plot_heat<- heatmap(final_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))  
    legend(x="right", legend=c("min", "med", "max"),fill=heat.colors(3))
    return(plot_heat)
  }
  
  
  #output results -- counts matrix summary table
  output$counts_summary<-renderDataTable({
    req(input$file_counts$datapath)
    
    
    draw_counts_table(norm_counts(),slider1 = input$slider_zero, slider2 = input$slider_variance) 
    
    # norm_counts()
  })
  
  #mean vs variance plot 
  output$counts_scatter<- renderPlot({
    req(input$file_counts$datapath)
    
    variance_vs_mean(counts_df(), TRUE, "Variance vs Mean (DESeq2)", input$slider_zero, input$slider_variance)
  })
  
  
  #mean count vs zeros plot 
  output$counts_scatter2<- renderPlot({
    req(input$file_counts$datapath)
    
    medcounts_vs_zeros(norm_counts(),input$slider_zero)
  })
  
  #pca plot 
  output$counts_pca <- renderPlot({
    req(input$file_counts$datapath)
    
    
    plot_pca(norm_counts(), 
             meta_data, 
             title='Normalized Counts PCA',
             input$pca_x,
             input$pca_y)
    
  })
  
  #heatmap plot
  output$counts_heatmap <- renderPlot({
    plot_heatmap(norm_counts(), input$slider_variance, input$slider_zero)
  })
  
  
  ############################### Differential Expression Section #########################################
  
  #reactive function -- load data
  load_data<- reactive({
    inFile <- read.delim(file = input$file_de$datapath, sep = ",", header = TRUE) %>%
      rename(ENSEMBL_ID = X) %>%
      return()
  })
  
  #function for generating volcano plot
  volcano_plot <-function(dataf, x_name, y_name, slider, color1, color2) {
    volc_plot <- dataf %>%
      select(c(x_name,y_name)) %>%
      mutate(volcano_status = ifelse(.[,y_name] <10^slider, 'TRUE', 'FALSE')) %>%
      ggplot()+
      geom_point(aes(x= !!sym(x_name),
                     y= -log10(!!sym(y_name)),
                     color = factor(volcano_status, levels = c("TRUE", "FALSE")))) +
      scale_color_manual(values = c(color1,color2)) +
      theme_minimal()+
      theme(legend.position = "bottom") +
      labs(x = x_name,
           y = paste0('-log10(',y_name,')'),
           color = paste0(y_name, ' < 1 x 10^',slider))
    
    return(volc_plot)
  }
  
  
  #Function that filters data frame table to rows that are above the slider magnitude
  draw_table <- function(dataf, slider) {
    new_table <- dataf %>%
      arrange(pvalue) %>%
      filter(padj < 10^slider) %>%
      mutate(pvalue = formatC(.$pvalue, digits = 2, format = 'e'),
             padj = formatC(.$padj, digits =2, format = 'e'))
    
    return(new_table)
  }
  
  
  #output results
  output$results_table<-renderDataTable({
    (req(input$file_de$datapath))
    
    draw_table(dataf = load_data(),
               slider = input$update_slider)
  })
  
  output$volcano <- renderPlot({
    req(input$file_de$datapath)
    
    volcano_plot(dataf = load_data(),
                 x_name = input$xaxis,
                 y_name = input$yaxis,
                 slider = input$update_slider,
                 color1 = input$accent_col,
                 color2 = input$base_col)
  })
  
  ############### GSEA ########################
  
  #reactive function -- load data
  load_gsea<- reactive({
    read_gsea <- read.csv(input$file_gsea$datapath, sep = ",", header = TRUE) %>%
      return(read_gsea)
  })
  
  top_pathways <- function(fgsea_results, num_paths){
    
    top_pos <- fgsea_results %>% slice_max(NES, n=num_paths) %>% pull(pathway)
    top_neg <- fgsea_results %>% slice_min(NES, n=num_paths) %>% pull(pathway)
    
    subset <- fgsea_results %>% 
      filter(pathway %in% c(top_pos, top_neg)) %>%
      mutate(pathway = factor(pathway)) %>%
      mutate(plot_name = str_replace_all(pathway, '_', ' '))
    
    plot <- subset %>% 
      mutate(plot_name = forcats::fct_reorder(factor(plot_name), NES)) %>%
      ggplot() +
      geom_bar(aes(x=plot_name, y=NES, fill = NES > 0), stat='identity', show.legend = FALSE) +
      scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue')) + 
      theme_minimal(base_size = 8) +
      ggtitle('fgsea results for Hallmark MSigDB gene sets') +
      ylab('Normalized Enrichment Score (NES)') +
      xlab('') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 80)) +
      coord_flip()
    return(plot)
  }
  
  
  
  # #draw filtered fgsea table
  draw_gsea_table<- function(data, slider, direction, col_name){
    filtered <- data[data$padj > slider,]
    if(direction == "positive"){
      new <- filter(filtered, NES > 0)
      col_name <- new[,c(col_name)] #specify a column by name 
      sorted <- new[order(col_name),]
      return(sorted)
    }
    else if(direction == "negative"){
      
      new <- filter(filtered, NES < 0)
      col_name <- new[,c(col_name)] #specify a column by name 
      sorted <- new[order(col_name),]
      return(sorted)
    }
    
    else {
      col_name <- filtered[,c(col_name)] #specify a column by name 
      sorted <- filtered[order(col_name),]
      return(sorted)
    }
    return(filtered)
    
  }
  
  
  
  #   # draw filtered fgsea table
  #   draw_gsea_table <- function(data, slider, direction) {
  #     filtered_table <- data %>%
  #       arrange(pval) %>%
  #       filter(padj < 10^slider) %>%
  #       mutate(pval = formatC(.$pval, digits = 2, format = 'e'),
  #              padj = formatC(.$padj, digits =2, format = 'e'))
  
  #     return(filtered_table)
  #   }
  
  
  #fgsea NES scatter plot  
  gsea_scatterplot<- function(data, slider){
    passed <- data[data$padj > slider, "pathway"]
    new_data <- mutate(data, passing = ifelse(data$pathway %in% passed, "TRUE", "FALSE"))
    scatter <- ggplot(new_data,aes(NES, -log10(padj), color = passing)) +
      geom_point()+
      scale_color_manual(values = c("blue", "grey")) +
      xlab("NES") +
      ylab("-log10(padj)") +
      ggtitle("Scatter Plot of NES vs -log10(padj) ")
    return(scatter)
  }
  
  # bar chart, tab 1 
  output$gsea_barplot <- renderPlot({
    req(input$file_gsea$datapath)
    
    top_pathways(load_gsea(), num_paths = input$n_path_slider)
  })
  
  # output$gsea_data <- renderDataTable(load_gsea())
  
  # #data table with filtered results
  output$gsea_data<-renderDataTable(draw_gsea_table(load_gsea(),
                                                    slider = input$gsea_padj,
                                                    direction = input$fgsea_direction)
                                    # col_name = input$gsea_col_name)
  )
  
  #fgsea  filtered results table download
  output$fgsea_output_download<-downloadHandler(
    filename= function() {'fgsea_results2.csv'},
    content = function(file){
      write_csv(draw_gsea_table(data = load_gsea(),
                                slider = input$gsea_padj),
                file)
    }
  )
  
  #fgsea normalized expression score -- scatter plot 
  output$gsea_scatter <- renderPlot({
    req(input$file_gsea$datapath)
    
    gsea_scatterplot(load_gsea(), slider=input$fgsea_scatter_slider)
    
  })
  
  
  
  
}

shinyApp(ui, server)