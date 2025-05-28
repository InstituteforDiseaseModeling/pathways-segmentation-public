@ -0,0 +1,234 @@
# Compare 2 segmentations ----
# This script generates a comprehensive PDF report to compare two clustering
# solutions side-by-side. It includes a function to generate:
#
# 1.  Sankey Diagram
# Visualizes how individuals move between clusters in segmentation A and segmentation B.
#
# 2. Confusion Matrix & Accuracy Stats
# Displays a confusion matrix and overall accuracy between the two cluster assignments.
#
# 3. Profiling Plots for Each Variable
# For each profiling variable:
#   Creates stacked bar plots comparing distributions across clusters in both
#   segmentations.
#   Adds Jensen-Shannon Divergence (JSD) values to quantify how similar each
#   cluster is between segmentations (cluster 1 vs cluster 1, etc.).
#   The Jensen-Shannon Divergence is a symmetric measure of similarity between
#   two probability distributions.
#   In this context, it compares the proportion of each category (e.g., education
#   levels) within matching clusters across segmentations.
#
# PDF Output
# All visualizations are saved into a single PDF, with 2 profiling variables per page for easy review.

# Load scripts ----
source('1_setup.R')

#  Install/load packages
install.packages('cvms')
install.packages('ggimage')
install.packages('rsvg')
devtools::install_github("ianmoran11/mmtable2")
library('cvms')
library('ggimage')
library('rsvg')
library('mmtable2')

# Read data -----
# Compare two segmentations (as an example, using actual vs typed segments)
# Actual segmentation solutions
urbseg <- readRDS(paste0(lca_path, "urban_outcomes_vulnerability_class.rds")) # 6 cluster for nga-2018
#rurseg <- readRDS(paste0(lca_path, "rural_outcomes_vulnerability_class.rds")) # 5 cluster for nga-2018
# Typed segmentation solution
urbseg_typed <- readRDS(paste0(cart_path, 'urban_outcomes_typing_tool_class.rds')) # 6 cluster for nga-2018
#rurseg_typed <- readRDS(paste0(cart_path, 'rural_outcomes_typing_tool_class.rds')) # 5 cluster for nga-2018

# Check that all labeled caseids are in the predicted df
stopifnot(all(urbseg$caseid %in% urbseg_typed$caseid))

# Join predicted labels with actual labeled dataframes
# Urban
urbseg_joined <- urbseg %>% left_join(urbseg_typed %>% select(caseid, pred_LCA6_class)) %>%
  # Make sure your clusters columns are factors
  mutate(across(c(LCA6_class, pred_LCA6_class), ~as.factor(.)))

rurseg_joined <- rurseg %>% left_join(rurseg_typed %>% select(caseid, pred_LCA5_class)) %>%
  # Make sure your clusters columns are factors
  mutate(across(c(LCA5_class, pred_LCA5_class), ~as.factor(.)))

# Read profiling variable names from pathways workbook
prof_vars = read_xlsx(pathways_workbook_path, sheet = 'vulnerabilities') %>%
  # To use all profiling vars, uncomment next 2 lines
  # filter(profile_include ==1,
  #       profile_strata=='both') %>%
  # To use all lca variables, uncomment next line
  filter(lca_include==1,
         # For urban, uncoment next line
         lca_strata %in% c('both', 'urban') #,
         # For rural, uncomment next line
         #lca_strata %in% c('both', 'rural')
  ) %>%
  pull(vulnerability_variable)


compare_segmentations <- function(data, seg1_col, seg2_col,
                                           vars_to_profile,
                                           output_file = "segmentation_comparison.pdf", warnings = FALSE) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gridExtra)
  library(grid)
  library(philentropy)
  library(caret)
  library(gridExtra)

  # Sankey chart
  df_alluvial <- data %>% select(all_of(c(seg1_col, seg2_col)))

  df_summary <- df_alluvial %>%
    group_by(.data[[seg1_col]], .data[[seg2_col]]) %>%
    summarise(Freq = n(), .groups = "drop")

  g <- ggplot(df_summary,
              aes(axis1 = .data[[seg1_col]], axis2 = .data[[seg2_col]], y = Freq)) +
    geom_alluvium(aes(fill = .data[[seg1_col]]), width = 1/12) +
    geom_stratum(width = 1/12, fill = "grey", color = "black") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Clustering A (Actual)", "Clustering B (Predicted)"), expand = c(.1, .1)) +
    theme_minimal() +
    labs(y = "Count", title = "Comparison of Cluster Memberships")

  # Confusion matrix
  cm <- confusionMatrix(as.factor(data[[seg1_col]]), as.factor(data[[seg2_col]]))
  tab0 <- tableGrob(round(cm$overall * 100, 2) %>% data.frame())
  evaluation <- evaluate(data %>% select(all_of(c(seg1_col, seg2_col)))  %>%
                           mutate_all(~as.character(fct_rev(.))),
                         target_col = seg1_col,
                         prediction_cols = seg2_col,
                         type = 'multinomial')


  plot0<-plot_confusion_matrix(
    evaluation[["Confusion Matrix"]][[1]],
    theme_fn = ggplot2::theme_bw)

  # Ensure factor type
  data <- data %>%
    mutate(across(all_of(c(seg1_col, seg2_col, vars_to_profile)), as.factor))

  # Compute JSD for matching clusters only
  compute_jsd <- function(data, cluster_name, var) {
    dist1 <- data %>%
      filter(.data[[seg1_col]] == cluster_name) %>%
      count(.data[[var]]) %>%
      mutate(p = n / sum(n)) %>%
      select(-n) %>%
      deframe()

    dist2 <- data %>%
      filter(.data[[seg2_col]] == cluster_name) %>%
      count(.data[[var]]) %>%
      mutate(p = n / sum(n)) %>%
      select(-n) %>%
      deframe()

    all_cats <- union(names(dist1), names(dist2))
    dist1 <- dist1[all_cats]; dist1[is.na(dist1)] <- 0
    dist2 <- dist2[all_cats]; dist2[is.na(dist2)] <- 0

    dist1 <- dist1 / sum(dist1)
    dist2 <- dist2 / sum(dist2)

    jsd <- JSD(rbind(dist1, dist2), unit = "log2")
    return(round(jsd, 2))
  }

  # Helper to create bar plots with optional JSD labels
  create_plot <- function(data, seg_col, variable, add_jsd_labels = FALSE) {
    plot_data <- data %>%
      count(.data[[seg_col]], .data[[variable]]) %>%
      group_by(.data[[seg_col]]) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()

    p <- ggplot(plot_data, aes(x = .data[[seg_col]], y = prop, fill = .data[[variable]])) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.15))) +
      labs(title = paste("Segmentation:", seg_col),
           x = "Segment",
           y = "Proportion",
           fill = "Category") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    if (add_jsd_labels) {
      clusters <- levels(data[[seg_col]])
      jsd_labels <- lapply(clusters, function(clust) {
        jsd <- compute_jsd(data, clust, variable)
        data.frame(cluster = clust, label = paste0("JSD=", jsd))
      }) %>% bind_rows()

      jsd_labels[[seg_col]] <- jsd_labels$cluster
      jsd_labels$y <- 1.05

      p <- p + geom_text(data = jsd_labels,
                         aes(x = .data[[seg_col]], y = y, label = label),
                         inherit.aes = FALSE,
                         size = 3.5)
    }

    return(p)
  }

  # Create plots
  plot_list <- list()
  for (var in vars_to_profile) {
    p1 <- create_plot(data, seg1_col, var) +
      theme_classic()
    p2 <- create_plot(data, seg2_col, var, add_jsd_labels = TRUE)+
      theme_classic()
    main_title <- paste0("Profiling Variable: ", var)
    combined_plot <- grid.arrange(p1, p2, ncol = 2,
                                  top = textGrob(main_title, gp = gpar(fontsize = 14, fontface = "bold")))
    plot_list[[var]] <- combined_plot
  }

  # Export PDF
  if(!dir.exists(paste0(root_path, user_path, 'plots/segmentation_comparison_plots/')))
    dir.create(paste0(root_path, user_path, 'plots/segmentation_comparison_plots/'))

  pdf(paste0(root_path, user_path, 'plots/segmentation_comparison_plots/',output_file), width = 14, height = 10)
  print(g)
  grid.arrange(plot0, tab0, nrow = 1, ncol = 2,
               widths = c(3, 1),
               top = textGrob("Confusion Matrix and Statistics", gp = gpar(fontsize = 18, fontface = "bold")))
  i <- 1
  while (i <= length(plot_list)) {
    if (i + 1 <= length(plot_list)) {
      grid.arrange(plot_list[[i]], plot_list[[i + 1]], nrow = 2)
    } else {
      grid.arrange(plot_list[[i]])
    }
    i <- i + 2
  }
  dev.off()
}


compare_segmentations(
  data = urbseg_joined,
  seg1_col = "LCA6_class", # k=6 columnn for urban is called LCA6_class
  seg2_col = "pred_LCA6_class", # predicted k=6 cluster column for rural is called pred_LCA6_class
  vars_to_profile = prof_vars, # vector with profiling variable names (must be in data)
  output_file = "urban_segmentation_comparison.pdf"
)

compare_segmentations(
  data = rurseg_joined,
  seg1_col = "LCA5_class", # k=5 columnn for urban is called LCA5_class
  seg2_col = "pred_LCA5_class", # predicted k=5 cluster column for rural is called pred_LCA5_class
  vars_to_profile = prof_vars, # vector with profiling variable names (must be in data)
  output_file = "rural_segmentation_comparison.pdf"
)
