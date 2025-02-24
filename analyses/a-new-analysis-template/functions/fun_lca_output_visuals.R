


################################################################################
# GENERATE LCA OUTPUT VISUALS
################################################################################


###################################
# DEFINE FUNCTION TO GENERATE LCA OUTPUT VISUALS
###################################

gen_lca_output_viz <- function(stratum=NULL, data_path=NULL, plot_path=NULL){


  # GET outcomes_vulnerability_class
  path = paste0(data_path, stratum, "_outcomes_vulnerability_class.rds")
  outcomes_vulnerability_class <- readRDS(path)
  nobs = nrow(outcomes_vulnerability_class)


  # GET LCA outputs
  for (i in 2:10){

    name <- paste0("LCA", i)
    path = paste0(data_path, stratum, "_", name, ".rds")
    data <- readRDS(path)
    assign(name, data)

  }


  lca_plots_file = paste0(plot_path, stratum, "_lca_output_plots.pdf")
  dir.create(dirname(lca_plots_file), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = lca_plots_file,
            width = 15,
            height = 10,
            onefile=T)


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0("LCA Output Plots for stratum: ", stratum, " N = ", nobs), cex = 2)


  ###################################
  # PLOT COMPARING SOLUTION BIC
  bic_data <- data.frame(bic = c(LCA2$bic, LCA3$bic, LCA4$bic, LCA5$bic, LCA6$bic, LCA7$bic, LCA8$bic, LCA9$bic, LCA10$bic),
                         labels = factor(c("2-class", "3-class", "4-class", "5-class", "6-class", "7-class", "8-class", "9-class", "10-class"),
                                         levels=c("2-class", "3-class", "4-class", "5-class", "6-class", "7-class", "8-class", "9-class", "10-class")))

  plot1 <- bic_data %>%
    ggplot(aes(x = labels, y = bic)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    theme_bw() +
    xlab("K-Class Solution") +
    ylab("BIC") +
    ggtitle(paste0("BIC for: ", stratum))
  # print(plot1)


  ###################################
  # PLOT DISTRIBUTION OF MAXIMUM LIKELIHOODS
  df_lca2_attempts <- data.frame(var = LCA2$attempts) %>%
    mutate(model = "2-class")

  df_lca3_attempts <- data.frame(var = LCA3$attempts) %>%
    mutate(model = "3-class")

  df_lca4_attempts <- data.frame(var = LCA4$attempts) %>%
    mutate(model = "4-class")

  df_lca5_attempts <- data.frame(var = LCA5$attempts) %>%
    mutate(model = "5-class")

  df_lca6_attempts <- data.frame(var = LCA6$attempts) %>%
    mutate(model = "6-class")

  df_lca7_attempts <- data.frame(var = LCA7$attempts) %>%
    mutate(model = "7-class")

  df_lca8_attempts <- data.frame(var = LCA8$attempts) %>%
    mutate(model = "8-class")

  df_lca9_attempts <- data.frame(var = LCA9$attempts) %>%
    mutate(model = "9-class")

  df_lca10_attempts <- data.frame(var = LCA10$attempts) %>%
    mutate(model = "10-class")

  mle_attempts <- rbind(df_lca2_attempts, df_lca3_attempts, df_lca4_attempts, df_lca5_attempts, df_lca6_attempts, df_lca7_attempts,
                        df_lca8_attempts, df_lca9_attempts, df_lca10_attempts) %>%
    mutate(model = factor(model, levels=c("2-class", "3-class", "4-class", "5-class", "6-class", "7-class", "8-class", "9-class", "10-class")))

  plot4 <- mle_attempts %>%
    ggplot(aes(x=var)) +
    geom_histogram() +
    facet_wrap(~model, ncol = 2, scales = "free") +
    theme_bw() +
    xlab("K-Class Solution") +
    labs(fill="") +
    ggtitle(paste0("Distibution of maximum likelihoods"))
  # print(plot4)


  ###################################
  # POSTERIOR PROBABILITY WITHIN EACH MODEL
  LCA2_posterior <- as.data.frame(LCA2$posterior) %>%
    cbind(pred_class=LCA2$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "2-class",
                     avg_prob = mean(prob))

  LCA3_posterior <- as.data.frame(LCA3$posterior) %>%
    cbind(pred_class=LCA3$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "3-class",
                     avg_prob = mean(prob))

  LCA4_posterior <- as.data.frame(LCA4$posterior) %>%
    cbind(pred_class=LCA4$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "4-class",
                     avg_prob = mean(prob))

  LCA5_posterior <- as.data.frame(LCA5$posterior) %>%
    cbind(pred_class=LCA5$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "5-class",
                     avg_prob = mean(prob))

  LCA6_posterior <- as.data.frame(LCA6$posterior) %>%
    cbind(pred_class=LCA6$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "6-class",
                     avg_prob = mean(prob))

  LCA7_posterior <- as.data.frame(LCA7$posterior) %>%
    cbind(pred_class=LCA7$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "7-class",
                     avg_prob = mean(prob))

  LCA8_posterior <- as.data.frame(LCA8$posterior) %>%
    cbind(pred_class=LCA8$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "8-class",
                     avg_prob = mean(prob))

  LCA9_posterior <- as.data.frame(LCA9$posterior) %>%
    cbind(pred_class=LCA9$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "9-class",
                     avg_prob = mean(prob))

  LCA10_posterior <- as.data.frame(LCA10$posterior) %>%
    cbind(pred_class=LCA10$predclass) %>%
    mutate(id = row_number()) %>%
    reshape2::melt(id.vars=c("id", "pred_class"), variable.name="cluster", value.name = "prob") %>%
    group_by(id) %>%
    mutate(max_prob = max(prob)) %>%
    dplyr::filter(prob == max_prob) %>%
    group_by() %>%
    dplyr::summarize(labels = "10-class",
                     avg_prob = mean(prob))

  post_data <- rbind(LCA2_posterior, LCA3_posterior, LCA4_posterior, LCA5_posterior, LCA6_posterior, LCA7_posterior, LCA8_posterior, LCA9_posterior, LCA10_posterior) %>%
    mutate(labels = factor(c("2-class", "3-class", "4-class", "5-class", "6-class", "7-class", "8-class", "9-class", "10-class"),
                           levels=c("2-class", "3-class", "4-class", "5-class", "6-class", "7-class", "8-class", "9-class", "10-class")))

  plot3 <- post_data %>%
    ggplot(aes(x = labels, y = avg_prob)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    theme_bw() +
    xlab("K-Class Solution") +
    ylab("Mean Post Prob") +
    ylim(0, 1) +
    ggtitle(paste0("Mean post of prob of assigned segment"))


  ###################################
  # PLOT ALL TOGETHER
  grid.arrange(arrangeGrob(plot1, plot3), plot4, ncol=2)


  ###################################
  # PROPORTION OF SAMPLE IN EACH SEGMENT


  df_plot <- outcomes_vulnerability_class %>%
    dplyr::select(caseid, survey, strata, wt, LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class, LCA8_class, LCA9_class, LCA10_class) %>%
    reshape2::melt(id.vars=c("caseid", "survey", "strata", "wt"), variable.name="n_segments", value.name = "segment") %>%
    group_by(strata, n_segments, segment) %>%
    mutate(count = sum(wt)) %>%
    group_by(strata, n_segments) %>%
    mutate(total = sum(wt)) %>%
    dplyr::select(strata, n_segments, segment, count, total) %>%
    distinct() %>%
    mutate(prop = round((count/total), 3))


  plot_prop <- df_plot %>%
    # mutate(n_segments = factor(n_segments, levels=c("LCA7_class", "LCA6_class", "LCA5_class", "LCA4_class", "LCA3_class", "LCA2_class"))) %>%
    mutate(n_segments = factor(str_sub(n_segments, 4), levels=c("2_class", "3_class", "4_class", "5_class", "6_class", "7_class", "8_class", "9_class", "10_class"))) %>%
    ggplot(aes(fill=as.factor(segment), y=prop, x=n_segments)) +
    geom_bar(position="fill", stat="identity") +
    geom_text(aes(label=sprintf("%1.0f%%", prop*100)), size = 3, position = position_stack(vjust = 0.5)) +
    theme_bw() +
    # coord_flip() +
    xlab("K-Class Solution") +
    labs(fill="") +
    theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
    scale_fill_brewer(palette="Paired") +
    ggtitle(paste0("Survey weighted class proportion by number of classes for stratum: ", stratum))
  print(plot_prop)


  ###################################
  # MAKE SANKEY PLOT (GGPLOT)
  sankey_title = paste0("Segment membership across segmentation solutions: ", stratum)


  data <- outcomes_vulnerability_class %>%
    dplyr::select(caseid, LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class,LCA8_class, LCA9_class, LCA10_class) %>%
    ggsankey::make_long(LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class,LCA8_class, LCA9_class, LCA10_class)

  plot_alluvial <- data %>%
    ggplot(aes(x = x,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(node),
               label = node)) +
    geom_alluvial(flow.alpha = 0.5,
                  node.color = 1,
                  space=0,
                  show.legend = FALSE) +
    geom_alluvial_text(size = 3, color = "white") +
    scale_fill_brewer(palette="Paired") +
    # scale_fill_viridis_d(drop = FALSE) +
    theme_alluvial(base_size = 18) +
    xlab("") +
    ylab("") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = .5)) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0)) +
    ggtitle(sankey_title)
  # print(plot_alluvial)


  ###################################
  # MAKE SANKEY PLOT (NETWORKD3 - HTML OUTPUT)


  data <- outcomes_vulnerability_class %>%
    dplyr::select(LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class) %>%
    setNames(c("cluster_k2", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "cluster_k7")) %>%
    as.data.table()

  links <- lapply(2:6, function(i){

    link.i<-  data[,.(value = .N),.(source = paste0("k", i, "_", base::get(paste0("cluster_k", i))), target = paste0("k", i+1, "_", base::get(paste0("cluster_k", i+1))))]

    return(link.i)
  }) %>% rbindlist(use.names = T, fill = T)

    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(
      name=c(as.character(links$source),
             as.character(links$target)) %>% unique()
    )

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     sinksRight=FALSE)
  sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h1(sankey_title))
  sankey

  html_path = paste0(plot_path, stratum, "_lca_sankey.html")
  png_path = paste0(plot_path, stratum, "_lca_sankey.png")

  saveNetwork(sankey, html_path)
  webshot::webshot(html_path, png_path)

  img <- image_read(png_path)
  plot(img)
  # title(sankey_title)

  if (file.exists(png_path)){
    file.remove(png_path)
  }


  ###################################
  dev.off()
  graphics.off()


}
