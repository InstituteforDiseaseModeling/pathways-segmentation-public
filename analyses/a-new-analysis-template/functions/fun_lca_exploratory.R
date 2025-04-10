
################################################################################
# GENERATE LCA PROFILE
################################################################################

# ###################################
# RUN SETUP
# stratum = "urban"
# n_clusters = 2
# data_path = lca_path
# plot_path = lca_plots
# lca_vars = lca_strata_input

###################################
# DEFINE FUNCTION TO GENERATE LCA PROFILE VISUALS
###################################


gen_lca_explore <- function(lca_vars=NULL, stratum=NULL, n_clusters=NULL, data_path=NULL, plot_path=NULL){

  # GET DATA
  path = paste0(lca_path, stratum, "_outcomes_vulnerability_class.rds")
  outcomes_vulnerability_class <- readRDS(path)

  # OUTCOMES
  outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))


  lca_plots_file = paste0(plot_path, stratum, "_", n_clusters, "_lca_exploratory_plots.pdf")
  # lca_plots_file = paste0(plot_path, "lca_profile_plots_lite_", stratum, "_", n_clusters, ".pdf")
  dir.create(dirname(lca_plots_file), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = lca_plots_file,
            width = 15,
            height = 10,
            onefile=T)


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0("LCA Exploratory Plots for stratum: ", stratum, " with N clusters: ", n_clusters), cex = 2)



  ###################################
  # RESPONSE PROPORTION BY CLASS


  # SUMMARIZE DATA FRAME
  df_plot <- outcomes_vulnerability_class %>%
    dplyr::mutate(cluster_col = eval(parse(text=paste0("LCA", n_clusters, "_class")))) %>%
    dplyr::select(dplyr::any_of(c("caseid", "strata", "wt", "cluster_col", lca_vars$vulnerability_variable))) %>%
    reshape2::melt(id.vars=c("caseid", "strata", "wt", "cluster_col"), variable.name = "vulnerability_variable") %>%
    dplyr::mutate(vulnerability_variable = as.character(vulnerability_variable),
           value = str_to_title(value)) %>% # str_to_sentence()
    base::merge(lca_vars, by=c("vulnerability_variable"))

  # DEFINE VARIABLE TYPE FOR PLOTTING AND CALCULATE PROPORTIONS
  df_plot <- df_plot %>%
    dplyr::group_by(vulnerability_variable) %>%
    dplyr::mutate(variable_type = case_when(all(na.omit(value) %in% c("Yes", "No")==TRUE) ~ "binomial",
                                            all(na.omit(value) %in% c(1, 0)==TRUE) ~ "binomial",
                                            TRUE ~ "multinomial")) %>%
    dplyr::mutate(value = case_when(variable_type == "binomial" & value == 1 ~ "Yes",
                                    variable_type == "binomial" & value == 0 ~ "No",
                                    TRUE ~ value)) %>%
    dplyr::group_by(cluster_col, vulnerability_variable, value) %>%
    dplyr::mutate(count = n(),
                  sum_wt=sum(wt)) %>%
    dplyr::group_by(cluster_col, vulnerability_variable) %>%
    dplyr::mutate(total=n(),
                  prop=round(count/total, 3),
                  total_wt = sum(wt),
                  prop_wt = round(sum_wt/total_wt, 3)) %>%
    arrange(cluster_col, vulnerability_variable)


  ###################################
  # HEAT MAP FOR BINARY VARIABLES | STACKED BAR FOR MULTINOMIAL

  var_list <- unique(df_plot$vulnerability_variable)
  plot_list <- list()


  # GENERATE PLOTS
  for (i in 1:length(var_list)){

    var = var_list[i]
    df_plot1 <- df_plot %>%
      dplyr::filter(vulnerability_variable == var)

    if (df_plot1$variable_type[[1]] == "binomial"){

      df_plot1 <- df_plot1 %>%
        dplyr::select(variable_type, cluster_col, vulnerability_variable, short_name, value, prop_wt) %>%
        distinct() %>%
        reshape2::dcast(variable_type + cluster_col + vulnerability_variable + short_name ~ value, value.var = "prop_wt") %>%
        reshape2::melt(id.vars = c("variable_type", "cluster_col", "vulnerability_variable", "short_name"), variable.name = c("value"), value.name = "prop_wt") %>%
        mutate(prop_wt = ifelse(is.na(prop_wt), 0, prop_wt))

      plot <- df_plot1 %>%
        dplyr::filter(value == "Yes") %>%
        ggplot(aes(x=cluster_col, y=vulnerability_variable, fill=prop_wt)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%1.0f%%", prop_wt*100))) +
        theme_bw() +
        ggtitle(paste0(df_plot1$short_name[[1]])) +
        theme(plot.title = element_text(size=10)) +
        theme(legend.text=element_text(size=6), legend.title=element_text(size=8)) +
        # theme(axis.title.y = element_blank()) +
        theme(axis.text.y = element_blank()) +
        theme(axis.ticks.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(aspect.ratio = .60) +
        # guides(fill = "none") +
        labs(y = "Proportion Yes") +
        # scale_fill_gradient2(low = "mediumseagreen", mid = "ivory", high = "coral", midpoint = 0.5,breaks=seq(0,1,0.1)) +
        scale_fill_gradient2(low = "white", high = "steelblue", name = "Prop Yes")
      # print(plot)

    } else if (df_plot1$variable_type[[1]] == "multinomial"){

      plot <- df_plot1 %>%
        dplyr::select(variable_type, cluster_col, vulnerability_variable, short_name, value, prop_wt, prop) %>%
        distinct() %>%
        ggplot(aes(fill=value)) +
        geom_bar(aes(x=cluster_col, y=prop_wt), stat="identity") +
        # geom_text(aes(x=cluster_col, y=prop_wt, label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 3.5, fontface = "bold") +
        # geom_point(aes(x=cluster_col, y=prop), shape=5, size=2, color="black") +
        # ggplot(aes(x=cluster_col, y=prop_wt, fill=value)) +
        # geom_bar(position="stack", stat="identity") +
        theme_bw() +
        labs(fill="") +
        ggtitle(paste0(df_plot1$short_name[[1]])) +
        theme(plot.title = element_text(size=10)) +
        theme(legend.text=element_text(size=6), legend.title=element_text(size=8)) +
        # theme(axis.title.y = element_text(size=8)) +
        theme(axis.title.y = element_blank()) +
        theme(aspect.ratio = .60) +
        xlab("") +
        scale_fill_brewer(palette="Set2", labels = function(x) str_wrap(x, width = 10))
      # print(plot)

    }

    plot_list[[i]] <- plot

  }

  # LOOP OVER LIST OF PLOTS, PLOTTING 9 PER PAGE
  total_plots <- 1:length(plot_list)
  plot_list_sub <- split(total_plots, ceiling(seq(total_plots)/9))

  for (p in plot_list_sub){

    print(p)
    p.fin <- cowplot::plot_grid(plotlist = plot_list[p], ncol = 3)
    print(p.fin)

  }

  ###################################
  dev.off()
  graphics.off()

}

