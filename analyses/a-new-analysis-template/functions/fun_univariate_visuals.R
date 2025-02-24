
################################################################################
# GENERATE UNIVARIATE PLOTS
################################################################################


# ###################################
# RUN SETUP
# df <- vulnerability
# metadata <- vulnerability_sheet
# plot_path = univariate_plots_file


###################################
# DEFINE FUNCTION
###################################

gen_univariate_plots <- function(df=NULL, metadata=NULL, plot_path=NULL){


  dir.create(dirname(plot_path), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = plot_path,
            width = 15,
            height = 10,
            onefile=T)


  # OUTCOMES OR VULNERABILITY
  if (grepl("vulnerability", plot_path) == TRUE){
    input_type = "Vulnerability Factors"
  } else if (grepl("outcomes", plot_path) == TRUE){
    input_type = "Outcomes"
  } else {
    input_type = "Unknown"
  }


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0(project_name, " | ", input_type, " | Univariate distributions"), cex = 2)


  var_list <- sort(names(df)[(names(df) %in% metadata$indicator)])
  plot_list <- list()

  for (i in 1:length(var_list)){

      # try({

        var = var_list[i]
        df$variable = var
        df$value = df %>% pull(eval(parse(text=var)))

        binary.var <- all(na.omit(df$value) %in% 0:1)


        # RECODE BINARY VARIABLES AS YES NO
        if (binary.var == TRUE){

          df_plot <- df %>%
            mutate_if(is.factor, as.character) %>%
            mutate(value_plot = case_when(value==1 ~ "Yes",
                                          value==0 ~ "No"))

        } else {

          df_plot <- df %>%
            mutate_if(is.factor, as.character) %>%
            mutate(value_plot = value)

        }

        # # GET SHORT_NAME FOR PLOT LABEL
        # df_plot <- df_plot %>%
        #   base::merge(metadata, by.x=c("variable"), by.y=c("indicator"))
        #
        # plot_label = df_plot$short_name[1]
        plot_label = metadata[metadata$indicator == var, c("short_name")][[1]]


        # LOGIC FOR HANDLING NAS
        if (is.numeric(df_plot$value_plot) == TRUE){


          df_plot <- df_plot %>%
            dplyr::mutate(is_na = ifelse(is.na(value_plot), "yes", "no"),
                          value_plot = ifelse(is.na(value_plot), -1, value_plot)) %>%
            dplyr::group_by(variable, value_plot, is_na) %>%
            dplyr::mutate(count=n(),
                          sum_wt=sum(wt)) %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(total=n(),
                          prop=count/total,
                          total_wt = sum(wt),
                          prop_wt = sum_wt/total_wt) %>%
            dplyr::select(variable, value_plot, is_na, count, sum_wt, total, prop, total, total_wt, prop_wt) %>%
            distinct()

          plot <- df_plot %>%
            ggplot(aes(fill=is_na)) +
            geom_bar(aes(x=value_plot, y=prop_wt), stat="identity") +
            geom_text(aes(x=value_plot, y=prop_wt, label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 3.5, fontface = "bold") +
            geom_point(aes(x=value_plot, y=prop), shape=5, size=2, color="darkred") +
            theme_bw() +
            coord_flip() +
            ggtitle(paste0(plot_label)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
            theme(axis.title.y = element_text(size=8)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.position="none") +
            theme(aspect.ratio = .60) +
            xlab("") +
            ylab("") +
            # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
            scale_fill_brewer(palette="Dark2")
          # print(plot)


        } else if (is.character(df_plot$value_plot) == TRUE){


          df_plot <- df_plot %>%
            dplyr::mutate(value_plot = ifelse(is.na(value_plot), "*NA", value_plot)) %>%
            dplyr::group_by(variable, value_plot) %>%
            dplyr::mutate(count=n(),
                          sum_wt=sum(wt)) %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(total=n(),
                          prop=count/total,
                          total_wt = sum(wt),
                          prop_wt = sum_wt/total_wt) %>%
            dplyr::select(variable, value_plot, count, sum_wt, total, prop, total, total_wt, prop_wt) %>%
            distinct()

          plot <- df_plot %>%
            ggplot() +
            geom_bar(aes(x=str_wrap(value_plot, width=10), y=prop_wt), stat="identity", fill="deepskyblue3") +
            geom_text(aes(x=str_wrap(value_plot, width=10), y=prop_wt, label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 3.5, fontface = "bold") +
            geom_point(aes(x=str_wrap(value_plot, width=10), y=prop), shape=5, size=2, color="darkred") +
            theme_bw() +
            coord_flip() +
            ggtitle(paste0(plot_label)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
            theme(axis.title.y = element_text(size=8)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.position="none") +
            theme(aspect.ratio = .60) +
            xlab("") +
            ylab("")
            # scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
            # scale_fill_brewer(palette="Dark2")
          # print(plot)


        }

        # print(plot)


        plot_list[[i]] <- plot

      # }, silent = TRUE)

    }

  # LOOP OVER LIST OF PLOTS, PLOTTING 9 PER PAGE
  total_plots <- 1:length(plot_list)
  plot_list_sub <- split(total_plots, ceiling(seq(total_plots)/9))

  for (p in plot_list_sub){

      print(p)
      p.fin <- cowplot::plot_grid(plotlist = plot_list[p], nrow = 3, ncol = 3)
      print(p.fin)

  }

  # }

  ###################################
  dev.off()
  graphics.off()

}

