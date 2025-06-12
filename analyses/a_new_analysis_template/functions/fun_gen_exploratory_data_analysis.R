


################################################################################
# GENERATE EXPLORATORY DATA ANALYSIS
################################################################################


###################################
# RUN SETUP
# outcomes.list <- c("u5mort.yn")
# out <- "u5mort.yn"
# measure = "slum.sum"
# df = outcomes_vulnerability
# plot_path = exploratory_plots
# stratum = "rural"


###################################
# DEFINE FUNCTION
###################################

fun_gen_exploratory_data_analysis <- function(df=NULL, outcomes.list=NULL, measure=NULL, strata=NULL, plot_path){


  # GET VARIABLE METADATA FOR PLOTTING
  outcome_metadata <- readRDS(outcomes_excel_file)
  vulnerability_metadata <- readRDS(vulnerability_excel_file)


  ###################################
  # SET EXPLANATORY VARIABLE
  df$var <- df %>% pull(eval(parse(text=measure)))


  # DEFINE DATA TYPE FOR EXPLANATORY VARIABLE
  exp.numeric <- is.numeric(df$var)
  exp.character <- is.character(df$var) | is.factor(df$var)


  # CONVERT CHARACTER TO FACTOR AND ORDER FACTOR LEVELS BY LARGEST TO SMALLEST FOR CONSISTENT INTERPRETATION OF P-VALUES
  if (exp.character == TRUE){

    df$var <- as.factor(df$var)
    df$var <- forcats::fct_infreq(df$var)
    ref_cat <- levels(df$var)[1]

  } else {

    ref_cat <- NA

  }


  ###################################
  eda_plots_file = paste0(plot_path, "eda_", measure, ".pdf")
  dir.create(dirname(eda_plots_file), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = eda_plots_file,
            width = 15,
            height = 10,
            onefile=T)


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0(project_name, " | EDA for: ", measure), cex = 2)


  for (out in outcomes.list){

    tryCatch({

      print(paste0(out, " ~ ", measure))


      ###################################
      # SET OUTCOME VARIABLE
      df$outcome <- df %>% pull(eval(parse(text=out)))


      # DEFINE DATA TYPE FOR OUTCOME VARIABLE
      out.binary <- (all(c(0, 1) %in% na.omit(df$outcome)) & all(na.omit(df$outcome) %in% c(0, 1))) |
        (all(c("Yes", "No") %in% str_to_title(na.omit(df$outcome)) & all(str_to_title(na.omit(df$outcome)) %in% c("Yes", "No"))))
      out.numeric <- is.numeric(df$outcome) & out.binary == FALSE
      out.character <- (is.character(df$outcome) | is.factor(df$outcome)) & out.binary == FALSE


      ###################################
      # BIVARIATE REGRESSION

      model_pred_probs <- data.frame()
      model_outputs <- data.frame()
      df_exp_plot <- data.frame()

      for (stratum in strata){


        df1 <- df %>% dplyr::filter(strata == stratum) %>%
          dplyr::select(caseid, all_of(svy_id_var), all_of(svy_strata_var), wt, strata, outcome, var)


        # DEFINE SURVEY OBJECT
        if (config::get("use_svy_design") == FALSE){

          df.design <- svydesign(id=~1, strata=NULL, weights=~wt, data=df1)

        } else {

          df.design <- svydesign(id=~eval(parse(text = svy_id_var)), strata=~eval(parse(text = svy_strata_var)), weights=~wt, data=df1, nest=TRUE)

        }


        ###################################
        # CREATE MODEL OUTPUT FOR PLOTTING


        # OUTCOME VARIABLE IS BINARY
        if (out.binary == TRUE){


          df1$outcome <- as.factor(df1$outcome)
          res_glm <- svyglm(outcome ~ var, family=quasibinomial, design=df.design, data=df1)


          # VULNERABILITY VARIABLE IS NUMERIC
          if (exp.numeric == TRUE){


            pred_prob <- data.frame(var = unique(na.omit(df$var)),
                                    p.value = tidy(res_glm)$p.value[[2]])

            model_output <- data.frame(var = names(coef(res_glm)),
                                       p.value = tidy(res_glm)$p.value,
                                       estimate = coef(res_glm),
                                       log_odds = coef(res_glm),
                                       log_odds_se = summary(res_glm)$coefficients[, "Std. Error"])

            model_output <- model_output %>%
              dplyr::filter(var == "var") %>%
              dplyr::mutate(var = ifelse(var == "var", measure, var),
                            lower_log_odds = log_odds - 1.96 * log_odds_se,
                            upper_log_odds = log_odds + 1.96 * log_odds_se,
                            estimate = exp(log_odds),
                            lower_ci = exp(lower_log_odds),
                            upper_ci = exp(upper_log_odds),
                            model = stratum)
                            # DNF_status = status)

            tbl_text = paste0("Binary Outcome variable: ", out, " and numeric Vulnerability variable: ", measure, ". Odds ratio displayed.")


          # VULNERABILITY VARIABLE IS CATEGORICAL
          } else if (exp.character == TRUE){


            pred_prob <- data.frame(var = res_glm$xlevels[[1]],
                                    p.value = tidy(res_glm)$p.value)


            model_output <- data.frame(var = res_glm$xlevels[[1]],
                                       p.value = tidy(res_glm)$p.value,
                                       log_odds = coef(res_glm),
                                       log_odds_se = summary(res_glm)$coefficients[, "Std. Error"])

            model_output <- model_output %>%
              dplyr::mutate(lower_log_odds = log_odds - 1.96 * log_odds_se,
                            upper_log_odds = log_odds + 1.96 * log_odds_se,
                            estimate = exp(log_odds),
                            lower_ci = exp(lower_log_odds),
                            upper_ci = exp(upper_log_odds),
                            model = stratum,
                            # DNF_status = status,
                            ref_cat_class = case_when(var == ref_cat ~ "Yes",
                                                      TRUE ~ "No"))

            tbl_text = paste0("Binary Outcome variable: ", out, " and categorical Vulnerability variable: ", measure, ". Odds ratio displayed.")


          }


        # OUTCOME VARIABLE IS NUMERIC
        } else if (out.numeric == TRUE){


          res_glm <- svyglm(outcome ~ var, family=gaussian(link ="identity"), design=df.design, data=df1)


          # VULNERABILITY VARIABLE IS NUMERIC
          if (exp.numeric == TRUE){


            pred_prob <- data.frame(var = unique(na.omit(df$var)),
                                    p.value = tidy(res_glm)$p.value[[2]])

            model_output <- data.frame(var = names(coef(res_glm)),
                                       estimate = coef(res_glm),
                                       se = summary(res_glm)$coefficients[, "Std. Error"],
                                       p.value = tidy(res_glm)$p.value[[2]])

            model_output <- model_output %>%
              dplyr::filter(var == "var") %>%
              dplyr::mutate(var = ifelse(var == "var", measure, var),
                            lower_ci = estimate - 1.96*se,
                            upper_ci = estimate + 1.96*se,
                            model = stratum)
                            # DNF_status = status)

            tbl_text = paste0("Numeric Outcome variable: ", out, " and numeric Vulnerability variable: ", measure, ". Regression coefficient displayed.")


          # VULNERABILITY VARIABLE IS CATEGORICAL
          } else if (exp.character == TRUE){


            pred_prob <- data.frame(var = res_glm$xlevels[[1]],
                                    p.value = tidy(res_glm)$p.value)


            model_output <- data.frame(var = res_glm$xlevels[[1]],
                                       p.value = tidy(res_glm)$p.value,
                                       estimate = coef(res_glm),
                                       se = summary(res_glm)$coefficients[, "Std. Error"])

            model_output <- model_output %>%
              dplyr::mutate(lower_ci = estimate - 1.96 * se,
                            upper_ci = estimate + 1.96 * se,
                            model = stratum,
                            # DNF_status = status,
                            ref_cat_class = case_when(var == ref_cat ~ "Yes",
                                                      TRUE ~ "No"))

            tbl_text = paste0("Numeric Outcome variable: ", out, " and categorical Vulnerability variable: ", measure, ". Regression coefficient displayed.")


          }


        }


        ###################################
        # MODEL FIT ERROR HANDLING


        if (length(res_glm$coefficients[exp(res_glm$coefficients) > 100])>0  |
            length(res_glm$coefficients[exp(res_glm$coefficients) < -100])>0){

          coefname <- names(res_glm$coefficients)[exp(res_glm$coefficients) > 100 | exp(res_glm$coefficients) < -100]
          coefname <- sub("var", "", coefname)
          coefname <- paste0("[", coefname, "]")
          res_glm$coefficients[exp(res_glm$coefficients) > 100 | exp(res_glm$coefficients) < -100] <- 0
          status = paste0(stratum, " model did not fit - level: ", coefname, " has too few observations\n")
          print(status)

        } else {

          status = paste0(stratum, " model successfully fit")

        }


        # COLLAPSE CHARACTER VECTOR AND RENAME
        status <- str_c(status, collapse = " | ")
        assign(paste0("status_", stratum), status)


        # CREATE FIT STATUS OBJECTS FOR PLOTTING
        plot_text <- grepl(" model did not fit", status)
        plot_text <- ifelse(any(plot_text), paste(status, collapse = ""),"")
        assign(paste0("plot_text_", stratum), plot_text)


        ###################################


        # CREATE DF FOR PREDICTED PROBABILITIES
        # pred_prob <- model_output %>% dplyr::select(var)
        probabilities <- predict(res_glm, pred_prob, type = "response", se.fit=T, na.action = na.omit)
        prob1 <- as.data.frame(probabilities)
        prob1$lower <- prob1$response - (1.96*prob1$SE)
        prob1$upper <- prob1$response + (1.96*prob1$SE)
        pred_prob <- cbind(pred_prob, prob1) %>%
          dplyr::mutate(model = stratum,
                        DNF_status = status)


        # RENAME FOR MODEL PLOTTING
        assign(paste0("pred_prob_", stratum), pred_prob)
        assign(paste0("model_output_", stratum), model_output)

        # BIND TOGETHER
        model_pred_probs <- rbind(base::get(paste0("pred_prob_", stratum)), model_pred_probs)
        model_outputs <- rbind(base::get(paste0("model_output_", stratum)), model_outputs)
        df_exp_plot <- rbind(df_exp_plot, df1)

      }


      ###################################
      # GET PLOTTING LABELS
      outcome_label <- outcome_metadata[outcome_metadata$outcome_variable==out, "short_name"][[1]]
      measure_label <- vulnerability_metadata[vulnerability_metadata$vulnerability_variable==measure, "short_name"][[1]]

      model_plot_text <- paste0(
        if (exists("plot_text_rural")) plot_text_rural else "",
        if (exists("plot_text_urban")) plot_text_urban else ""
      )


      ###################################
      # PLOT MODEL OUTPUT


      # DEFINE COLORS FOR URBAN | RURAL
      plot.color <- c(rural = "#0072B2", urban = "#E69F00")


      # PLOT MODEL PROBABILITIES

      if (out.binary == TRUE | out.character == TRUE){


        plot1 <- model_pred_probs %>%
          ggplot(aes(
            y = var,
            x = response,
            xmin = lower,
            xmax = upper,
            color = model
          )) +
          geom_point(size = 3,
                     position = position_dodge(width = 0.5)) +
          geom_errorbar(width = 0.2,
                        position = position_dodge(width = 0.5)) +
          geom_vline(xintercept = 0.5, linetype = "dotted") +
          theme_bw() +
          theme(legend.position = "bottom") +
          scale_color_manual(values=plot.color) +
          ylab("") +
          xlab("Predicted Probabilities and 95% Confidence Intervals") +
          labs(color = "Strata") +
          xlim(0, 1) +
          scale_shape_discrete(guide = if (!is.na(ref_cat)) "legend" else "none") +
          annotate("text",  x=Inf, y=Inf, label = model_plot_text, vjust=1.5, hjust=1.1, color="darkblue")+
          ggtitle(paste0(outcome_label, " ~ ", measure_label))


    } else if (out.numeric == TRUE){


        plot1 <- model_pred_probs %>%
          ggplot(aes(
            y = var,
            x = response,
            xmin = lower,
            xmax = upper,
            color = model
          )) +
          geom_point(size = 3,
                     position = position_dodge(width = 0.5)) +
          geom_errorbar(width = 0.2,
                        position = position_dodge(width = 0.5)) +
          geom_vline(xintercept = 0.5, linetype = "dotted") +
          theme_bw() +
          theme(legend.position = "bottom") +
          scale_color_manual(values=plot.color) +
          ylab("") +
          xlab("Predicted Probabilities and 95% Confidence Intervals") +
          labs(color = "Strata") +
          scale_shape_discrete(guide = if (!is.na(ref_cat)) "legend" else "none") +
          annotate("text",  x=Inf, y=Inf, label = model_plot_text, vjust=1.5, hjust=1.1, color="darkblue")+
          ggtitle(paste0(outcome_label, " ~ ", measure_label))


    }


      # CREATE TABLE OF MODEL OUTPUT
      plot2 <- model_outputs %>%
        dplyr::mutate(sig = case_when(p.value < 0.001 ~ "***",
                                      p.value < 0.01 ~ "**",
                                      p.value < 0.05 ~ "*",
                                      TRUE ~ ""),
               statistic = paste0(round(estimate, 2), sig, "  [", round(lower_ci, 2), ", ", round(upper_ci, 2), "] "),
               statistic = case_when(var == ref_cat ~ "Ref",
                                     is.na(ref_cat) ~ statistic,
                                     TRUE ~ statistic)) %>%
        reshape2::dcast(var ~ model, value.var = "statistic") %>%
        arrange(desc(row_number()))

      plot2 <- rbind(head(plot2), tail(plot2)) %>% distinct()

      tbl1 <- tableGrob(plot2,
                        rows = NULL,
                        theme = ttheme_minimal(
                          core = list(fg_params = list(fontface = "plain"),
                                      bg_params = list(col = "black", lwd = 1)),  # Black border
                          colhead = list(fg_params = list(fontface = "bold"),
                                         bg_params = list(col = "black", lwd = 1)) # Border for headers
                        ))


      tbl1 <- arrangeGrob(
        tbl1,
        top = textGrob(tbl_text,
                       gp = gpar(fontsize = 8, fontface = "bold")
        ))

      # grid.draw(tbl1)


      ###################################
      # BAR CHART TO SHOW EXPLANATORY VARIABLE DISTRIBUTION


      if (exp.numeric == TRUE) {


        # STRATIFIED SAMPLE DISTRIBUTION OF EXPLANATORY VARIABLE
        df_plot <- df_exp_plot %>%
          mutate(is_na = ifelse(is.na(var), "yes", "no"),
                 outcome = ifelse(is.na(var), -1, var)) %>%
          group_by(strata, var, is_na) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = sum(ifelse(is_na == "yes", 0, count)),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot.color <- setNames(
          ifelse(grepl("urban", unique(df_plot$strata), ignore.case = TRUE), "#E69F00", "#0072B2"),
          unique(df_plot$strata)
        )


        plot3 <- df_plot %>%
          ggplot(aes(x=var, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .60) +
          xlab("") +
          ylab("") +
          # ylim(0, 1) +
          # scale_fill_brewer(palette="Dark2") +
          scale_fill_manual(values=plot.color) +
          ggtitle(paste0("Survey weighted distribution of vulnerability variable: ", measure_label))
        # print(plot3)


      } else if (exp.character == TRUE){


        # STRATIFIED SAMPLE DISTRIBUTION OF EXPLANATORY VARIABLE
        df_plot <- df_exp_plot %>%
          dplyr::mutate(var = as.character(var)) %>%
          dplyr::mutate(var = ifelse(is.na(var), "*NA", var)) %>%
          group_by(strata, var) %>%
          dplyr::mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          dplyr::mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = ifelse(var == "*NA", 0, count)) %>%
          dplyr::select(strata, var, count, sum_wt, total, prop, total_wt, prop_wt, sample_size) %>%
          distinct() %>%
          group_by(strata) %>%
          dplyr::mutate(sample_size = sum(sample_size),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot.color <- setNames(
          ifelse(grepl("urban", unique(df_plot$strata), ignore.case = TRUE), "#E69F00", "#0072B2"),
          unique(df_plot$strata)
        )


        plot3 <- df_plot %>%
          ggplot(aes(x=var, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .60) +
          xlab("") +
          ylab("") +
          ylim(0, 1) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE)) +
          # scale_fill_brewer(palette="Dark2") +
          scale_fill_manual(values=plot.color) +
          ggtitle(paste0("Survey weighted distribution of vulnerability variable: ", measure_label))
        # print(plot3)


      }


      # BAR CHART TO SHOW OUTCOME VARIABLE DISTRIBUTION
      if (out.binary == TRUE){

        df_plot <- df_exp_plot %>%
          mutate(outcome = case_when(outcome == 1 ~ "Yes",
                                     outcome == 0 ~ "No",
                                     is.na(outcome) ~ "*NA",
                                     TRUE ~ outcome)) %>%
          group_by(strata, outcome) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = ifelse(outcome == "*NA", 0, count)) %>%
          dplyr::select(strata, outcome, count, sum_wt, total, prop, total_wt, prop_wt, sample_size) %>%
          distinct() %>%
          group_by(strata) %>%
          mutate(sample_size = sum(sample_size),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot.color <- setNames(
          ifelse(grepl("urban", unique(df_plot$strata), ignore.case = TRUE), "#E69F00", "#0072B2"),
          unique(df_plot$strata)
        )


        plot4 <- df_plot %>%
          ggplot(aes(x=outcome, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .20) +
          xlab("") +
          ylab("") +
          ylim(0, 1) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE)) +
          # scale_fill_brewer(palette="Dark2") +
          scale_fill_manual(values=plot.color) +
          ggtitle(paste0("Survey weighted distribution of health outcome: ", outcome_label))
        # print(plot4)


      } else if (out.numeric == TRUE){


        df_plot <- df_exp_plot %>%
          mutate(is_na = ifelse(is.na(outcome), "yes", "no"),
                 outcome = ifelse(is.na(outcome), -1, outcome)) %>%
          group_by(strata, outcome, is_na) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = ifelse(is_na == "yes", 0, count)) %>%
          dplyr::select(strata, outcome, is_na, count, sum_wt, total, prop, total_wt, prop_wt, sample_size) %>%
          distinct() %>%
          group_by(strata) %>%
          mutate(sample_size = sum(sample_size),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot.color <- setNames(
          ifelse(grepl("urban", unique(df_plot$strata), ignore.case = TRUE), "#E69F00", "#0072B2"),
          unique(df_plot$strata)
        )


        plot4 <- df_plot %>%
          ggplot(aes(x=outcome, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .60) +
          xlab("") +
          ylab("") +
          # ylim(0, 1) +
          # scale_fill_brewer(palette="Dark2") +
          scale_fill_manual(values=plot.color) +
          ggtitle(paste0("Survey weighted distribution of health outcome: ", outcome_label))
        # print(plot4)


      }


      ###################################
      # PLOT TOGETHER
      grid.arrange(plot1,
                   arrangeGrob(plot4, plot3, tbl1),
                   ncol=2
                   # widths = c(2,1),
      )

    }, error = function(e) {
      message(paste0(out, " ~ ", measure, " did not fit; check values for Outcome and Vulnerability variable"))
    }

    )

  }


  ###################################
  dev.off()
  graphics.off()

}


















