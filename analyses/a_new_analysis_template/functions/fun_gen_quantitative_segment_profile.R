

################################################################################
# GENERATE SEGMENT PROFILES
################################################################################


# stratum = "urban"
# n_class = "LCA6_class"
# shp_file = shp_file


###################################
# DEFINE FUNCTION TO GENERATE SEGMENT PROFILES
###################################

fun_gen_quantitative_segment_profile <- function(df=NULL, stratum=NULL, n_class=NULL, shp_file=NULL){

  ###################################
  # GET DATA


  # GET DOMAINS
  domain_set <- params_sheet %>%
    dplyr::select(domains) %>%
    dplyr::filter(!is.na(domains)) %>%
    distinct() %>%
    dplyr::mutate(domains = str_replace_all(domains, " ", "."))


  # GET OUTCOME LIST
  outcome_vars <- readRDS(outcomes_excel_file)

  outcome_vars_profile <- outcome_vars %>%
    dplyr::filter(profile_include == 1) %>%
    dplyr::select(outcome_variable, short_name, category) %>%
    setNames(c("variable", "short_name", "category")) %>%
    distinct()

  if (nrow(outcome_vars_profile) == 0){
    stop("No outcome variables selected for quantitative segment profiling. Select variables in the Pathways Workbook.")
  }


  # GET VULNERABILITY LIST
  vulnerability_vars <- readRDS(vulnerability_excel_file)

  vulnerability_vars_profile <- vulnerability_vars %>%
    dplyr::filter(profile_include == 1) %>%
    dplyr::filter(profile_strata %in% c("both", "all", stratum)) %>%
    dplyr::select(vulnerability_variable, short_name, profile_strata, domain_set$domains) %>%
    reshape2::melt(id.vars=c("vulnerability_variable", "short_name", "profile_strata"), variable.name="domain", value.name="domain_include") %>%
    dplyr::filter(domain_include == 1) %>%
    dplyr::select(vulnerability_variable, short_name, domain) %>%
    setNames(c("variable", "short_name", "domain")) %>%
    distinct()

  if (nrow(vulnerability_vars_profile) == 0){
    stop("No vulnerability variables selected for quantitative segment profiling. Select variables in the Pathways Workbook.")
  }


  # # GET COMBINED DATASET WITH MODELED SEGMENTS
  # path = paste0(lca_path, stratum, "_outcomes_vulnerability_class_ranked.rds")
  # df <- readRDS(path)
  ###################################


  df$segment <- df %>% pull(eval(parse(text=n_class)))
  df$model_cat <- n_class

  n_segments <- length(unique(df$segment))
  for (n in seq(1, n_segments)){

    col = paste0("seg", n)
    df[[col]] <- ifelse(df$segment==n, 1, 0)

  }


  # DATA PREP
  outcome_names <- names(df)[(names(df) %in% c("caseid", "wt", "survey", "strata", "model_cat", "segment", outcome_vars_profile$variable))]
  outcomes <- subset(df, select=c(outcome_names))

  vulnerability_names <- names(df)[(names(df) %in% c("caseid", "wt", "survey", "strata", "model_cat", "segment", vulnerability_vars_profile$variable))]
  vulnerability <- subset(df, select=c(vulnerability_names))


  # STD DIFFERENCES
  outcomes_melt <- outcomes %>%
    reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    base::merge(outcome_vars_profile, by=c("variable")) %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(variable) %>%
    mutate(value_std = (value - mean(value, na.rm=TRUE))/sd(value, na.rm=TRUE),
           var_std_mean = mean(value_std)) %>%
    group_by(segment, variable) %>%
    mutate(var_class_std_mean = mean(value_std),
           sd = sd(value_std),
           se = sd/sqrt(n()),
           t_score = qt(p=0.05/2, df=n()-1, lower.tail = FALSE),
           me = t_score * se,
           lower_bound = var_class_std_mean - me,
           upper_bound = var_class_std_mean + me)


  # MEAN RANK OF STD MEANS
  outcomes_melt1 <- outcomes_melt %>%
    dplyr::select(strata, model_cat, variable, short_name, category, segment, var_class_std_mean) %>%
    distinct() %>%
    group_by(variable) %>%
    dplyr::mutate(class_rank = rank(var_class_std_mean)) %>%
    group_by(category, segment) %>%
    dplyr::mutate(mean_rank = mean(class_rank)) %>%
    group_by(category) %>%
    dplyr::mutate(var_count = n_distinct(variable),
                  category_var_count = paste0(category, " (", var_count, ")")) %>%
    dplyr::select(strata, model_cat, segment, category_var_count, category, mean_rank) %>%
    distinct()


  # # PROPORTION DIFFERENCES (ONLY WORKS WITH BINARY OUTCOME VARIABLES)
  # outcomes_melt2 <- outcomes %>%
  #   reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
  #   base::merge(outcome_vars_profile, by=c("variable")) %>%
  #   filter(!is.na(value)) %>%
  #   group_by(segment, variable) %>%
  #   mutate(total_responses = n(),
  #          responses = sum(value),
  #          prop = responses/total_responses) %>%
  #   group_by(variable) %>%
  #   mutate(mean_prop = mean(prop),
  #          mean_diff = prop - mean_prop)


  # VULNERABILITY RESPONSE PROPORTIONS
  vulnerability_melt <- vulnerability %>%
    reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    dplyr::mutate(value = ifelse(is.na(value), "*NA", value)) %>%
    group_by(segment, variable) %>%
    dplyr::mutate(total = sum(wt)) %>%
    group_by(segment, variable, value) %>%
    dplyr::mutate(count = sum(wt),
                  prop=round(count/total, 3))


  # VULNERABILITY RESPONSE PROPORTIONS + DOMAINS
  vulnerability_melt <- vulnerability_melt %>%
    base::merge(vulnerability_vars_profile, by=c("variable"))


  # VULNERABILITY RESPONSE PROPORTIONS WITH DUMMY VARS
  vulnerability_melt1 <- vulnerability %>%
    reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(segment, variable) %>%
    mutate(total_sum = sum(wt)) %>%
    group_by(segment, variable, value) %>%
    mutate(value_sum = sum(wt),
           prop = value_sum/total_sum) %>%
    dplyr::select(strata, model_cat, segment, variable, value, prop) %>%
    distinct() %>%
    group_by(variable, value) %>%
    mutate(prop_std = (prop - mean(prop, na.rm=TRUE))/sd(prop, na.rm=TRUE),
           prop_std_mean = mean(prop_std),
           prop_std_mean_diff = prop_std - prop_std_mean)


  # VULNERABILITY RESPONSE PROPORTIONS WITH DUMMY VARS + DOMAINS
  vulnerability_melt1 <- vulnerability_melt1 %>%
    # base::merge(vulnerability_vars_tt, by=c("variable"))
    base::merge(vulnerability_vars_profile, by=c("variable")) %>%
    mutate(variable_value = paste0(short_name, " | ", value))



  ###################################
  profile_plots_file = paste0(profile_plots, stratum, "_segment_profile_plots.pdf")
  dir.create(dirname(profile_plots_file), showWarnings = F, recursive = T)

  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = profile_plots_file,
            width = 15,
            height = 10,
            onefile=T)


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0(project_name, " | Segment profile plots | ", stratum), cex = 2)


  ###################################
  # SEGMENT DISTRIBUTION


  # DEFINE SURVEY OBJECT
  if (config::get("use_svy_design") == FALSE){

    df.design <- svydesign(id=~1, strata=NULL, weights=~wt, data=df)

  } else {

    df.design <- svydesign(id=~eval(parse(text = svy_id_var)), strata=~eval(parse(text = svy_strata_var)), weights=~wt, data=df, nest=TRUE)

  }


  # PIE CHART
  seg.prop <- df %>%
    mutate(total_sample = sum(wt)) %>%
    group_by(segment) %>%
    mutate(segment_sample = sum(wt),
           prop = segment_sample/total_sample) %>%
    dplyr::select(strata, segment, segment_sample, total_sample, prop) %>%
    distinct() %>%
    mutate(Segment = paste0("Segment ", segment))

  plot1 <- seg.prop %>%
    ggplot(aes(x="", y=prop, fill=Segment)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    theme_void() +
    theme(axis.text.x=element_blank()) +
    theme(legend.position = "bottom") +
    geom_text(aes(x = 1.7, label = scales::percent(prop, accuracy = .1)), position = position_stack(vjust = .5)) +
    labs(title = paste0("Segmentation Proportion: ", stratum)) +
    scale_fill_brewer(palette="Dark2")
  # print(plot1)


  # MAP
  df <- df %>%
    mutate(state = as.character(eval(parse(text = data_state_var))),
           state = stringr::str_to_title(trimws(gsub(pattern = "rurale|urbain|rural|urban|nc|ne|nw","", state))),
           state = case_when(state == "Thi<E8>S" ~ "Thies",
                             state == "Kolda Urban" ~ "Kolda",
                             state == "Ziquinchor" ~ "Ziguinchor",
                             state == "Benishangul" ~ "Benshangul-Gumaz",
                             state == "Snnpr" ~ "Southern Nations, Nationalities",
                             state == "Gambela" ~ "Gambela Peoples",
                             state == "Harari" ~ "Harari People",
                             state == "Addis Adaba" ~ "Addis Abeba",
                             state == "Fct Abuja" ~ "Federal Capital Territory",
                             TRUE ~ state))

  state_prop <- df %>%
    group_by(strata, state) %>%
    mutate(state_sample = sum(wt)) %>%
    group_by(strata, state, segment) %>%
    mutate(class_sample = sum(wt),
           class_state_prop = class_sample/state_sample) %>%
    dplyr::select(strata, state, segment, class_sample, state_sample, class_state_prop) %>%
    mutate(segment = paste0("Segment ", segment)) %>%
    distinct()

  state_prop_max <- state_prop %>%
    group_by(state) %>%
    mutate(max_prop = max(class_state_prop)) %>%
    mutate(max_prop_label = paste0(round(100*max_prop, 1),"%"),
           max_segment = ifelse(max_prop==class_state_prop, segment, NA)) %>%
    group_by(state) %>%
    tidyr::fill(max_segment, .direction = c("downup"))

  state_prop_max <- shp_file %>%
    base::merge(state_prop_max, by.x=c("NAME_1"), by.y=c("state"), all.x=TRUE) %>%
    mutate(centroid = sf::st_centroid(geometry))


  # PRINT OUT STATES THAT DON'T JOIN
  missing_admin1 <- df %>%
    dplyr::filter(!state %in% shp_file$NAME_1) %>%
    dplyr::select(state) %>%
    distinct()

  if (length(missing_admin1 > 0)){

    print(paste0("In Segmentation strata: ", stratum,  ", the following Admin1 geographies need to be conformed in the survey data: ", paste(missing_admin1$state, collapse = ", ")))

  } else {

    print(paste0("In Segmentation strata: ", stratum, ", all Admin1 geographies mapped."))

  }


  plot2 <- state_prop_max %>%
    dplyr::filter(max_prop == class_state_prop) %>%
    ggplot(aes(fill = segment, label=max_prop_label)) +
    geom_sf( linewidth = 0.2) +
    geom_sf_label(fill = "white",  # override the fill from aes()
                  fun.geometry = sf::st_centroid,size=2.5) +
    theme_void() +
    theme(legend.title=element_blank()) +
    theme(legend.position = "bottom") +
    labs(title = paste0("Largest Segment Represented: ", stratum)) +
    scale_fill_brewer(palette="Dark2")
  # print(plot2)

  grid.arrange(plot1, plot2,
               ncol=2)


  # MAP + PIE
  state_prop_max1 <- tidyr::extract(state_prop_max, centroid, c('lat', 'long'), '\\((.*)\\s(.*)\\)') %>%
    mutate(lat = str_replace_all(lat, ",", "")) %>%
    mutate(long = as.numeric(long),
           lat = as.numeric(lat))

  pie_dat <- state_prop_max1 %>%
    tidyr::gather(key="key", value="value", c("class_state_prop"))

  plot <- state_prop_max1 %>%
    ggplot() +
    # geom_sf(aes(fill=state_prop_max1$max_segment), data = state_prop_max1$geometry, linewidth = 0.2) +
    geom_sf(data = state_prop_max1$geometry, linewidth = 0.2) +
    geom_scatterpie(aes(x=lat, y=long), data=pie_dat, pie_scale = 2, cols="segment", long_format = TRUE) +
    theme_void() +
    theme(legend.title=element_blank()) +
    labs(title = paste0("Proportion of Segments Represented by State: ", stratum)) +
    scale_fill_brewer(palette="Dark2")
  print(plot)


  ###################################
  # OUTCOMES


  # LINE PLOT - DOTTED
  plot <- outcomes_melt %>%
    dplyr::select(strata, model_cat, variable, short_name, category, segment, var_class_std_mean) %>%
    distinct() %>%
    # filter(category == "ANC/PNC")
    ggplot(aes(x=reorder(short_name, -desc(category)), y=var_class_std_mean, group=segment)) +
    geom_point(aes(color=factor(segment)), size=4) +
    geom_line(aes(color=factor(segment)), lwd=0.5, linetype="dashed") +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0)) +
    theme(axis.text.y = element_blank()) +
    xlab("") +
    ylab("") +
    scale_color_brewer(palette="Dark2", name = "Segment") +
    # scale_color_manual(values=c("darkgreen", "purple", "red", "orange", "blue"), name = "Segment") +
    ggtitle(paste0("Outcomes std diff from mean across segments for model: ", n_class))
  print(plot)


  # LINE PLOT - SOLID
  plot <- outcomes_melt %>%
    dplyr::select(strata, model_cat, variable, short_name, category, segment, var_class_std_mean) %>%
    distinct() %>%
    # filter(category == "ANC/PNC")
    ggplot(aes(x=reorder(short_name, -desc(category)), y=var_class_std_mean, group=segment)) +
    geom_point(aes(color=factor(segment)), size=4) +
    geom_line(aes(color=factor(segment)), lwd=1, linetype="solid") +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0)) +
    theme(axis.text.y = element_blank()) +
    xlab("") +
    ylab("") +
    scale_color_brewer(palette="Dark2", name = "Segment") +
    # scale_color_manual(values=c("darkgreen", "purple", "red", "orange", "blue"), name = "Segment") +
    ggtitle(paste0("Outcomes std diff from mean across segments for model: ", n_class))
  print(plot)


  # LINE PLOT WITH ERROR BARS
  plot <- outcomes_melt %>%
    dplyr::select(strata, model_cat, variable, short_name, category, segment, var_class_std_mean, lower_bound, upper_bound) %>%
    distinct() %>%
    ggplot(aes(x=reorder(short_name, -desc(category)), y=var_class_std_mean, ymin=lower_bound, ymax=upper_bound, group=segment)) +
    geom_point(aes(color=factor(segment)), size=4, position=position_dodge(width=0.8)) +
    geom_errorbar(aes(color=factor(segment)), width = 0.5, position=position_dodge(width=0.8)) +
    geom_line(aes(color=factor(segment)), lwd=0.5, alpha=0.4, linetype="dashed", position=position_dodge(width=0.8)) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0)) +
    theme(axis.text.y = element_blank()) +
    xlab("") +  ylab("") +
    scale_color_brewer(palette="Dark2", name = "Segment") +
    # scale_color_manual(values=c("darkgreen", "purple", "red", "orange", "blue"), name = "Segment") +
    ggtitle(paste0("Outcomes std diff from mean across segments for model: ", n_class))
  print(plot)


  ###################################
  # RADAR PLOT
  segment_count = n_distinct(outcomes_melt1$segment)
  segment_count_mid = round(segment_count/2)

  data <- outcomes_melt1 %>%
    dplyr::select(segment, category_var_count, mean_rank) %>%
    reshape2::dcast(segment ~ category_var_count, value.var = "mean_rank")

  plot1 <- ggradar(plot.data = data, values.radar = c(segment_count,segment_count_mid,1), grid.min = 1, grid.mid = segment_count_mid, grid.max = segment_count) +
    theme(legend.position="bottom") +
    scale_color_brewer(palette="Dark2") +
    ggtitle(paste0("Mean ranking by outcome category (1 = less desirbable)"))
  print(plot1)


  data <- outcomes_melt %>%
    dplyr::select(strata, model_cat, variable, short_name, category, segment, var_class_std_mean) %>%
    distinct() %>%
    group_by(category) %>%
    dplyr::mutate(var_count = n_distinct(variable),
                  category_var_count = paste0(category, " (", var_count, ")")) %>%
    group_by(strata, model_cat, category, category_var_count, segment) %>%
    dplyr::summarize(mean_class_std_mean = mean(var_class_std_mean)) %>%
    ungroup() %>%
    dplyr::select(segment, category_var_count, mean_class_std_mean) %>%
    distinct() %>%
    reshape2::dcast(segment ~ category_var_count, value.var = "mean_class_std_mean")

  plot2 <- ggradar(plot.data = data, values.radar = c(-1,0,1), grid.min = -1, grid.mid = 0, grid.max = 1) +
    theme(legend.position="bottom") +
    scale_color_brewer(palette="Dark2") +
    ggtitle(paste0("Mean of standardized means by outcome category (1 = less desirable)"))
  print(plot2)

  # grid.arrange(plot1, plot2,
  #              ncol=2)


  # #
  # plot <- outcomes_melt2 %>%
  #   dplyr::select(strata, model_cat, variable, short_name, category, segment, prop, mean_prop, mean_diff) %>%
  #   distinct() %>%
  #   ggplot(aes(x=reorder(short_name, -desc(category)), y=mean_diff, group=segment)) +
  #   geom_point(aes(color=factor(segment)), size=4) +
  #   geom_line(aes(color=factor(segment)), lwd=1, linetype="dashed") +
  #   geom_hline(yintercept = 0) +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0)) +
  #   # theme(axis.text.y = element_blank()) +
  #   xlab("") +
  #   ylab("") +
  #   scale_color_brewer(palette="Dark2", name = "Segment") +
  #   # scale_color_manual(values=c("darkgreen", "purple", "red", "orange", "blue"), name = "Segment") +
  #   ggtitle(paste0("Outcomes prop yes diff from mean across segments for model: ", n_class))
  # print(plot)


  ###################################
  # VULNERABILITY FACTORS | STACKED BAR

  for (dom in unique(vulnerability_vars_profile$domain)){
    print(paste0(stratum, ": ", dom))

    # CLEAN UP DOMAIN NAME
    dom_label <- gsub("\\.", " ", dom)


    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, paste0("Vulnerability Factors for Domain: ", dom_label), cex = 2)

    domain_vars <- vulnerability_vars_profile %>%
      dplyr::filter(domain == dom)


    var_list <- sort(names(vulnerability)[(names(vulnerability) %in% domain_vars$variable)])
    plot_list <- list()


    # GENERATE PLOTS
    for (i in 1:length(var_list)){

      df_plot <- vulnerability_melt %>%
        dplyr::select(strata, model_cat, variable, short_name, segment, value, total, count, prop) %>%
        distinct() %>%
        dplyr::filter(variable == var_list[i])

      plot <- df_plot %>%
        ggplot(aes(x=segment, y=prop, fill=value)) +
        geom_bar(position="stack", stat="identity") +
        theme_bw() +
        labs(fill="") +
        ggtitle(paste0(df_plot$short_name[[1]])) +
        theme(plot.title = element_text(size=10)) +
        theme(legend.text=element_text(size=6), legend.title=element_text(size=8)) +
        # theme(axis.title.y = element_text(size=8)) +
        theme(axis.title.y = element_blank()) +
        theme(aspect.ratio = .60) +
        xlab("") +
        scale_fill_brewer(palette="Set2", labels = function(x) str_wrap(x, width = 10))

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

  }


  ###################################
  # VULNERABILITY FACTORS | PROPORTIONAL DIFFERENCES

  for (dom in unique(vulnerability_vars_profile$domain)){

    # CLEAN UP DOMAIN NAME
    dom_label <- gsub("\\.", " ", dom)

    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, paste0("Vulnerability Factors for Domain: ", dom_label), cex = 2)

    domain_vars <- vulnerability_vars_profile %>%
      dplyr::filter(domain == dom)


    df_plot <- vulnerability_melt1 %>%
      dplyr::filter(domain == dom) %>%
      dplyr::filter(variable %in% domain_vars$variable) %>%
      dplyr::select(strata, domain, model_cat, variable, short_name, segment, variable_value, prop_std, prop_std_mean, prop_std_mean_diff) %>%
      distinct()

    plot <- df_plot %>%
      dplyr::select(strata, model_cat, variable, short_name, segment, variable_value, prop_std_mean_diff) %>%
      distinct() %>%
      ggplot(aes(x=reorder(variable_value, -desc(variable_value)), y=prop_std_mean_diff, group=segment)) +
      geom_point(aes(color=factor(segment)), size=4) +
      geom_line(aes(color=factor(segment)), lwd=0.5, linetype="solid") +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0)) +
      theme(axis.text.y = element_blank()) +
      xlab("") +
      ylab("") +
      scale_color_brewer(palette="Dark2", name = "Segment") +
      # scale_color_manual(values=c("darkgreen", "purple", "red", "orange", "blue"), name = "Segment") +
      ggtitle(paste0("Vulnerability prop differences across segments for model: ", n_class, " for domain: ", dom))
    print(plot)


  }



  ###################################
  dev.off()
  graphics.off()


}


