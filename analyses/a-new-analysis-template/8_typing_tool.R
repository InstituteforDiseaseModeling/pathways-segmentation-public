


################################################################################
# TYPING TOOL
################################################################################


###################################
# RUN SETUP
source("1_setup.R")
# pacman::p_load(rpart, rpart.plot, caret, rattle, foreign, ggplot2, dplyr, tidyr, data.table, haven, ggcorrplot)


###################################
# SET PARAMETERS
params_sheet <- readRDS(params_excel_file)

final_models <- params_sheet %>%
  dplyr::select(strata, final_model) %>%
  dplyr::filter(!is.na(strata))

strata_set <- unique(final_models$strata)

urban_seg <- final_models %>% dplyr::filter(strata == "urban") %>% pull(final_model)
rural_seg <- final_models %>% dplyr::filter(strata == "rural") %>% pull(final_model)

set.seed(99)


###################################
# GET DATA INPUTS


vulnerability_vars <- readRDS(vulnerability_excel_file)

vulnerability_vars_tt <- vulnerability_vars %>%
  dplyr::filter(typing_tool_include == 1)


path = paste0(lca_path, "urban_outcomes_vulnerability_class.rds")
urban_sol <- readRDS(path)


path = paste0(lca_path, "rural_outcomes_vulnerability_class.rds")
rural_sol <- readRDS(path)


######################################################################
# URBAN
stratum = "urban"

urban_sol$n_class <- urban_sol %>% pull(eval(parse(text=urban_seg)))

urban_tt_vars <- vulnerability_vars_tt %>%
  dplyr::filter(typing_tool_strata %in% c("both", "all", stratum)) %>%
  dplyr::select(vulnerability_variable) %>%
  setNames(c("variable"))


tt_input_names <- names(urban_sol)[(names(urban_sol) %in% unique(c("n_class", urban_tt_vars$variable)))]
urban_input <- subset(urban_sol, select=c(tt_input_names))


###################################
# DATA PREP
# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(urban_input)){

  if ((all(c(0, 1) %in% na.omit(urban_input[[col]])) == TRUE) & (all(na.omit(urban_input[[col]]) %in% c(0, 1)) == TRUE) |
      (all(c("Yes", "No") %in% str_to_title(na.omit(urban_input[[col]]))) == TRUE) & (all(str_to_title(na.omit(urban_input[[col]])) %in% c("Yes", "No")) == TRUE)){

    print(col)
    urban_input[[col]] <- ifelse(urban_input[[col]] == 1, "Yes", urban_input[[col]])
    urban_input[[col]] <- ifelse(urban_input[[col]] == 0, "No", urban_input[[col]])
    urban_input[[col]] <- ifelse(urban_input[[col]] == "yes", "Yes", urban_input[[col]])
    urban_input[[col]] <- ifelse(urban_input[[col]] == "no", "No", urban_input[[col]])
  }
}

# urban_input <- urban_input %>%
#   dplyr::mutate(has.bank = case_when(has.bank == 1 ~ "No",
#                                      has.bank == 2 ~ "Yes"))


# GROW TREE
tree_urban <- rpart(n_class ~ .,
                    data=urban_input,
                    control=rpart.control(cp=0.0001, xval=5), method="class")


# CONFUSION MATRIX - UNPRUNED
confusion.matrix = confusionMatrix(data = predict(tree_urban,
                                                  data=urban_input,
                                                  type="class"),
                                   as.factor(urban_input$n_class))
print(confusion.matrix)
tbl_cm_urban_1 <- data.frame(confusion.matrix$byClass)
tbl_cm_urban_1 <- cbind(variable = rownames(tbl_cm_urban_1), tbl_cm_urban_1)
rownames(tbl_cm_urban_1) <- NULL

# VARIABLE IMPORTANCE
tbl_vi_urban_1 <- data.frame(tree_urban$variable.importance)
tbl_vi_urban_1
tbl_vi_urban_1 <- cbind(variable = rownames(tbl_vi_urban_1), tbl_vi_urban_1)
rownames(tbl_vi_urban_1) <- NULL

# VIEW TREE
prp(tree_urban, type=3, varlen=0, cex=1)

# PRINT COMPLEXITY PARAMTER TABLE & PLOT
plotcp(tree_urban); printcp(tree_urban)

tbl_cp_urban_1 <- data.frame(printcp(tree_urban))

optimal_xerror_urban <- tbl_cp_urban_1 %>%
  dplyr::filter(xerror == min(xerror)) %>%
  dplyr::mutate(optimal_xerror = xerror + xstd) %>%
  pull(optimal_xerror)

optimal_cp_urban <- tbl_cp_urban_1 %>%
  dplyr::filter(xerror == xerror[which.min(abs(xerror - optimal_xerror_urban))]) %>%
  dplyr::filter(nsplit == min(nsplit))

cp_urban <- optimal_cp_urban %>% pull(CP)
size_urban <- optimal_cp_urban %>% pull(nsplit)


###################################
# PRUNE URBAN TREE
tree_urban_prune <- prune(tree_urban, cp=cp_urban)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

PredictCART_train_urban <- predict(tree_urban_prune, data=urban_input, type="class")

confusion.matrix <- confusionMatrix(data=PredictCART_train_urban,
                                    as.factor(urban_input$n_class))
print(confusion.matrix)
tbl_cm_urban_2 <- data.frame(confusion.matrix$byClass)
tbl_cm_urban_2 <- cbind(variable = rownames(tbl_cm_urban_2), tbl_cm_urban_2)
rownames(tbl_cm_urban_2) <- NULL

# VIEW TREE
prp(tree_urban_prune, type=3, varlen=0, cex=1)


###################################
# SAVE OUTPUT
path = paste0(cart_path, 'urban_typing_tool.rds')
urban_output <- urban_sol %>%
  dplyr::select(caseid, survey, strata, all_of(svy_id_var), all_of(svy_id_var), unique(all_of(svy_strata_var), all_of(data_state_var))) %>%
  cbind(urban_input) %>%
  cbind(pred_class = PredictCART_train_urban) %>%
  dplyr::mutate(final_model = urban_seg)

dir.create(dirname(path), showWarnings = F, recursive = T)
saveRDS(urban_output, file = path)


######################################################################
# RURAL
stratum = "rural"

rural_sol$n_class <- rural_sol %>% pull(eval(parse(text=rural_seg)))

rural_tt_vars <- vulnerability_vars_tt %>%
  dplyr::filter(typing_tool_strata %in% c("both", "all", stratum)) %>%
  dplyr::select(vulnerability_variable) %>%
  setNames(c("variable"))


tt_input_names <- names(rural_sol)[(names(rural_sol) %in% c("n_class", rural_tt_vars$variable))]
rural_input <- subset(rural_sol, select=c(tt_input_names))


###################################
# DATA PREP
# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(rural_input)){

  if ((all(c(0, 1) %in% na.omit(rural_input[[col]])) == TRUE) & (all(na.omit(rural_input[[col]]) %in% c(0, 1)) == TRUE) |
      (all(c("Yes", "No") %in% str_to_title(na.omit(rural_input[[col]]))) == TRUE) & (all(str_to_title(na.omit(rural_input[[col]])) %in% c("Yes", "No")) == TRUE)){

    print(col)
    rural_input[[col]] <- ifelse(rural_input[[col]] == 1, "Yes", rural_input[[col]])
    rural_input[[col]] <- ifelse(rural_input[[col]] == 0, "No", rural_input[[col]])
    rural_input[[col]] <- ifelse(rural_input[[col]] == "yes", "Yes", rural_input[[col]])
    rural_input[[col]] <- ifelse(rural_input[[col]] == "no", "No", rural_input[[col]])
  }
}

# rural_input <- rural_input %>%
#   dplyr::mutate(has.bank = case_when(has.bank == 1 ~ "No",
#                                      has.bank == 2 ~ "Yes"))


# GROW TREE
tree_rural <- rpart(n_class ~ .,
                    data=rural_input, control=rpart.control(cp=0.0001, xval=5), method="class")


# CONFUSION MATRIX - UNPRUNED
confusion.matrix = confusionMatrix(data = predict(tree_rural,
                                                  data=rural_input,
                                                  type="class"),
                                   as.factor(rural_input$n_class))
print(confusion.matrix)
tbl_cm_rural_1 <- data.frame(confusion.matrix$byClass)
tbl_cm_rural_1 <- cbind(segment = rownames(tbl_cm_rural_1), tbl_cm_rural_1)
rownames(tbl_cm_rural_1) <- NULL

# VARIABLE IMPORTANCE
tbl_vi_rural_1 <- data.frame(tree_rural$variable.importance)
tbl_vi_rural_1
tbl_vi_rural_1 <- cbind(variable = rownames(tbl_vi_rural_1), tbl_vi_rural_1)
rownames(tbl_vi_rural_1) <- NULL

# VIEW TREE
prp(tree_rural, type=3, varlen=0, cex=1)

# PRINT COMPLEXITY PARAMTER TABLE & PLOT
plotcp(tree_rural); printcp(tree_rural)

tbl_cp_rural_1 <- data.frame(printcp(tree_rural))

optimal_xerror_rural <- tbl_cp_rural_1 %>%
  dplyr::filter(xerror == min(xerror)) %>%
  dplyr::mutate(optimal_xerror = xerror + xstd) %>%
  pull(optimal_xerror)

optimal_cp_rural <- tbl_cp_rural_1 %>%
  dplyr::filter(xerror == xerror[which.min(abs(xerror - optimal_xerror_rural))]) %>%
  dplyr::filter(nsplit == min(nsplit))

cp_rural <- optimal_cp_rural %>% pull(CP)
size_rural <- optimal_cp_rural %>% pull(nsplit)


###################################
# PRUNE RURAL TREE
tree_rural_prune <- prune(tree_rural, cp=cp_rural)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

PredictCART_train_rural <- predict(tree_rural_prune, data=rural_input, type="class")
confusion.matrix <- confusionMatrix(data = PredictCART_train_rural,
                                    as.factor(rural_input$n_class))
print(confusion.matrix)
tbl_cm_rural_2 <- data.frame(confusion.matrix$byClass)
tbl_cm_rural_2 <- cbind(variable = rownames(tbl_cm_rural_2), tbl_cm_rural_2)
rownames(tbl_cm_rural_2) <- NULL

prp(tree_rural_prune, type=3, varlen=0, cex=1)


###################################
# SAVE OUTPUT
path = paste0(cart_path, 'rural_typing_tool.rds')
rural_output <- rural_sol %>%
  dplyr::select(caseid, survey, strata, all_of(svy_id_var), all_of(svy_id_var), unique(all_of(svy_strata_var), all_of(data_state_var))) %>%
  cbind(rural_input) %>%
  cbind(pred_class = PredictCART_train_rural) %>%
  dplyr::mutate(final_model = rural_seg)

dir.create(dirname(path), showWarnings = F, recursive = T)
saveRDS(rural_output, file = path)


###################################
# GENERATE PDF OF OUTPUTS | URBAN
path = paste0(cart_plots, "typing_tool_outputs_urban.pdf")
dir.create(dirname(path), showWarnings = F, recursive = T)
cairo_pdf(filename = path,
          width = 15,
          height = 10,
          onefile=T)

plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(5, 8, paste0(project_name, " | CART outputs | ", "Urban", " | Nobs: ", sum(complete.cases(urban_input))), cex = 2)

tbl1 <- tableGrob(tbl_cm_urban_1,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl1)

tbl2 <- tableGrob(tbl_vi_urban_1,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl2)

# grid.arrange(tbl1, tbl2,
#              ncol=2,
#              top = textGrob(paste0("Unpruned tree: urban"), gp = gpar(fontsize = 16, fontface = "bold"))
# )

prp(tree_urban, type=3, varlen=0, cex=1)
plotcp(tree_urban); printcp(tree_urban)


#
tbl3 <- tableGrob(tbl_cp_urban_1,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.5),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.5),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl3)
grid.text(paste0("Optimal CP: ", round(cp_urban, 5), ". Optimal nsplit: ", size_urban, "."), x = 0.5, y = 0.98, gp = gpar(fontsize = 16, fontface = "bold"))


#
tbl4 <- tableGrob(tbl_cm_urban_2,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl4)

prp(tree_urban_prune, type=3, varlen=0, cex=1)

fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)


###################################
dev.off()
graphics.off()



###################################
# GENERATE PDF OF OUTPUTS | RURAL
path = paste0(cart_plots, "typing_tool_outputs_rural.pdf")
dir.create(dirname(path), showWarnings = F, recursive = T)
cairo_pdf(filename = path,
          width = 15,
          height = 10,
          onefile=T)

plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(5, 8, paste0(project_name, " | CART outputs | ", "Rural", " | Nobs: ", sum(complete.cases(rural_input))), cex = 2)

tbl1 <- tableGrob(tbl_cm_rural_1,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl1)

tbl2 <- tableGrob(tbl_vi_rural_1,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl2)

# grid.arrange(tbl1, tbl2,
#              ncol=2,
#              top = textGrob(paste0("Unpruned tree: rural"), gp = gpar(fontsize = 16, fontface = "bold"))
# )

prp(tree_rural, type=3, varlen=0, cex=1)
plotcp(tree_rural); printcp(tree_rural)


#
tbl3 <- tableGrob(tbl_cp_rural_1,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.5),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.5),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl3)
grid.text(paste0("Optimal CP: ", round(cp_rural, 5), ". Optimal nsplit: ", size_rural, "."), x = 0.5, y = 0.98, gp = gpar(fontsize = 16, fontface = "bold"))


#
tbl4 <- tableGrob(tbl_cm_rural_2,
                  rows = NULL,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                bg_params = list(col = "black", lwd = 1)),  # Black border
                    colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                   bg_params = list(col = "black", lwd = 1)) # Border for headers
                  ))
grid.newpage()
grid.draw(tbl4)

prp(tree_rural_prune, type=3, varlen=0, cex=1)

fancyRpartPlot(tree_rural_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)


###################################
dev.off()
graphics.off()


