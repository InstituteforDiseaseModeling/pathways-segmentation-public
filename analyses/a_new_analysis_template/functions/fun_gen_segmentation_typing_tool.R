

################################################################################
# GENERATE TYPING TOOL OUTPUT
################################################################################


fun_gen_typing_tool <- function(df=NULL, stratum=NULL, tt_vars=NULL, final_seg=NULL){


  # SUBSET TO TT VARS
  input <- subset(df, select=c(tt_vars))


  ###################################
  # DATA PREP
  # CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
  for (col in names(input)){

    if ((all(c(0, 1) %in% na.omit(input[[col]])) == TRUE) & (all(na.omit(input[[col]]) %in% c(0, 1)) == TRUE) |
        (all(c("Yes", "No") %in% str_to_title(na.omit(input[[col]]))) == TRUE) & (all(str_to_title(na.omit(input[[col]])) %in% c("Yes", "No")) == TRUE)){

      print(col)
      input[[col]] <- ifelse(input[[col]] == 1, "Yes", input[[col]])
      input[[col]] <- ifelse(input[[col]] == 0, "No", input[[col]])
      input[[col]] <- ifelse(input[[col]] == "yes", "Yes", input[[col]])
      input[[col]] <- ifelse(input[[col]] == "no", "No", input[[col]])
    }
  }


  # GROW TREE
  tree <- rpart(n_class ~ .,
                data=input,
                control=rpart.control(cp=0.0001, xval=5), method="class")


  # CONFUSION MATRIX - UNPRUNED
  confusion.matrix = confusionMatrix(data = predict(tree,
                                                    data=input,
                                                    type="class"),
                                     as.factor(input$n_class))
  print(confusion.matrix)
  tbl_cm_1 <- data.frame(confusion.matrix$byClass)
  tbl_cm_1 <- cbind(variable = rownames(tbl_cm_1), tbl_cm_1)
  rownames(tbl_cm_1) <- NULL


  # VARIABLE IMPORTANCE
  tbl_vi_1 <- data.frame(tree$variable.importance)
  tbl_vi_1
  tbl_vi_1 <- cbind(variable = rownames(tbl_vi_1), tbl_vi_1)
  rownames(tbl_vi_1) <- NULL

  # VIEW TREE
  prp(tree, type=3, varlen=0, cex=1)

  # PRINT COMPLEXITY PARAMTER TABLE & PLOT
  plotcp(tree); printcp(tree)

  tbl_cp_1 <- data.frame(printcp(tree))

  optimal_xerror <- tbl_cp_1 %>%
    dplyr::slice_min(xerror, n=1, with_ties=FALSE) %>%
    dplyr::mutate(optimal_xerror = xerror + xstd) %>%
    pull(optimal_xerror)

  optimal_cp <- tbl_cp_1 %>%
    dplyr::filter(xerror == xerror[which.min(abs(xerror - optimal_xerror))]) %>%
    dplyr::filter(nsplit == min(nsplit))

  cp <- optimal_cp %>% pull(CP)
  size <- optimal_cp %>% pull(nsplit)


  ###################################
  # PRUNE TREE
  tree_prune <- prune(tree, cp=cp)
  plotcp(tree_prune); printcp(tree_prune)

  PredictCART_train <- predict(tree, data=input, type="class")

  confusion.matrix <- confusionMatrix(data=PredictCART_train,
                                      as.factor(input$n_class))
  print(confusion.matrix)
  tbl_cm_2 <- data.frame(confusion.matrix$byClass)
  tbl_cm_2 <- cbind(variable = rownames(tbl_cm_2), tbl_cm_2)
  rownames(tbl_cm_2) <- NULL

  # VIEW TREE
  prp(tree_prune, type=3, varlen=0, cex=1)



  ###################################
  # SAVE OUTPUT
  path = paste0(cart_path, stratum, '_typing_tool.rds')
  output <- df %>%
    dplyr::select(caseid, survey, strata, all_of(svy_id_var), all_of(svy_id_var), unique(all_of(svy_strata_var), all_of(data_state_var))) %>%
    cbind(input) %>%
    cbind(pred_class = PredictCART_train) %>%
    dplyr::mutate(final_model = final_seg)

  dir.create(dirname(path), showWarnings = F, recursive = T)
  saveRDS(output, file = path)


  ###################################
  # GENERATE PDF OF OUTPUTS | STRATA
  path = paste0(cart_plots, "typing_tool_outputs_", stratum, ".pdf")
  dir.create(dirname(path), showWarnings = F, recursive = T)
  cairo_pdf(filename = path,
            width = 15,
            height = 10,
            onefile=T)

  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0(project_name, " | CART outputs | ", stratum, " | Nobs: ", sum(complete.cases(input))), cex = 2)

  tbl1 <- tableGrob(tbl_cm_1,
                    rows = NULL,
                    theme = ttheme_minimal(
                      core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                  bg_params = list(col = "black", lwd = 1)),  # Black border
                      colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                     bg_params = list(col = "black", lwd = 1)) # Border for headers
                    ))
  grid.newpage()
  grid.draw(tbl1)

  tbl2 <- tableGrob(tbl_vi_1,
                    rows = NULL,
                    theme = ttheme_minimal(
                      core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                  bg_params = list(col = "black", lwd = 1)),  # Black border
                      colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                     bg_params = list(col = "black", lwd = 1)) # Border for headers
                    ))
  grid.newpage()
  grid.draw(tbl2)


  prp(tree, type=3, varlen=0, cex=1)
  plotcp(tree); printcp(tree)


  #
  tbl3 <- tableGrob(tbl_cp_1,
                    rows = NULL,
                    theme = ttheme_minimal(
                      core = list(fg_params = list(fontface = "plain", cex = 0.5),
                                  bg_params = list(col = "black", lwd = 1)),  # Black border
                      colhead = list(fg_params = list(fontface = "bold", cex = 0.5),
                                     bg_params = list(col = "black", lwd = 1)) # Border for headers
                    ))
  grid.newpage()
  grid.draw(tbl3)
  grid.text(paste0("Optimal CP: ", round(cp, 5), ". Optimal nsplit: ", size, "."), x = 0.5, y = 0.98, gp = gpar(fontsize = 16, fontface = "bold"))


  #
  tbl4 <- tableGrob(tbl_cm_2,
                    rows = NULL,
                    theme = ttheme_minimal(
                      core = list(fg_params = list(fontface = "plain", cex = 0.7),
                                  bg_params = list(col = "black", lwd = 1)),  # Black border
                      colhead = list(fg_params = list(fontface = "bold", cex = 0.7),
                                     bg_params = list(col = "black", lwd = 1)) # Border for headers
                    ))
  grid.newpage()
  grid.draw(tbl4)

  prp(tree_prune, type=3, varlen=0, cex=1)

  fancyRpartPlot(tree_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)


  ###################################
  # SAVE FULL TREE SAVE PRUNE TREE
  saveRDS(tree, file = paste0(cart_path, stratum, "_tree_full_model.rds"))
  saveRDS(tree_prune, file = paste0(cart_path, stratum, "_tree_prune_model.rds"))


  ###################################
  dev.off()
  graphics.off()


}
