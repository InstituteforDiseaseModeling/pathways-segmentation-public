

################################################################################
# SEGMENT PROPORTIONS OF POPULATION
################################################################################


###################################
# RUN SETUP
source("1_setup.R")

nreps = 100


###################################
# SET PARAMETERS
params_sheet <- readRDS(params_excel_file)

final_models <- params_sheet %>%
  dplyr::select(strata, final_model) %>%
  dplyr::filter(!is.na(strata))

strata_set <- unique(final_models$strata)


###################################
#

df_pop_prop <- data.frame()

for (stratum in strata_set){
  print(stratum)


  # SET SELECTED MODEL TO PROFILE
  n_class <- final_models %>%
    dplyr::filter(strata == stratum) %>%
    pull(final_model)
  print(n_class)

  # GET COMBINED DATASET WITH MODELED SEGMENTS
  path = paste0(lca_path, "nreps", nreps, "/", stratum, "_outcomes_vulnerability_class_ranked.rds")
  df <- readRDS(path)

  df$segment <- df %>% pull(eval(parse(text=n_class)))
  df$model_cat <- n_class
  df$segment = paste0(df$strata, "_segment_", df$segment)
  # df$segment = paste0("segment_", df$segment_rank)

  df <- df %>%
    dplyr::mutate(state = as.character(eval(parse(text = data_state_var))),
                  state = stringr::str_to_title(trimws(gsub(pattern = "rurale|urbain|rural|urban|nc|ne|nw|,","", state))),
                  state = case_when(state == "Thi<E8>S" ~ "Thies",
                                    state == "Kolda Urban" ~ "Kolda",
                                    state == "Ziquinchor" ~ "Ziguinchor",
                                    state == "Benishangul" ~ "Benshangul-Gumaz",
                                    state == "Snnpr" ~ "Southern Nations, Nationalities",
                                    state == "Gambela" ~ "Gambela Peoples",
                                    state == "Harari" ~ "Harari People",
                                    state == "Addis Adaba" ~ "Addis Abeba",
                                    state == "Fct Abuja" ~ "Federal Capital Territory",
                                    state == "Boucle Du Mouhoun" ~ "Boucle du Mouhoun",
                                    state == "Centre Est" ~ "Centre-Est",
                                    state == "Centre Nord" ~ "Centre-Nord",
                                    state == "Centre Ouest" ~ "Centre-Ouest",
                                    state == "Centre Sud" ~ "Centre-Sud",
                                    state == "Hauts-Bassins" ~ "Haut-Bassins",
                                    state == "Plateau Central" ~ "Plateau-Central",
                                    state == "Kpk" ~ "Khyber-Pakhtunkhwa",
                                    state == "Fata" ~ "Federally Administered Tribal Ar",
                                    state == "Ict" ~ "Islamabad",
                                    state == "Gb" ~ "Gilgit-Baltistan",
                                    state == "Ajk" ~ "Azad Kashmir",
                                    TRUE ~ state)) %>%
    dplyr::select(strata, state, caseid, wt, segment)

  df_pop_prop <- rbind(df_pop_prop, df)


}


# CALCULATE PROPORTIONS WITHIN STRATA
df_pop_prop_strata <- df_pop_prop %>%
  dplyr::group_by(strata) %>%
  dplyr::mutate(strata_total = sum(wt)) %>%
  dplyr::group_by(segment) %>%
  dplyr::mutate(strata_segment_total = sum(wt),
                strata_segment_prop = round(strata_segment_total/strata_total, 3)) %>%
  ungroup() %>%
  dplyr::mutate(total = sum(wt),
                segment_prop = round(strata_segment_total/total, 3)) %>%
  dplyr::select(segment, strata_segment_prop, segment_prop) %>%
  distinct()


#
df_pop_prop_state <- df_pop_prop %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(state_total = sum(wt)) %>%
  dplyr::group_by(state, segment) %>%
  dplyr::mutate(state_segment_total = sum(wt),
                state_segment_prop = round(state_segment_total/state_total, 3)) %>%
  ungroup() %>%
  dplyr::mutate(total = sum(wt)) %>%
  dplyr::group_by(segment) %>%
  dplyr::mutate(segment_prop = sum(wt),
                segment_total_prop = round(segment_prop/total, 3),
                state_total_prop = round(state_total/total, 3)) %>%
  dplyr::group_by(strata) %>%
  dplyr::mutate(strata_total = sum(wt),
                strata_prop = round(strata_total/total, 3)) %>%
  dplyr::select(strata, state, segment, state_segment_prop, segment_total_prop, state_total_prop, strata_prop) %>%
  distinct()


#
df_pop_prop_segment_state <- df_pop_prop %>%
  dplyr::group_by(segment) %>%
  dplyr::mutate(segment_total = sum(wt)) %>%
  dplyr::group_by(segment, state) %>%
  dplyr::mutate(segment_state_total = sum(wt),
                segment_state_prop = round(segment_state_total/segment_total, 3)) %>%
  dplyr::group_by(segment) %>%
  dplyr::mutate(max_segment_state_prop = max(segment_state_prop),
                max_segment_state = ifelse(segment_state_prop == max_segment_state_prop, state, NA)) %>%
  ungroup() %>%
  dplyr::select(strata, segment, state, segment_total, segment_state_total, segment_state_prop, max_segment_state_prop, max_segment_state) %>%
  distinct()






#   dplyr::group_by(state) %>%
#   dplyr::mutate(state_total = sum(wt),
#                 state_segment_prop = round(strata_state_segment_total/state_total, 3)) %>%
#   dplyr::select(strata, state, segment, strata_segment_prop, segment_prop, strata_state_segment_prop, state_segment_prop) %>%
#   distinct()
#
# urban <- df_pop_prop_strata %>%
#   dplyr::filter(strata == "urban")
#
# urban %>%
#   dplyr::group_by(segment) %>%
#   dplyr::summarize(prop = sum(state_segment_prop))




