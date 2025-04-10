


################################################################################
# CREATE SAMPLE DATA FROM DHS ETHIOPIA 2016
################################################################################

IR <- readRDS(file = paste0(data_path, "IR.rds"))
BR <- readRDS(file = paste0(data_path, "BR.rds"))
KR <- readRDS(file = paste0(data_path, "KR.rds"))
HH <- readRDS(file = paste0(data_path, "HH.rds"))
MR <- readRDS(file = paste0(data_path, "MR.rds"))


outcomes_vulnerability <- readRDS(file = paste0(outcomes_vulnerability_file, ".rds"))
outcomes_vulnerability_sample <- outcomes_vulnerability %>%
  dplyr::filter(!is.na(num.solidfood),
                !is.na(m.men.dec.purchases)) %>%
  dplyr::slice_sample(n = 100) %>%
  dplyr::select(caseid, v001, v002, v003) %>%
  distinct() %>%
  dplyr::mutate(sample_caseid = sample(1e6, n(), replace = FALSE),
                sample_v001 = sample(1e6, n(), replace = FALSE),
                sample_v002 = sample(1e6, n(), replace = FALSE),
                sample_v003 = sample(1e6, n(), replace = FALSE))


# JOIN THE REST OF THE RECODE FILES ON THESE 100 WOMEN AND DROP THE caseid
ir_sample <- IR %>%
  base::merge(outcomes_vulnerability_sample[,c("v001", "v002", "v003", "sample_v001", "sample_v002", "sample_v003")], by=c("v001", "v002", "v003")) #%>%
  # dplyr::mutate(v001 = sample_v001,
  #               v002 = sample_v002,
  #               v003 = sample_v003) %>%
  # dplyr::select(-c("sample_v001", "sample_v002", "sample_v003"))


br_sample <- BR %>%
  base::merge(outcomes_vulnerability_sample[,c("v001", "v002", "v003", "sample_v001", "sample_v002", "sample_v003")], by=c("v001", "v002", "v003")) #%>%
  # dplyr::mutate(v001 = sample_v001,
  #               v002 = sample_v002,
  #               v003 = sample_v003) %>%
  # dplyr::select(-c("sample_v001", "sample_v002", "sample_v003"))


kr_sample <- KR %>%
  base::merge(outcomes_vulnerability_sample[,c("v001", "v002", "v003", "sample_v001", "sample_v002", "sample_v003")], by=c("v001", "v002", "v003")) #%>%
  # dplyr::mutate(v001 = sample_v001,
  #               v002 = sample_v002,
  #               v003 = sample_v003) %>%
  # dplyr::select(-c("sample_v001", "sample_v002", "sample_v003"))


hh_sample <- HH %>%
  base::merge(outcomes_vulnerability_sample[,c("v001", "v002", "sample_v001", "sample_v002")], by.x=c("hv001", "hv002"), by.y=c("v001", "v002")) #%>%
  # dplyr::mutate(v001 = sample_v001,
  #               v002 = sample_v002) %>%
  # dplyr::select(-c("sample_v001", "sample_v002"))


mr_sample <- MR %>%
  base::merge(outcomes_vulnerability_sample[,c("v001", "v002", "v003", "sample_v001", "sample_v002", "sample_v003")], by.x=c("mv001", "mv002", "mv003"), by.y=c("v001", "v002", "v003")) #%>%
  # dplyr::mutate(mv001 = sample_v001,
  #               mv002 = sample_v002,
  #               mv003 = sample_v003) %>%
  # dplyr::select(-c("sample_v001", "sample_v002", "sample_v003"))


# SAVE FILES
sample_data_path = "../samples/data/"

saveRDS(ir_sample, file = paste0(sample_data_path, "IR.rds"))
saveRDS(br_sample, file = paste0(sample_data_path, "BR.rds"))
saveRDS(kr_sample, file = paste0(sample_data_path, "KR.rds"))
saveRDS(hh_sample, file = paste0(sample_data_path, "HH.rds"))
saveRDS(mr_sample, file = paste0(sample_data_path, "MR.rds"))


# # TEST
# vulnerability <- gen_vulnerability_factors_dhs(IR=ir_sample, BR=br_sample, HH=hh_sample, MR=mr_sample, dhs=7)
# vulnerability_vars <- setdiff(names(vulnerability), unique(c(names(ir_sample), names(br_sample), names(hh_sample), names(mr_sample))))
# vulnerability <- subset(vulnerability, select=unique(c("caseid", "survey", all_of(svy_id_var), all_of(svy_strata_var), all_of(data_state_var), vulnerability_vars)))

