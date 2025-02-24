

###################################
#

vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))

vulnerability_sheet <- readRDS(vulnerability_excel_file) %>%
  dplyr::filter(univariate_include == 1) %>%
  dplyr::select(vulnerability_variable, short_name) %>%
  setNames(c("indicator", "short_name")) %>%
  distinct()

df <- vulnerability
###################################
###################################
#

var = "age.1stbrth"
plot_label = vulnerability_sheet[vulnerability_sheet$indicator == var, c("short_name")][[1]]


df$variable = var
df$value = df %>% pull(eval(parse(text=var)))

df_plot <- df %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_plot = value)

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
  scale_fill_brewer(palette="Dark2")
print(plot)


###################################
# CREATE BINARY CATEGORICAL VARIABLE BY SPLITTING THE DISTRIBUTION INTO TWO GROUPS
# PRESERVE NA VALUES AS NA
df <- df %>%
  dplyr::mutate(age.1stbrth.cat = case_when(age.1stbrth >= 19 ~ "19+",
                                            age.1stbrth < 19 ~ "<19"))


var = "age.1stbrth.cat"
plot_label = "Age 1st Birth Cat"

df$variable = var
df$value = df %>% pull(eval(parse(text=var)))

df_plot <- df %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_plot = value)

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
print(plot)


###################################
###################################
#
var = "age.1stsex"
plot_label = vulnerability_sheet[vulnerability_sheet$indicator == var, c("short_name")][[1]]


df$variable = var
df$value = df %>% pull(eval(parse(text=var)))

df_plot <- df %>%
  mutate_if(is.factor, as.character) %>%
  mutate(value_plot = value)

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
  scale_fill_brewer(palette="Dark2")
print(plot)


###################################
# CREATE A BINNED CATEGORICAL VARIABLE FROM A CONTINUOUS VARIABLE
# PRESERVE NA VALUES AS NA
df <- df %>%
  dplyr::mutate(age.1stsex.cat = case_when(age.1stsex <= 15 ~ "<15",
                                           age.1stsex > 15 &  age.1stsex <= 25 ~ "16-25",
                                           age.1stsex > 25 ~ ">25"))


var = "age.1stsex.cat"
plot_label = "Age 1st Sex Cat"

df$variable = var
df$value = df %>% pull(eval(parse(text=var)))

df_plot <- df %>%
  # mutate_if(is.factor, as.character) %>%
  mutate(value_plot = value)

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
print(plot)


