rm(list=ls())
pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect)
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven", 
               "survey", "ggplot2", "dplyr", "tidyr", "sf", "RColorBrewer", "readxl")
#conn <- odbcConnect("IDMAzureDatabricks_DSN") 
options(max.print=1000000)
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")

### READ-IN TRUNCATED UP DATASET
load("Data/BR.RData")
load("Data/IR.RData")
load("Data/HH.RData")
load("Data/KR.RData")

######################################################################
# 1 | WOMAN AND HER PAST EXPERIENCES
######################################################################

hv109 <- HH %>% dplyr:: select(starts_with("hv109_"))
lapply(hv109, table)
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "no education", "1")))
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "incomplete primary", "2")))
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "complete primary", "3")))
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "incomplete secondary", "4")))
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "complete secondary", "5")))
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "higher", "6")))
HH <- HH %>%
  mutate(across(starts_with("hv109_"), ~replace(., . == "don't know", NA))) ## don't know -> NA
hv109 <- HH %>% dplyr:: select(starts_with("hv109_"))
lapply(hv109, table)
HH <- HH %>%
  mutate(across(starts_with("hv109_"), as.numeric))
hv109 <- HH %>% dplyr:: select(starts_with("hv109_"))
lapply(hv109, summary)
HH$edu.hh.highest_ <- pmax(HH$hv109_01, HH$hv109_02, HH$hv109_03, HH$hv109_04, HH$hv109_05, HH$hv109_06, HH$hv109_07, HH$hv109_08, HH$hv109_09, HH$hv109_10,
                          HH$hv109_11, HH$hv109_12, HH$hv109_13, HH$hv109_14, HH$hv109_15, HH$hv109_16, HH$hv109_17, HH$hv109_18, HH$hv109_19, HH$hv109_20,
                          HH$hv109_21, HH$hv109_22, HH$hv109_23, HH$hv109_24, HH$hv109_25, HH$hv109_26, HH$hv109_27, HH$hv109_28, HH$hv109_29, HH$hv109_30,
                          HH$hv109_31, HH$hv109_32, HH$hv109_33, HH$hv109_34, HH$hv109_35, na.rm= TRUE)
table(HH$edu.hh.highest_)
prop.table(table(HH$edu.hh.highest_))
HH <- HH %>%
  mutate(edu.hh.highest=case_when(
    edu.hh.highest_==1 ~ "1: no/primary education",
    edu.hh.highest_==2 ~ "1: no/primary education",
    edu.hh.highest_==3 ~ "1: no/primary education",
    edu.hh.highest_==4 ~ "2: incomplete secondary",
    edu.hh.highest_==5 ~ "3: complete secondary",
    edu.hh.highest_==6 ~ "4: higher"
  ))
table(HH$edu.hh.highest)
prop.table(table(HH$edu.hh.highest))


###################################
# WOMAN RESPONDENT ATTRIBUTES

#caste: 
table(IR$s116)
IR <- IR %>% mutate(
  caste = case_when(
    s116=="schedule caste" | s116=="schedule tribe" ~ "1: Schedule caste/tribe",
    s116=="obc" ~ "2: obc", 
    s116=="don't know" | s116=="none of them" ~ "3: Others"
  )
)
table(IR$caste)
prop.table(table(IR$caste))

# Ethnicity: v131--no variation 
prop.table(table(IR$v130))
IR$hindu <- ifelse(IR$v130=="hindu", 1, 0)
prop.table(table(IR$hindu))

# Literacy
IR$literacy <- ifelse(IR$v155=="cannot read at all", 0, 1)
IR$literacy[IR$v155=="blind/visually impaired" | IR$v155=="no card with required language"] <- NA
table(IR$literacy)
prop.table(table(IR$literacy))

# EDUCATION FACTORS
# Highest level education (not nessarily completed)
IR <- IR %>%
  mutate(ed.level=case_when(
    v149=="no education" ~ "0: no education",
    v149=="incomplete primary" ~ "1: incomplete primary",
    v149=="incomplete secondary" ~ "2: incomplete secondary",
    v149=="complete secondary" ~ "3: complete secondary/higher",
    v149=="higher" ~ "3: complete secondary/higher"
  ))
table(IR$ed.level)
prop.table(table(IR$ed.level))

# Woman has any education
IR$ed.any <- ifelse(IR$v149 == "no education", 0, 1)
table(IR$ed.any)
prop.table(table(IR$ed.any))

###################################
#The Prohibition of Child Marriage Act of 2006 came into effect in 2007, 
#raising the minimum age for marriage to 18 for women and 21 for men.
# Age at first marriage/cohabitation
table(IR$v511)
IR$early.cohab.18 <- ifelse(IR$v511<18, 1, 0)
table(IR$early.cohab.18)
prop.table(table(IR$early.cohab.18))

################################### sex experience 

#age at first menstrual period 
table(IR$s259)

# Age at first sex
table(IR$v531)
sum(is.na(IR$v531))
IR$v531 <- as.numeric(IR$v531)
summary(IR$v531)
IR$early.sex.18 <- ifelse(IR$v531 >0 & IR$v531 <18, 1, 0)
table(IR$early.sex.18)
prop.table(table(IR$early.sex.18))

###################################
# Age at first birth
table(IR$v212)
IR$v212 <- as.numeric(IR$v212)
IR$early.birth.18 <- ifelse(IR$v212<18, 1, 0)
table(IR$early.birth.18)
prop.table(table(IR$early.birth.18))

###################################
# Time from marriage to fist birth
IR$v221[IR$v221=="negative interval"] <- "-1"
IR$v221 <- as.numeric(IR$v221)
summary(IR$v221)
IR<- IR %>% mutate(cohab.birth.months = case_when(
  (v221 <12) ~ "1: within a year",
  (v221 >= 12 & v221 < 25) ~ "2: 1-2 years",
  (v221 >= 25 & v221 < 37) ~ "3: 2-3 years",
  (v221 >= 37 & v221 < 400) ~ "4: >3 years"))
table(IR$cohab.birth.months)
prop.table(table(IR$cohab.birth.months))



###################################
# Male child preference
table(IR$v627)
table(IR$v628)
IR <- IR %>%
  mutate(v627=as.numeric(v627),
         v628=as.numeric(v628))
IR$male.child.pref <- ifelse(IR$v627-IR$v628 > 0, 1, 0)
table(IR$male.child.pref)
prop.table(table(IR$male.child.pref))

#sex composition of children: 2=more sons than daughters, 1=equal, 2=no children
# label variable v202     "Sons at home"
# label variable v203     "Daughters at home"
# label variable v204     "Sons elsewhere"
# label variable v205     "Daughters elsewhere"
table(IR$v202)
table(IR$v203)
table(IR$v204)
table(IR$v205)
IR$sons <- IR$v202 + IR$v203
IR$daus <- IR$v204 + IR$v205
table(IR$sons)
table(IR$daus)
IR$more.sons <- ifelse(IR$sons > IR$daus, 1, 0)
table(IR$more.sons)
prop.table(table(IR$more.sons))

# ################################### gender role attitudes 
sum(is.na(IR$v744a))
table(IR$v744b)
table(IR$v744c)
table(IR$v744d)
table(IR$v744e)

# Attitudes about domestic violence
IR <- IR %>% mutate(dv.out = case_when(
  v744a =="no" ~0,
  v744a == "yes" ~1))
table(IR$dv.out)
prop.table(table(IR$dv.out))

IR <- IR %>% mutate(dv.negkid = case_when(
  v744b =="no" ~0,
  v744b == "yes" ~1))
table(IR$dv.negkid)
prop.table(table(IR$dv.negkid))

IR <- IR %>% mutate(dv.argue = case_when(
  v744c =="no" ~0,
  v744c == "yes" ~1))
table(IR$dv.argue)
prop.table(table(IR$dv.argue))

IR <- IR %>% mutate(dv.nosex = case_when(
  v744d =="no" ~0,
  v744d == "yes" ~1))
table(IR$dv.nosex)
prop.table(table(IR$dv.nosex))

IR <- IR %>% mutate(dv.burnfd = case_when(
  v744e =="no" ~0,
  v744e == "yes" ~1))
table(IR$dv.burnfd)
prop.table(table(IR$dv.burnfd))

# label variable s943f    "Justifies domestic violence: Wife unfaithful"
# label variable s943g    "Justifies domestic violence: Wife disrespect"
sum(is.na(IR$s943f))
table(IR$s943f)
IR <- IR %>% mutate(dv.unfaith = case_when(
  s943f =="no" ~0,
  s943f == "yes" ~1))
table(IR$dv.unfaith)
prop.table(table(IR$dv.unfaith))

IR <- IR %>% mutate(dv.disrespect = case_when(
  s943g =="no" ~0,
  s943g == "yes" ~1))
table(IR$dv.disrespect)
prop.table(table(IR$dv.disrespect))


IR$dv.accept <- ifelse(IR$v744a=="yes" | IR$v744b=="yes" | IR$v744c=="yes" | 
                         IR$v744d=="yes" | IR$v744e=="yes" |
                         IR$s943f=="yes" | IR$s943g == "yes", 1, 0)
table(IR$dv.accept)
prop.table(table(IR$dv.accept))



# ################################### IPV actualy experience


##experience of IPV 
# Physical spousal violence: push you, shake you, or throw something at you; slap you; twist your arm or pull
# your hair; punch you with his fist or with something that could hurt you; kick you, drag you, or beat you up;
# try to choke you or burn you on purpose; or threaten or attack you with a knife, gun, or any other weapon

# Sexual spousal violence: physically force you to have sexual intercourse with him even when you did not
# want to; physically force you to perform any other sexual acts you did not want to; force you with threats or
# in any other way to perform sexual acts you did not want to

# Emotional spousal violence: say or do something to humiliate you in front of others; threaten to hurt or harm
# you or someone close to you; insult you or make you feel bad about yourself

# Marital control
# Women whose current husband (if currently married) or most recent husband (if
# formerly married) demonstrates at least one of the following controlling
# behaviours: is jealous or angry if she talks to other men; frequently accuses her
# of being unfaithful; does not permit her to meet her female friends; tries to limit
# her contact with her family; insists on knowing where she is at all times; and does
# not trust her with any money.

table(IR$d102) #marital control
table(IR$d104) #emotional violence
table(IR$d106) #Experienced any less severe violence (D105A-C,J) by husband/partner"
table(IR$d107) #"Experienced any severe violence (D105D-F) by husband/partner"
table(IR$d108) #"Experienced any sexual violence (D105H-I,K) by husband/partner"
IR$ipv_control <- ifelse(IR$d102>0, 1, 0)
IR$ipv_emotion <- ifelse(IR$d104=="yes", 1, 0)
IR$ipv_physical <- ifelse(IR$d106=="yes" | IR$d107=="yes", 1, 0)
IR$ipv_sexual <- ifelse(IR$d108=="yes", 1, 0)
table(IR$ipv_control)
table(IR$ipv_emotion)
table(IR$ipv_physical)
table(IR$ipv_sexual)
prop.table(table(IR$ipv_control))
prop.table(table(IR$ipv_emotion))
prop.table(table(IR$ipv_physical))
prop.table(table(IR$ipv_sexual)) #too little variation 

IR$ipv.any <- ifelse(IR$d104=="yes" | IR$d106=="yes" | IR$d107=="yes" | IR$d108=="yes", 1, 0)
table(IR$ipv.any)
prop.table(table(IR$ipv.any))

#d121 "Respondent's father ever beat her mother": 17%
table(IR$d121)
IR$ipv.childhood <- ifelse(IR$d121=="yes", 1, 0)
table(IR$ipv.childhood)
prop.table(table(IR$ipv.childhood))
# 
# 
###################################
# Violence from other household/community members
table(IR$d115b )
sum(!is.na(IR$d115b))
# count the total number of adults by checking if each column is greater than 18 years
ipv_count <- function(row) {
  # Count the number of elements >=15
  result <- sum(row=="yes", na.rm = T)
  return(result)
}
IR$violence_other  <-  apply(dplyr::select(IR, starts_with("d115")), 1, ipv_count)
table(IR$violence_other)
summary(IR$violence_other)
IR$violence_other[IR$violence_other>1] <- 1
IR$violence_other[IR$ipv_sample==0] <- NA
table(IR$violence_other)
prop.table(table(IR$violence_other)) #too little variation


#afraid of husband 
sum(is.na(IR$d129))
table(IR$d129)
IR$his.afraid <- ifelse(IR$d129=="never afraid", 0, 1)
table(IR$his.afraid)
prop.table(table(IR$his.afraid)) 

######################################################################
# 2 | HOUSEHOLD RELATIONSHIPS
######################################################################

###################################
# Type of marriage, among married women
# table(IR$v505) # no variation at all

###################################
# Number of adults (15+) in the household
hv105 <- HH %>% dplyr:: select(starts_with("hv105_"))
lapply(hv105, table)
HH <- HH %>%
  mutate(across(starts_with("hv105_"), ~replace(., . == "don't know", NA))) ## don't know -> NA
HH <- HH %>%
  mutate(across(starts_with("hv105_"), ~replace(., . == "95+", "95"))) ## 95+ -> 95
HH <- HH %>%
  mutate(across(starts_with("hv105_"), as.numeric))
#check if all columns are numeric now 
hv105 <- HH %>% dplyr:: select(starts_with("hv105_"))
lapply(hv105, class)
lapply(hv105, table)
# count the total number of adults by checking if each column is greater than 18 years 
adult_count <- function(row) {
  # Count the number of elements >=15
  result <- sum(row >=15, na.rm = T)
  return(result)
}
HH$cohab.adult.sum  <-  apply(dplyr::select(HH, starts_with("hv105_")), 1, adult_count)
table(HH$cohab.adult.sum)
summary(HH$cohab.adult.sum)

# Recode cohab.adult.sum as binary
HH$cohab.adult.sum.4plus <- ifelse(HH$cohab.adult.sum>=4,1,0)
table(HH$cohab.adult.sum.4plus)
prop.table(table(HH$cohab.adult.sum.4plus))

###################################
# Partner does not live in the household (among women married or in union)
table(IR$v504)
prop.table(table(IR$v504))
IR <- IR %>% mutate(partner.absent = case_when(
  v504 == "living with her" ~ "No",
  v504 == "staying elsewhere" ~ "Yes"))
table(IR$partner.absent)
prop.table(table(IR$partner.absent))

###################################
# Sex of head of household
table(IR$v151)
IR$head.male <- ifelse(IR$v151 == "male", 1, 0)
table(IR$head.male)
prop.table(table(IR$head.male))


# HOUSE OWNERSHIP STATUS
# table(HH$sh62)
# sum(is.na(HH$sh62))
# HH$hh.house.owner.male <- ifelse(HH$sh62=="male member" | HH$sh62=="both", 1, 0)
# table(HH$hh.house.owner.male)
# prop.table(table(HH$hh.house.owner.male))

################################### FERTILITY 

#parity
table(IR$v201)
sum(is.na(IR$v201))
IR$child.ever <- ifelse(IR$v201>0, 1, 0)
table(IR$child.ever)
prop.table(table(IR$child.ever))

## total number of live births women gave birth to 
IR$num.births <- IR$v201 
table(IR$num.births)
IR$num.births.cat <- IR$num.births
IR$num.births.cat[IR$num.births>4] <- 5
IR$num.births.cat <- as.character(IR$num.births.cat)
IR$num.births.cat[IR$num.births.cat=="5"] <- "5+"
table(IR$num.births.cat)
prop.table(table(IR$num.births.cat))

# Number of pregnancies
#label variable v228     "Ever had a terminated pregnancy"
#label variable v234     "Other such pregnancies"
table(IR$v228)
table(IR$v234)
IR$num.preg <- ifelse(IR$v228 == "yes" & IR$v234 == "yes", IR$v201 + 2,
                      ifelse(IR$v228 == "yes" & IR$v234 == "no", IR$v201 + 1,
                             IR$v201))
table(IR$num.preg)
sum(is.na(IR$num.preg))
IR$num.preg.cat <- IR$num.preg
IR$num.preg.cat[IR$num.preg>4] <- 5
IR$num.preg.cat <- as.character(IR$num.preg.cat)
IR$num.preg.cat[IR$num.preg.cat=="5"] <- "5+"
table(IR$num.preg.cat)
prop.table(table(IR$num.preg.cat))

###################################
# Number of children living
class(IR$v218)
IR$num.children <- IR$v218
table(IR$num.children)
IR$num.children.cat <- IR$num.children
IR$num.children.cat[IR$num.children>4] <- 5
IR$num.children.cat <- as.character(IR$num.children.cat)
IR$num.children.cat[IR$num.children.cat=="5"] <- "5+"
table(IR$num.children.cat)
prop.table(table(IR$num.children.cat))


###################################
# Duration of marriage, among women still with first partner
table(IR$v512) #"Years since first cohabitation"
class(IR$v512)
sum(is.na(IR$v512))
summary(IR$v512)
IR$married.10years <- ifelse(IR$v512>10, 1, 0)
table(IR$married.10years)
prop.table(table(IR$married.10years))

###################################
# Number of children in the household: 
# Under 5
table(IR$v137)
sum(is.na(IR$v137))
IR <- IR %>% mutate(num.under5.cat = case_when(
  v137 == 0 ~ "0",
  v137 == 1 ~ "1",
  v137 >=2 ~ "2+"))
table(IR$num.under5.cat)
prop.table(table(IR$num.under5.cat))

# count the total number of adults by checking if each column is greater than 18 years 
child_count <- function(row) {
  # Count the number of elements >=15
  result <- sum(row <15, na.rm = T)
  return(result)
}
HH$cohab.child.sum  <-  apply(dplyr::select(HH, starts_with("hv105_")), 1, child_count)
table(HH$cohab.child.sum)
summary(HH$cohab.child.sum)

# Recode cohab.child.sum as binary
HH$cohab.child.cat <- ifelse(HH$cohab.child.sum>=3,3,HH$cohab.child.sum)
table(HH$cohab.child.cat)
prop.table(table(HH$cohab.child.cat))

# Ratio of kids under 5 to women 15-49
#label variable v137     "Number of children 5 and under in household (de jure)"
#label variable v138     "Number of eligible women in household (de facto)"
table(IR$v137)
table(IR$v138)
IR$hh.kid.wom.rat1 <- ifelse((IR$v137/IR$v138)>=1, 1, 0)
table(IR$hh.kid.wom.rat1)
prop.table(table(IR$hh.kid.wom.rat1))

#label variable v136     "Number of household members (listed)"
table(IR$v136)
summary(IR$v136)
IR <- IR%>% 
  mutate(hh.members.cat = case_when(
    v136 <=4 ~ 1,
    v136 ==5 ~ 2,
    v136 ==6 ~ 3,
    v136 >=7 ~ 4
  ))
table(IR$hh.members.cat)
prop.table(table(IR$hh.members.cat))

###################################
# Number of children alive
prop.table(table(IR$v218))
IR <- IR %>% 
  mutate(num.child.alive = case_when(
    v218==0 ~ "0",
    v218==1 ~ "1",
    v218==2 ~ "2",
    v218>=3 ~ "3+"
  ))
table(IR$num.child.alive)
prop.table(table(IR$num.child.alive))


###################################
# Ages of living children
# Number under 5
class(BR$b8)
BR <- BR %>% group_by(caseid) %>%
  mutate(kids.under5 = sum(b8 <5, na.rm = TRUE))
table(BR$kids.under5)
prop.table(table(BR$kids.under5))

BR <- BR %>% 
  mutate(kids.under5.cat = case_when(
  kids.under5 == 0 ~ "0",
  kids.under5 == 1 ~ "1",
  kids.under5 >= 2 ~ "2+"))
table(BR$kids.under5.cat)
prop.table(table(BR$kids.under5.cat))

# number under 15
BR <- BR %>% group_by(caseid) %>%
  mutate(kids.under15 = sum(b8 <15, na.rm = TRUE))
table(BR$kids.under15)
prop.table(table(BR$kids.under15))

BR <- BR %>% 
  mutate(kids.under15.cat = case_when(
  kids.under15==0 ~ "0",
  kids.under15==1 ~ "1",
  kids.under15==2 ~ "2",
  kids.under15==3 ~ "3",
  kids.under15>=4 ~ "4+"))
table(BR$kids.under15.cat)
prop.table(table(BR$kids.under15.cat))

###################################
# CHILDREN THAT HAVE DIED
IR$child.die <- ifelse(IR$v206 > 0 | IR$v207>0, 1, 0)
table(IR$child.die)
prop.table(table(IR$child.die))

###################################
# Number of biological children in the household
table(IR$v202)
table(IR$v203)
IR <- IR %>%
  rowwise() %>%
  mutate(num.kids.house = sum(v202, v203, na.rm=TRUE))
table(IR$num.kids.house)
prop.table(table(IR$num.kids.house))

IR <- IR %>% 
  mutate(num.kids.house.cat = case_when(
  num.kids.house==0 ~ "0",
  num.kids.house==1 ~ "1",
  num.kids.house==2 ~ "2",
  num.kids.house>=3 ~ "3+"))
table(IR$num.kids.house.cat)
prop.table(table(IR$num.kids.house.cat))



###################################
# Difference between woman's and partner's age: whether husband is 1 year older than the wife
sum(is.na(IR$v730))
IR$his.older <- ifelse((IR$v730 - IR$v012)>1, 1, 0)
table(IR$his.older)
prop.table(table(IR$his.older))


# RESPONDENT EARNS MORE THAN HUSBAND/PARTNER --- THIS VAR missing a lot, check to see pop? 
# table(IR$v746)
# IR$her.earn.more <- IR$v746
# table(IR$her.earn.more)
# prop.table(table(IR$her.earn.more))

###################################
# Husband's education
sum(is.na(IR$v729))
table(IR$v729)
# Highest level of education
IR <- IR %>%
  mutate(ed.lev.his=case_when(
    v729=="no education" ~ "0: no education",
    v729=="incomplete primary" ~ "1: incomplete primary",
    v729=="incomplete secondary" ~ "2: incomplete secondary",
    v729=="complete secondary" ~ "3: complete secondary/higher",
    v729=="higher" ~ "3: complete secondary/higher"
  ))
table(IR$ed.lev.his)
prop.table(table(IR$ed.lev.his))
# 
# 
# #Sought help to stop violence: only 11% would seek help 
# #BASE: Women who experienced violence 
# sum(is.na(IR$d119h))
# table(IR$d119h)
# IR$help.violence <- ifelse(IR$d119h=="yes" | 
#                              IR$d119i=="yes" | 
#                              IR$d119j=="yes" | 
#                              IR$d119k=="yes" | 
#                              IR$d119u=="yes" | 
#                              IR$d119x=="yes" | 
#                              IR$d119y=="yes" | 
#                              IR$d119xb=="yes" | 
#                              IR$d119xd=="yes" | 
#                              IR$d119xe=="yes" |
#                              IR$d119xf=="yes" |
#                              IR$d119xg=="yes" |
#                              IR$d119xh=="yes" , 1, 0)
# table(IR$help.violence)
# prop.table(table(IR$help.violence)) 

# EVER TOLD ANYONE ELSE ABOUT VIOLENCE: no variation
# table(IR$d128)
# IR$tell.violence <- ifelse(IR$d128=="yes", 1, 0)
# table(IR$tell.violence)
# prop.table(table(IR$tell.violence)) 




## DECISION-MAKING

# # JOINT DECISION: FINANCIAL -- only avaiable for women who have earnings? 
# table(IR$v739)

#v632     "Decision maker for using contraception" (only among users)
# table(IR$v313)
#v467b    "Getting medical help for self: getting permission to go"
sum(is.na(IR$v467b))
table(IR$v467b)
IR$medical.permit <- ifelse(IR$v467b=="no problem", 1, 0)
table(IR$medical.permit)
prop.table(table(IR$medical.permit)) 

#s947     "Can you say no to your husband if you do not want to have sexual intercourse?" 
sum(is.na(IR$s947))
table(IR$s947)
IR$say.no.sex <- ifelse(IR$s947=="yes", 1, 0)
table(IR$say.no.sex)
prop.table(table(IR$say.no.sex)) 

# label variable s944     "If a wife knows her husband has a sexually transmitted disease, is she justified"
# label variable s945     "If a wife knows her husband has sex with other women, is she justified in refusi"
IR$say.no.sex.sti <- ifelse(IR$s944=="yes", 1, 0)
table(IR$say.no.sex.sti)
prop.table(table(IR$say.no.sex.sti)) 

IR$say.no.sex.otherwomen <- ifelse(IR$s945=="yes", 1, 0)
table(IR$say.no.sex.otherwomen)
prop.table(table(IR$say.no.sex.otherwomen)) 


#v743a    "Person who usually decides on respondent's health care"
table(IR$v743a)
IR$joint.health <- ifelse(IR$v743a=="respondent alone" | IR$v743a=="respondent and husband/partner", 1, 0)
table(IR$joint.health)
prop.table(table(IR$joint.health)) 

#v743b    "Person who usually decides on large household purchases"
table(IR$v743b)
IR$joint.large <- ifelse(IR$v743b=="respondent alone" | IR$v743b=="respondent and husband/partner", 1, 0)
table(IR$joint.large)
prop.table(table(IR$joint.large)) 

#v743d    "Person who usually decides on visits to family or relatives"
table(IR$v743d)
IR$joint.visit <- ifelse(IR$v743d=="respondent alone" | IR$v743d=="respondent and husband/partner", 1, 0)
table(IR$joint.visit)
prop.table(table(IR$joint.visit)) 

#v743f    "Person who usually decides what to do with money husband earns"
table(IR$v743f)
IR$joint.his.money <- ifelse(IR$v743f=="respondent alone" | IR$v743f=="respondent and husband/partner", 1, 0)
table(IR$joint.his.money)
prop.table(table(IR$joint.his.money)) 


# HOUSE OWNERSHIP 
table(IR$v745a)
prop.table(table(IR$v745a))
IR$her.own.house <- ifelse(IR$v745a=="does not own", 0, 1)
table(IR$her.own.house)
prop.table(table(IR$her.own.house)) 

# OWNS LAND ALONE OR JOINTLY
table(IR$v745b)
prop.table(table(IR$v745b))
IR$her.own.land <- ifelse(IR$v745b=="does not own", 0, 1)
table(IR$her.own.land)
prop.table(table(IR$her.own.land)) 
# 

#s929     "Do you have any money of your own that you alone can decide how to use"
table(IR$s929)
IR$her.own.money <- ifelse(IR$s929=="yes", 1, 0)
table(IR$her.own.money)
prop.table(table(IR$her.own.money))

# label variable s931     "Do you have a bank or savings account that you yourself use"
# label variable s932     "Do you have any mobile phone that you yourself use"
# label variable s933     "Do you use your mobile phone for any financial transaction?"
# label variable s934     "Have you ever used the internet?"
# label variable s937     "Are you able to read text (SMS) messages"

IR$her.own.bank <- ifelse(IR$s931=="yes", 1, 0)
table(IR$her.own.bank)
prop.table(table(IR$her.own.bank))

IR$her.own.phone <- ifelse(IR$s932=="yes", 1, 0)
table(IR$her.own.phone)
prop.table(table(IR$her.own.phone))

table(IR$s933)
IR$her.own.phone.finance <- ifelse(IR$s933=="yes", 1, 0)
IR$her.own.phone.finance[IR$s932=="no"] <- 0
table(IR$her.own.phone.finance)
prop.table(table(IR$her.own.phone.finance)) #no variation



######################################################################
######################################################################
# 3 | HOUSEHOLD ECONOMICS


## EMPLOYMENT FACTORS

# RESPONDENT CURRENTLY WORKS
table(IR$v714)
IR$work.current <- ifelse(IR$v714=="yes", 1, 0)
table(IR$work.current)
prop.table(table(IR$work.current))

# RESPONDENT WORKED IN PAST !@ MONTHS
table(IR$v731)
IR$work.12mo <- ifelse(IR$v731=="no", 0, 1)
table(IR$work.12mo)
prop.table(table(IR$work.12mo))

# RESPONDENT OCCUPATION TYPES 
table(IR$v717)
prop.table(table(IR$v717))
sum(is.na(IR$v717))
IR <- IR %>% mutate(work.type = case_when(
  v717 == "not working" ~ "0: No work",
  v717 == "agricultural" ~ "1: agricultural"))
IR$work.type[IR$v717!="not working" & IR$v717!="agricultural"] <- "2: others"
table(IR$work.type)
prop.table(table(IR$work.type))

# WORK SEASONALITY
IR <- IR %>% mutate(work.seasonal = case_when(
  v731 == "no" ~ "0: No work in past 12 months",
  v732 == "occasional" ~ "1: Occassional/Seasonal",
  v732 == "seasonal" ~ "1: Occassional/Seasonal",
  v732 == "all year" ~ "2: All year"))
table(IR$work.seasonal)
prop.table(table(IR$work.seasonal))

table(IR$v719) #no variation


# HUSBAND OCCUPATION TYPES 
sum(is.na(IR$v705))
table(IR$v705)
IR <- IR %>% mutate(work.type.his = case_when(
  v705 == "agricultural" ~ "1: agricultural",
  v705 == "skilled and unskilled manual" ~ "2: skilled and unskilled manual") )
IR$work.type.his[IR$v705!="skilled and unskilled manual" & IR$v705!="agricultural" & !is.na(IR$v705)] <- "3: others"
table(IR$work.type.his)
prop.table(table(IR$work.type.his))

# HUSBAND/PARTNER WORKING
table(IR$v704a)
IR <- IR %>% mutate(work.12mo.his = case_when(
  v704a == "didn't work last 12 months" ~ "0: Didn't work last 12 months",
  v704a == "worked last 7 days" ~ "1: Worked last 7 days",
  v704a == "worked last 12 months" ~ "2: Worked last 12 months"))
table(IR$work.12mo.his)
prop.table(table(IR$work.12mo.his))

# BINARY FACTOR FOR HUSBAND/PARTNER EMPLOYED CURRENTLY
IR$work.current.his <- ifelse(IR$work.12mo.his =="1: Worked last 7 days", 1, 0)
table(IR$work.current.his)
prop.table(table(IR$work.current.his))


## HOUSEHOLD ASSETS

# ELECTRICITY
table(HH$hv206)
HH <- HH %>% mutate(hh.electricity = case_when(
  hv206 == "yes" ~ 1,
  hv206 == "no" ~ 0))
table(HH$hh.electricity)
prop.table(table(HH$hh.electricity))

# RADIO: ALMOST NO VARIATION 
# HH <- HH %>% mutate(hh.radio = case_when(
#   hv207 == "yes" ~ 1,
#   hv207 == "no" ~ 0))
# table(HH$hh.radio)
# prop.table(table(HH$hh.radio))

# TELEVISION
HH <- HH %>% mutate(hh.tv = case_when(
  hv208 == "yes" ~ 1,
  hv208 == "no" ~ 0))
table(HH$hh.tv)
prop.table(table(HH$hh.tv))

# REFRIGIRATOR
HH <- HH %>% mutate(hh.refrig = case_when(
  hv209 == "yes" ~ 1,
  hv209 == "no" ~ 0))
table(HH$hh.refrig)
prop.table(table(HH$hh.refrig))

# BICYCLE
HH <- HH %>% mutate(hh.bike = case_when(
  hv210 == "yes" ~ 1,
  hv210 == "no" ~ 0))
table(HH$hh.bike)
prop.table(table(HH$hh.bike))

# MOTORCYCLE
HH <- HH %>% mutate(hh.motor = case_when(
  hv211 == "yes" ~ 1,
  hv211 == "no" ~ 0))
table(HH$hh.motor)
prop.table(table(HH$hh.motor))

# CAR: ALMOST NO VARIATION 
# HH <- HH %>% mutate(hh.car = case_when(
#   hv212 == "yes" ~ 1,
#   hv212 == "no" ~ 0))
# table(HH$hh.car)
# prop.table(table(HH$hh.car))

# MOBILE PHONE: ALMOST NO VARIATION 
# HH <- HH %>% mutate(hh.phone = case_when(
#   hv243a == "yes" ~ 1,
#   hv243a == "no" ~ 0))
# table(HH$hh.phone)
# prop.table(table(HH$hh.phone))


# ANIMAL-DRAWN CART: ALMOST NO VARIATION 
# HH <- HH %>% mutate(hh.cart = case_when(
#   hv243c == "yes" ~ 1,
#   hv243c == "no" ~ 0))
# table(HH$hh.cart)
# prop.table(table(HH$hh.cart))

# COMPUTER: ALMOST NO VARIATION 
# HH <- HH %>% mutate(hh.computer = case_when(
#   hv243e == "yes" ~ 1,
#   hv243e == "no" ~ 0))
# table(HH$hh.computer)
# prop.table(table(HH$hh.computer))


# INTERNET
table(HH$sh50n)
HH$hh.internet <- ifelse(HH$sh50n == "yes", 1, 0)
table(HH$hh.internet)
prop.table(table(HH$hh.internet))

# AIR CONDITIONER
table(HH$sh50q)
HH$hh.ac <- ifelse(HH$sh50q == "yes", 1, 0)
table(HH$hh.ac)
prop.table(table(HH$hh.ac))

# Washing machine
table(HH$sh50r )
HH$hh.wash.machine <- ifelse(HH$sh50r == "yes", 1, 0)
table(HH$hh.wash.machine)
prop.table(table(HH$hh.wash.machine))

# OWN LAND USABLE FOR AGRICULTURE
table(HH$hv244)
HH$hh.land <- ifelse(HH$hv244 == "yes", 1, 0)
table(HH$hh.land)
prop.table(table(HH$hh.land))

# HECTARES FOR AGRICULTURAL LAND
table(HH$hv245)
HH$hv245[HH$hv245=="95 or more"] <- "96"
HH$hv245 <- as.numeric(HH$hv245)
summary(HH$hv245)
HH <- HH %>% mutate(hh.land.ha = case_when(
  hv244 == "no" ~ "0",
  hv245 %in% c(1:5) ~ "1: 0.1-0.5 ha",
  hv245 %in% c(6:10) ~ "2: 0.6-1.0 ha",
  hv245 %in% c(10:950) ~ "3: More than 1.0 ha"))
table(HH$hh.land.ha)
prop.table(table(HH$hh.land.ha))

# LIVESTOCK, HERDS, OR ANIMALS
HH$hh.animal <- ifelse(HH$hv246 == "yes", 1, 0)
table(HH$hh.animal)
prop.table(table(HH$hh.animal))

# BANK: NO VARIATION
# HH <- HH %>% mutate(hh.bank = case_when(
#   hv247 == "yes" ~ 1,
#   hv247 == "no" ~ 0))
# table(HH$hh.bank)
# prop.table(table(HH$hh.bank))

# URBAN/RURAL WEALTH INDEX
HH$wealth.index.ur <- factor(HH$hv270a)
HH <- HH %>%
  mutate(wealth.index.ur=case_when(
    hv270a=="poorest" ~ "1: poorest",
    hv270a=="poorer" ~ "2: poorer",
    hv270a=="middle" ~ "3: middle",
    hv270a=="richer" ~ "4: richer",
    hv270a=="richest" ~ "5: richest"
  ))
table(HH$wealth.index.ur)
prop.table(table(HH$wealth.index.ur))



sum(is.na(IR$v481))
table(IR$v481)


######################################################################
# 4 | HEALTHCARE AND MENTAL MODELS 



# HEALTH INSURANCE 
table(IR$v481)
IR$health.insurance <- ifelse(IR$v481=="yes", 1, 0)
table(IR$health.insurance)
prop.table(table(IR$health.insurance))

#label variable v474     "Heard of tuberculosis or TB"
sum(is.na(IR$v474))
table(IR$v474)
#Knowledge and attitudes toward tuberculosis 
#Misconceptions about the transmission of TB include that it can be spread by sharing utensils, 
#by touching a person with TB, through food, through sexual contact, through mosquito bites, 
#or through any other means except through the air when coughing or sneezing.
sum(is.na(IR$v474a))
table(IR$v474a)
IR$know.tb.cough <- ifelse(IR$v474a=="yes", 1, 0)
table(IR$know.tb.cough)
prop.table(table(IR$know.tb.cough))

IR$know.tb.misinfo <- ifelse(IR$v474b=="yes" |
                               IR$v474c=="yes" |
                               IR$v474d=="yes" |
                               IR$v474e=="yes" |
                               IR$v474f=="yes" , 1, 0)
table(IR$know.tb.misinfo)
prop.table(table(IR$know.tb.misinfo))

IR$know.tb.cure <- ifelse(IR$v475=="yes", 1, 0)
table(IR$know.tb.cure)
prop.table(table(IR$know.tb.cure)) ##no variation

#This section presents the findings from NFHS-5 on HIV/AIDS knowledge and attitudes, prior
# HIV testing, and the prevalence of sexually transmitted infections (STIs), as well as sexual
# behaviour of the adult and youth population. Questions on HIV/AIDS were included only in
# the subsample of households selected for the state module.

#label variable v750     "Ever heard of a Sexually Transmitted Infection (STI)"
# label variable v751     "Ever heard of AIDS"
# label variable v837     "Heard of drugs to help HIV infected people live longer"
table(IR$v750)
table(IR$v751)
# table(IR$v837)
# sum(is.na(IR$v837)) #missing to much

IR$heard.sti <- ifelse(IR$v750=="yes", 1, 0)
table(IR$heard.sti)
prop.table(table(IR$heard.sti))


IR$heard.hiv <- ifelse(IR$v751=="yes", 1, 0)
table(IR$heard.hiv)
prop.table(table(IR$heard.hiv))

#label variable s1004a   "Source of information about AIDS: Radio"
# label variable s1004b   "Source of information about AIDS: Television"
# label variable s1004c   "Source of information about AIDS: Cinema"
# label variable s1004d   "Source of information about AIDS: Newspapers/magazines"
# label variable s1004e   "Source of information about AIDS: Posters/hoardings"
# label variable s1004f   "Source of information about AIDS: Exhibition/mela"
# label variable s1004p   "Source of information about AIDS: INTERNET"
IR$heard.hiv.media <- ifelse(IR$s1004a=="yes" |
                               IR$s1004b=="yes" |
                               IR$s1004c=="yes" |
                               IR$s1004d=="yes" |
                               IR$s1004e=="yes" |
                               IR$s1004f=="yes" |
                               IR$s1004p=="yes" , "2: media", "1: not media")

IR$heard.hiv.media[IR$v751=="no"] <- "0: never heard of HIV"
sum(is.na(IR$heard.hiv.media))
table(IR$heard.hiv.media)
prop.table(table(IR$heard.hiv.media)) 

# Only 13 percent of women and 22
# percent of men in Uttar Pradesh have
# a ‘comprehensive knowledge’ about
# HIV/AIDS. This means they know
# that consistent use of condoms every
# time they have sex and having just
# one uninfected sex partner who has
# no other partners can reduce the
# chance of getting HIV/AIDS, they
# know that a healthy-looking person
# can have HIV/AIDS, and they reject
# two common misconceptions about
# the transmission or prevention of
# HIV/AIDS.

# label variable v754cp   "Reduce risk of getting HIV: always use condoms during sex"
# label variable v754dp   "Reduce risk of getting HIV: have 1 sex partner only, who has no other partners"
# label variable v754jp   "Can get HIV from mosquito bites"
# label variable v754wp   "Can get HIV by sharing food with person who has AIDS"
# label variable v756     "A healthy looking person can have HIV"
table(IR$v754cp)
table(IR$v754dp)
table(IR$v754jp)
table(IR$v754wp)
table(IR$v756)

IR$know.hiv.reduce <- ifelse(IR$v754cp=="yes" & IR$v754dp=="yes", "2: Yes", "1: No")
IR$know.hiv.reduce[IR$v751=="no"] <- "0: Never heard of hiv"
table(IR$know.hiv.reduce)
prop.table(table(IR$know.hiv.reduce))

IR$know.hiv.misinfo <- ifelse(IR$v754jp=="yes" | IR$v754wp=="yes" | IR$v756=="no", "2: Yes", "1: No")
IR$know.hiv.misinfo[IR$v751=="no"] <- "0: Never heard of hiv"
table(IR$know.hiv.misinfo)
prop.table(table(IR$know.hiv.misinfo))

# label variable v825     "Would buy vegetables from vendor with HIV"
# label variable s1040    "Do you think a child with HIV should be allowed to attend school with students w"
# label variable s1042    "Do you fear that you could get HIV if you come into contact with the saliva of a"
# label variable s1046    "Do you think that people living with HIV should be treated in the same public ho"
# label variable s1047    "Do you think that people living with HIV should be allowed to work in the same o"
##missing a lot 
IR$know.hiv.stigma <- ifelse(IR$s1040=="no" | 
                               IR$s1042=="no" | 
                               IR$s1046=="no" | 
                               IR$s1047=="no", "2: stigma", "1: no stigma")
IR$know.hiv.stigma[IR$v751=="no"] <- "0: Never heard of HIV"
table(IR$know.hiv.stigma)
prop.table(table(IR$know.hiv.stigma))


#ORS
table(IR$v416)
sum(is.na(IR$v416))
IR$heard.ors <- ifelse(IR$v416=="never heard of", 0, 1)
table(IR$heard.ors)
prop.table(table(IR$heard.ors))

# 
# #Receive financial assistance for delivery care (for underfive children)
# IR$finance.assist.deliver <- ifelse(IR$s457_1=="yes" | 
#                                       IR$s457_2=="yes" | 
#                                       IR$s457_3=="yes" | 
#                                       IR$s457_4=="yes" | 
#                                       IR$s457_5=="yes", 1, 0)
# table(IR$finance.assist.deliver)
# prop.table(table(IR$finance.assist.deliver))
# 
# 
# #correct denominator 
# IR$finance.assist.deliver[(!is.na(IR$s457_1) | 
#                              !is.na(IR$s457_2) | 
#                              !is.na(IR$s457_3) | 
#                              !is.na(IR$s457_4) | 
#                              !is.na(IR$s457_5)) & is.na(IR$finance.assist.deliver)] <- 0 
# table(IR$finance.assist.deliver)
# prop.table(table(IR$finance.assist.deliver))
# 


##medical barriers 
# label variable v467b    "Getting medical help for self: getting permission to go"
# label variable v467c    "Getting medical help for self: getting money needed for treatment"
# label variable v467d    "Getting medical help for self: distance to health facility"
# label variable v467e    "Getting medical help for self: having to take transport"
# label variable v467f    "Getting medical help for self: not wanting to go alone"
# label variable v467g    "Getting medical help for self: concern no female health provider"
# label variable v467h    "Getting medical help for self: concern no provider"
# label variable v467i    "Getting medical help for self: concern no drugs available"
IR$medical.money <- ifelse(IR$v467c=="no problem", 1, 0)
table(IR$medical.money)
prop.table(table(IR$medical.money)) 

IR$medical.distance <- ifelse(IR$v467d=="no problem", 1, 0)
table(IR$medical.distance)
prop.table(table(IR$medical.distance)) 

IR$medical.transport <- ifelse(IR$v467e=="no problem", 1, 0)
table(IR$medical.transport)
prop.table(table(IR$medical.transport)) 

IR$medical.alone <- ifelse(IR$v467f=="no problem", 1, 0)
table(IR$medical.alone)
prop.table(table(IR$medical.alone)) 

IR$medical.female.provider <- ifelse(IR$v467g=="no problem", 1, 0)
table(IR$medical.female.provider)
prop.table(table(IR$medical.female.provider)) 

IR$medical.no.provider <- ifelse(IR$v467h=="no problem", 1, 0)
table(IR$medical.no.provider)
prop.table(table(IR$medical.no.provider)) 

IR$medical.no.drug <- ifelse(IR$v467i=="no problem", 1, 0)
table(IR$medical.no.drug)
prop.table(table(IR$medical.no.drug)) 

# label variable v384a    "Heard family planning on radio last few months"
# label variable v384b    "Heard family planning on TV last few months"
# label variable v384c    "Heard family planning in newspaper/magazine last few months"
# label variable s617d    "Heard family planning on wall painting or hoarding"
# label variable s617e    "Heard family planning on INTERNET"
sum(is.na(IR$v384a))
sum(is.na(IR$v384b))
sum(is.na(IR$v384c))
sum(is.na(IR$s617d))
sum(is.na(IR$s617e))
prop.table(table(IR$v384a))
prop.table(table(IR$v384b))
prop.table(table(IR$v384c))
prop.table(table(IR$s617d))
prop.table(table(IR$s617e))
IR$heard.FP.media <- ifelse(IR$v384a=="yes" |
                              IR$v384b=="yes" |
                              IR$v384c=="yes" |
                              IR$s617d=="yes" |
                              IR$s617e=="yes", 1, 0)
table(IR$heard.FP.media)
prop.table(table(IR$heard.FP.media)) 



######################################################################
######################################################################
# 5 | LIFE IN THE COMMUNITY 

# label variable s940     "Do you know of any programmes in this area that give loans to women to start or "
# label variable s941     "Have you yourself ever taken a loan, in cash or in kind, from any of these progr"

table(IR$s940)
sum(is.na(IR$s940))
IR$know.womanloan.program <- ifelse(IR$s940=="yes", 1, 0)
table(IR$know.womanloan.program)
prop.table(table(IR$know.womanloan.program)) 

table(IR$s941)
IR$take.loan.program <- ifelse(IR$s941=="yes", 1, 0)
IR$take.loan.program[IR$s940=="no"] <- 0
table(IR$take.loan.program)
prop.table(table(IR$take.loan.program)) #no variation

#drinking water source from neighbor or the community : no variation
# IR$community.water <- ifelse(IR$v113=="piped to neighbor" |
#                                IR$v113=="public tap/standpipe" |
#                                IR$v113=="community or plant", 1, 0)
# table(IR$community.water)
# prop.table(table(IR$community.water)) #6%--we can still try to do something...


#source of FP for users and nonusers 
table(IR$v326)
table(IR$v327)
#v3a00r   "Source of family planning for non-users: Shop / Friend / Relative"
table(IR$v3a00r) #almost 10% 
#label variable v3a00s   "Source of family planning for non-users: Public: Anganwadi / ICDS Centre"
# label variable v3a00t   "Source of family planning for non-users: Public: ASHA"
# label variable v3a00u   "Source of family planning for non-users: Public: Other Community-Based Worker"
# label variable v3a00v   "Source of family planning for non-users: Other Public Health sector"
# label variable v3a00w   "Source of family planning for non-users: NGO or Trust Hospital / Clinic"
table(IR$v3a00s)
table(IR$v3a00t)
table(IR$v3a00u)
table(IR$v3a00v)
table(IR$v3a00w)
# table(IR$v313)
# table(IR$v313[!is.na(IR$v3a00s)])


IR$source.fp.friend <- ifelse(IR$v327=="shop, church, friend" |
                                IR$v3a00r=="yes", 1, 0)
IR$source.fp.friend[is.na(IR$source.fp.friend)] <- 0
table(IR$source.fp.friend)
prop.table(table(IR$source.fp.friend))


IR$source.fp.chw <- ifelse(IR$v326=="public: anganwadi/icds centre" |
                             IR$v326=="public: asha" | 
                             IR$v326=="public: other comm-based worker" |
                             IR$v3a00s=="yes" | IR$v3a00t=="yes" | IR$v3a00u=="yes", 1, 0)
IR$source.fp.chw[is.na(IR$source.fp.chw)] <- 0
table(IR$source.fp.chw)
prop.table(table(IR$source.fp.chw))
sum(is.na(IR$source.fp.chw))

# #S356		Told by a health worker about any methods of family planning
# sum(is.na(IR$s356))
# IR$told.fp.chw <- ifelse(IR$s356=="yes", 1, 0)
# table(IR$told.fp.chw)
# prop.table(table(IR$told.fp.chw))

### source of HIV info: other ppl in the community
# label variable s1004g   "Source of information about AIDS: Health workers"
# label variable s1004h   "Source of information about AIDS: Adult education programme"
# label variable s1004i   "Source of information about AIDS: Religious leaders"
# label variable s1004j   "Source of information about AIDS: Political leaders"
# label variable s1004k   "Source of information about AIDS: School/teachers"
# label variable s1004l   "Source of information about AIDS: Community meetings"
# label variable s1004m   "Source of information about AIDS: Husband"
# label variable s1004n   "Source of information about AIDS: Friends/relatives"
# label variable s1004o   "Source of information about AIDS: Work place"

IR$heard.hiv.community <- ifelse(IR$s1004g=="yes" |
                               IR$s1004h=="yes" |
                               IR$s1004i=="yes" |
                               IR$s1004j=="yes" |
                               IR$s1004k=="yes" |
                               IR$s1004l=="yes" |
                               IR$s1004m=="yes" |
                                 IR$s1004n=="yes" |
                                 IR$s1004o=="yes" 
                                 , "2: community", "1: not community")

IR$heard.hiv.community[IR$v751=="no"] <- "0: never heard of HIV"
sum(is.na(IR$heard.hiv.community))
table(IR$heard.hiv.community)
prop.table(table(IR$heard.hiv.community)) 

#s361     "Met with an anganwadi worker, ASHA or other community health worker in last 3 mo": ALL WOMEN 
sum(is.na(IR$s361))
table(IR$s361)
IR$chw.3mo <- ifelse(IR$s361=="yes", 1, 0)
table(IR$chw.3mo)
prop.table(table(IR$chw.3mo))

#The type of person who gave prenatal care to the respondent prior to the last birth.
table(IR$m2h_1)
table(IR$m2i_1)
table(IR$m2j_1)
IR$chw.prenatal <- ifelse(IR$m2h_1=="yes" | IR$m2i_1=="yes" | IR$m2j_1=="yes", 1, 0)
table(IR$chw.prenatal)
prop.table(table(IR$chw.prenatal))
sum(is.na(IR$chw.prenatal))
# 
# ###before discharge 
# table(IR$m64_1)
# table(IR$m68_1)
# IR$discharge.asha <- ifelse(IR$m64_1=="asha" | IR$m68_1=="asha" , "2: Yes", "1: No")
# 
# ##last birth: transport and who arranged it? 
# table(IR$s449_1) #Transport used by respondent to go to health facility for delivery
# table(IR$s450a_1)
# table(IR$s450b_1)
# table(IR$s450c_1)
# table(IR$s450d_1)
# table(IR$s450e_1) #Person who arranged transport: ASHA
# 
# IR$deliver.trans.asha <- ifelse(IR$s450e_1=="yes", 1, 0)
# table(IR$deliver.trans.asha)
# prop.table(table(IR$deliver.trans.asha))
# sum(is.na(IR$deliver.trans.asha))

#difference: ANM, Health Worker, Anganwadi Worker, ASHA, PRI Member, NGO, CBO
# QUESTION: how to correctly code CHW? 


######################################################################
# 6 | LARGER ENVIRONMENT
######################################################################


###################################
# Air quality in the house / ventilation
table(HH$hv226)
### COOKING FUEL / AIR QUALITY 
# categorize to "smoke-producing cooking fuel" (1) versus “non-smoke-producing fuels” (0)
# REF: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8374520/
#[1] Wood
#[2] Charcoal
#[3] LPG/Natural gas (YES)
#[4] Straw/shrubs/grass
#[5] Agricultural crop
#[6] Animal dung
#[7] Parafin/Kerosene
#[8] Electricity (YES)
HH$hh.cook.fuel.nosmoke <-  ifelse(HH$hv226=="electricity" | HH$hv226=="lpg", 1, 0)
table(HH$hh.cook.fuel.nosmoke)
prop.table(table(HH$hh.cook.fuel.nosmoke))

#label variable hv241    "Food cooked in the house/ separate building/ outdoors"
table(HH$hv241)
HH$hh.cook.inside <-  ifelse(HH$hv241=="in the house", 1, 0)
table(HH$hh.cook.inside)
prop.table(table(HH$hh.cook.inside))


###################################
# Number of rooms for sleeping
table(HH$hv216)
HH$hv216[HH$hv216=="25+"] <- "25"
HH$hv216 <- as.numeric(HH$hv216)
HH <- HH %>% mutate(hh.bedrooms = case_when(
  hv216 <=1 ~ "0-1",
  hv216 == 2 ~ "2",
  hv216 >= 3 ~ "3+"))
table(HH$hh.bedrooms)
prop.table(table(HH$hh.bedrooms))

###################################
# Type of toilet facility: WASH standard - flush and not shared 
table(HH$hv205)
table(HH$hv225)
HH$hh.WASH.toilet <- ifelse(HH$hv225=="no" & 
                              (HH$hv205== "flush to piped sewer system" |
                                 HH$hv205== "flush to septic tank" ), 1, 0)
table(HH$hh.WASH.toilet)
prop.table(table(HH$hh.WASH.toilet))


###################################
# Location of toilet facility
# table(HH$hv238a)
# prop.table(table(HH$hv238a))
# HH$hh.toilet.locate <- ifelse(HH$hv238a=="in own dwelling", 1, 0)
# table(HH$hh.toilet.locate)
# prop.table(table(HH$hh.toilet.locate))

###################################
# Source of drinking water
table(HH$hv201)
HH$hh.pipe.water <- ifelse(HH$hv201 %in% c( "piped into dwelling", "piped to yard/plot", "public tap/standpipe"), 1, 0)
table(HH$hh.pipe.water)
prop.table(table(HH$hh.pipe.water))


###################################
# Water treatment
## Anything done to make water safe to drink
table(HH$hv237)
HH$hh.water.purify <- ifelse(HH$hv237=="yes", 1, 0)
table(HH$hh.water.purify)
prop.table(table(HH$hh.water.purify))

###################################
# Time to get water
table(HH$hv204)
HH$hh.water.premise <- ifelse(HH$hv204=="on premises",1,0)
table(HH$hh.water.premise)
prop.table(table(HH$hh.water.premise))

###################################
# MEDIA EXPOSURE
# label variable v157     "Frequency of reading newspaper or magazine"
# label variable v158     "Frequency of listening to radio"
# label variable v159     "Frequency of watching television"
table(IR$v157)
table(IR$v158)
table(IR$v159)
IR$media <- ifelse(IR$v157=="not at all" & IR$v158=="not at all" & IR$v159=="not at all", 0, 1)
table(IR$media)
prop.table(table(IR$media))


IR$her.use.internet <- ifelse(IR$s934=="yes", 1, 0)
table(IR$her.use.internet)
prop.table(table(IR$her.use.internet))

IR$her.read.text <- ifelse(IR$s937=="yes", 1, 0)
table(IR$her.read.text)
prop.table(table(IR$her.read.text))

# HAND WASHING STATION OBSERVED: NO VARIATION
# table(HH$hv230b)



# RESPONDENT HAS LANGLINE-TELEPHONE: NO VARIATION
# table(IR$v153)
# prop.table(table(IR$v153))


# TYPE OF FLOOR
table(HH$hv213)
HH$floor.slum <- ifelse(HH$hv213 == "mud / clay / earth" | HH$hv213== "dung" | HH$hv213== "raw wood planks" | HH$hv213== "palm / bamboo" |
                     HH$hv213=="sand",1,0)
table(HH$floor.slum)
prop.table(table(HH$floor.slum))

# TYPE OF WALL
table(HH$hv214 )
HH$wall.slum <- ifelse( HH$hv214 == "bamboo with mud" |
                         HH$hv214== "cane / palm / trunks" |
                         HH$hv214 == "cardboard" | 
                         HH$hv214== "grass / reeds / thatch" |
                         HH$hv214== "mud" |
                         HH$hv214 == "no walls" |  
                         HH$hv214 == "stone with mud" | 
                         HH$hv214 == "wood planks / shingles" |
                         HH$hv214 == "gi/  metal / asbestos sheets", 1, 0)
table(HH$wall.slum)
prop.table(table(HH$wall.slum))

# NUMBER OF HOUSEHOLD MEMBERS
class(HH$hv009)
HH$hh.members <- HH$hv009
table(HH$hh.members)
#make into categorical var 

# NUMBER OF PEOPLE SLEEPING IN HOUSEHOLD
table(HH$hv216)
HH$hh.ppl.bedroom.rat <-  HH$hh.members/HH$hv216
summary(HH$hh.ppl.bedroom.rat)

# BINARY FACTOR FOR NUMBER OF PEOPLE SLEEPING IN HOUSEHOLD
HH$bedroom.slum <- ifelse(HH$hh.ppl.bedroom.rat > 3 | HH$hv216==0,1,0)
table(HH$bedroom.slum)
prop.table(table(HH$bedroom.slum))
sum(is.na(HH$bedroom.slum))

# BINARY FACTOR FOR TOILET TYPE: LATRINE
table(HH$hv205)
HH$latrine <- ifelse(HH$hv205 %in% c("pit latrine with slab", 
                                     "pit latrine without slab/open pit", 
                                     "ventilated improved pit latrine (vip)", 
                                     "composting toilet", 
                                     "dry toilet", 
                                     "hanging toilet/latrine", "other",
                                     "no facility/bush/field"), 1,0)
table(HH$latrine)
prop.table(table(HH$latrine))

#water slum
# HH$hh.pipe.water.slum <- 1 - HH$hh.pipe.water
# 
# # URBAN SLUM - UN DEFINITION
# HH<-HH %>%
#   rowwise() %>%
#   mutate(slum.sum = sum(floor.slum, hh.pipe.water.slum, latrine, bedroom.slum, na.rm=T))
# table(HH$slum.sum)
# prop.table(table(HH$slum.sum))
# 
# table(HH$slum.sum, HH$hv025)
# # URBAN SLUM 1 STATUS
# HH<-HH %>% mutate(slum1 = case_when(
#   hv025=="rural" ~ "rural",
#   (hv025 == "urban" & slum.sum >= 2) ~ "urban slum",
#   (hv025 == "urban" & slum.sum < 2) ~ "urban non-slum"))
# 
# 
# # URBAN SLUM 2 STATUS (ZULU ET AL 2002 DEFINITION)
# HH$slum2 <- ifelse(HH$hh.urban == 1 & HH$hh.electricity == 0 & HH$latrine == 1 & HH$piped == 1, 1, 0)
# 






############################################################# EXTRACT PREDICTORS 

colnames(HH)
colnames(IR)

HH$v001 <- HH$hv001
HH$v002 <- HH$hv002

IR.HH <- base::merge(IR, HH, by=c("v001", "v002"), all.x=T)

predictors <- read_excel("UP_DHS_2020_Data_Dictionary.xlsx", sheet="predictors")
predictors <- predictors %>% 
  select(varnames) %>%
  filter(!is.na(varnames))
vulnerability_vars <- names(IR.HH)[(names(IR.HH) %in% predictors$varnames)]

vul <- IR.HH %>% dplyr::select(caseid, sdweight, all_of(vulnerability_vars))
colSums(is.na(vul))

saveRDS(vul, file = "India_constructed_data/2_UP_DHS_vulpredictors.RData")


