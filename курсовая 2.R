library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")


tbl = read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1,]
tbl
tbl=tbl[tbl$DOY > 244 & tbl$DOY < 346,]
tbl
tbl=tbl[tbl$daytime == FALSE,]

glimpse(tbl)
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric))
cor_td
cor_td = cor(drop_na(tbl_numeric))%>% as.data.frame %>% select(h2o_flux)
vars=row.names(cor_td)[cor_td$h2o_flux^2 > .2]%>% na.exclude

formula1 = h2o_flux ~ LE + rand_err_LE + rand_err_h2o_flux + 
  co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
  RH + VPD + T. + un_LE + un_h2o_flux + w.h2o_cov + co2+ co2.1

model1 = lm(formula1, data = tbl)
coef(model1)
resid(model1)
confint(model1)

summary(model1)
anova(model1)

formula2 = h2o_flux ~ LE + rand_err_LE + rand_err_h2o_flux + 
  co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
  RH + VPD + T. + un_LE + un_h2o_flux + w.h2o_cov + co2+ co2.1-T.-co2-co2.1
model1 = lm(formula2, data = tbl)

coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)

formula3=lm(h2o_flux ~ (LE + rand_err_LE + rand_err_h2o_flux + 
                        co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                        RH + VPD + T. + un_LE + un_h2o_flux + w.h2o_cov + co2+ co2.1-T.-co2-co2.1)^2,data=tbl)
coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)

plot(model1)

