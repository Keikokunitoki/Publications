library(dplyr)
library(ggplot2)
library(geepack)
library(survminer)
library(survival)

cov<- read.csv("*path_to_data*.csv")
cov$BMI<-as.numeric(cov$BMI)


MGH<-cov%>%filter(MGH=="1")
nMGH<-cov%>%filter(MGH=="0")



## model with age, sex, BMI, rem*dex, all_steroids, MGH

### 1. MGH
# a. t_onset_death
modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 


# b. t_hospital_death
modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 


# c. t_hospital
modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 


# d. t_ICU
modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 


# e. t_oxy_death
modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 


# f. t_vent
modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=MGH) 
summary(modelLC.o) 



### 2. International (no MGH)
# a. t_onset_death
modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 


# b. t_hospital_death
modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 


# c. t_hospital
modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 


# d. t_ICU
modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 


# e. t_oxy_death
modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 


# f. t_vent
modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=nMGH) 
summary(modelLC.o) 




### 3. ALL
# a. t_onset_death
modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 


# b. t_hospital_death
modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 


# c. t_hospital
modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 


# d. t_ICU
modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 


# e. t_oxy_death
modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 


# f. t_vent
modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Epithelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Endothelial+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Alveolar+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Macro+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Lymph+Age+Sex+BMI+Dex*Rem+All_Steroid, data=cov) 
summary(modelLC.o) 





#################
### unadjusted ##
#################

### 1. MGH
# a. t_onset_death
modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Epithelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Endothelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Alveolar, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Macro, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Lymph, data=MGH) 
summary(modelLC.o) 


# b. t_hospital_death
modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Epithelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Endothelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Alveolar, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Macro, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Lymph, data=MGH) 
summary(modelLC.o) 


# c. t_hospital
modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Epithelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Endothelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Alveolar, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Macro, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Lymph, data=MGH) 
summary(modelLC.o) 


# d. t_ICU
modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Epithelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Endothelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Alveolar, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Macro, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Lymph, data=MGH) 
summary(modelLC.o) 


# e. t_oxy_death
modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Epithelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Endothelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Alveolar, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Macro, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Lymph, data=MGH) 
summary(modelLC.o) 


# f. t_vent
modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Epithelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Endothelial, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Alveolar, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Macro, data=MGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Lymph, data=MGH) 
summary(modelLC.o) 



### 2. International (no MGH)
# a. t_onset_death
modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Epithelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Endothelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Alveolar, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Macro, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Lymph, data=nMGH) 
summary(modelLC.o) 


# b. t_hospital_death
modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Epithelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Endothelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Alveolar, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Macro, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Lymph, data=nMGH) 
summary(modelLC.o) 


# c. t_hospital
modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Epithelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Endothelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Alveolar, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Macro, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Lymph, data=nMGH) 
summary(modelLC.o) 


# d. t_ICU
modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Epithelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Endothelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Alveolar, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Macro, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Lymph, data=nMGH) 
summary(modelLC.o) 


# e. t_oxy_death
modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Epithelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Endothelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Alveolar, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Macro, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Lymph, data=nMGH) 
summary(modelLC.o) 


# f. t_vent
modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Epithelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Endothelial, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Alveolar, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Macro, data=nMGH) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Lymph, data=nMGH) 
summary(modelLC.o) 




### 3. ALL
# a. t_onset_death
modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Epithelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Endothelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Alveolar, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Macro, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_onset_death, death) ~ Lymph, data=cov) 
summary(modelLC.o) 


# b. t_hospital_death
modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Epithelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Endothelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Alveolar, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Macro, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital_death, death) ~ Lymph, data=cov) 
summary(modelLC.o) 


# c. t_hospital
modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Epithelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Endothelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Alveolar, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Macro, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_hospital, death) ~ Lymph, data=cov) 
summary(modelLC.o) 


# d. t_ICU
modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Epithelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Endothelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Alveolar, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Macro, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_ICU, death) ~ Lymph, data=cov) 
summary(modelLC.o) 


# e. t_oxy_death
modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Epithelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Endothelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Alveolar, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Macro, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_oxy_death, death) ~ Lymph, data=cov) 
summary(modelLC.o) 


# f. t_vent
modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Epithelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Endothelial, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Alveolar, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Macro, data=cov) 
summary(modelLC.o) 

modelLC.o <-coxph(formula = Surv(t_vent, death) ~ Lymph, data=cov) 
summary(modelLC.o) 
