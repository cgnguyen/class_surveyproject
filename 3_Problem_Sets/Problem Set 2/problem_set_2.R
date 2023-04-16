# Setup -------------------------------------------------------------------
 ## Pakete  ----------------------------------------------------------------
  library(tidyverse) #Tidyverse Framework
  library(stargazer) #Schöne Regressionstabellen und Export
  library(arm) #standardize Befehl 
 
  ##Daten einlesen ---------------------------------------------------------
  D<-read_rds("./data/ess.rds")


#Frage 1: Bivariate Regression ----------------------------------------------------
  mod_1<-lm(trstplt~gender, data=D)
  summary(mod_1)
  
  D$gender_male<-relevel(D$gender, ref="Mann")
  
  mod_1<-lm(trstplt~gender_male, data=D)
  summary(mod_1)
  
  
# Frage 2: Bivariate Regression 2--------------------------------------------------

  mod_2<-lm(trstplt~stfdem, data=D)

  summary(mod_2)
  
# Frage 3:Soziodemographische Kontrollen -------------------
  mod_3<-lm(trstplt~stfdem+
              age+education_simple+activity_simple+gender, data=D)
  
  summary(mod_3)
  
#Frage 4: Zufriedenheitsvariablen 
   mod_4<-lm(trstplt~lr+stfeco+stfgov+stflife+stfdem+
               age+education_simple+activity_simple+gender, data=D)
  

# Ergebnisse Exportieren --------------------------------------------------
  stargazer(mod_1,
            star.cutoffs = c(0.05, 0.01, 0.001),
            style="apsr",
            type="text",
            covariate.labels = c("Männlich",
                                  "Intercept"))
   
  stargazer(mod_2,
            star.cutoffs = c(0.05, 0.01, 0.001),
            style="apsr",
            type="text",
            covariate.labels = c("Männlich",
                                  "Intercept"))