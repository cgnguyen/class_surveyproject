#Setup-------------
  library(tidyverse)
  library(haven)
  library(plm)  #Panel Modelle
  library(panelr) #Hilfsfunktionen für Panel Modelle
  library(naniar) #Missing Value Visualization
  library(broom) #Daten Säubern

#Read and Clean Data----------------------------------
  ##Read in data ---------------------------
  ##Kann auch woanders abgelegt sein bei euch 
  D <- read_dta("6_Data/AUTNES/10017_da_en_v2_0.dta")
  
 
  
  #Datensatz im "Wide" format  
  head(D)
  
  
  ##Daten säubern und spezifische Variablen auswählen ---------------
  #Fokus auf spezifische Variablen - Referenz im Codebuch  
  

  D<-
    D%>%
      #Emotions about politics
        mutate(
        emo_annoyed_1 = w1_q36x3,
         emo_annoyed_2 = w2_q57x3,
         emo_annoyed_3 = w3_q52x3,
         emo_annoyed_4 = w4_q51x3,
         emo_anxious_1 = w1_q36x4,
         emo_anxious_2 = w2_q57x4,
         emo_anxious_3 = w3_q52x4,
         emo_anxious_4 = w4_q51x4,
         emo_worried_1 = w1_q36x2,
         emo_worried_2 = w2_q57x2,
         emo_worried_3 = w3_q52x2,
         emo_worried_4 = w4_q51x2,
         emo_hopeful_1 = w1_q36x5,
         emo_hopeful_2 = w2_q57x5,
         emo_hopeful_3 = w3_q52x5,
         emo_hopeful_4 = w4_q51x5)%>%
      #FPO PTV
        mutate(
          fpo_1 = car::recode(w1_q12x3, "99=NA; 77=NA"),
          fpo_2 = car::recode(w2_q5x3, "99=NA; 77=NA"),
          fpo_3 = car::recode(w3_q8x3, "99=NA; 77=NA"),
          fpo_4 = car::recode(w4_q10x3, "99=NA; 77=NA"))%>%
      #Clean Demographic variables 
          mutate(
            age=as_factor(sd2x2),
            gender=as_factor(sd3),
            edu=as_factor(sd7),
            activity=as_factor(sd10),
            income=as_factor(sd22))
  
  
  

  
  D_select<-
    D%>%
      select(id,
        starts_with("emo"), starts_with("fpo"),
        age,gender,edu,activity,income)
  
  
  ##Wide to long format-----------
  
    D_long<-
    long_panel(D_select, 
               prefix = "_",
               begin =1, 
               end=4)
    
    view(D_long)
    
  ##Long to wide format ---------
  D_long <- panel_data(D_long, id = id, wave = wave)
  
  D_wide<- widen_panel(D_long)

  head(D_wide)
      

#Panel Attrition - Missing Data------------------------
   D_long%>%
    as.data.frame(.)%>%
    select(starts_with("fpo"),wave)%>%
    gg_miss_var(.,facet = wave)
  
  
    #das ist ein grosses Problem das wir nun ignorieren  - nur Daten für alle Wellen

    D_long<-complete_data(D_long, min.waves = "all")
    
#Visualsierungen  --------------------
     ##Veränderungen über die Zeit - Vertrauen ------------
  
  #Eine Variable
  D_long %>% 
    select(wave,starts_with("fpo"))%>%
    ungroup()%>%
    group_by(wave)%>%
    summarize(fpo=mean(fpo))%>%
    ggplot()+
    aes(x=wave,y=fpo)+
    geom_line()+
    theme_bw()+
    ylab("FPO PTV")+
    xlab("Welle")+
    ylim(0,5)
  

  ##Mehrere Variablen - mit "reshaping"----------------
  D_long %>%
    ungroup() %>%
    group_by(wave) %>%
    summarize(across(starts_with("emo"), ~ mean(.), .names = "mean_{.col}"))%>%
    pivot_longer(cols=-wave)%>%
    ggplot()+
      aes(x=wave,y=value, color=name)+
      geom_line()+
      theme_bw()+
      xlab("Welle")

  
    
#Einfache Regressionsmodelle ------------------------
  #Nur eine Welle - wie Cross Sectional Model
  mod_1<-lm(fpo_1~emo_annoyed_1, data=D_wide)
  
  summary(mod_1)
    
    
  ##Pooled Model--------------------------
  mod_2<-lm(fpo ~ emo_annoyed, data=D_long)
  
  summary(mod_2)
  
  mod_3<-lm(fpo ~ emo_annoyed+age+gender+edu+activity+income,
              data=D_long)

  summary(mod_3)    


#Panel Modelle ------------------------------------
  ##Within Model
  #Fixed Effect 
  #"within" - kann sehr lange dauern + Effekte sind selten
  #Hier "within" Individuen 
  
  mod_4<-lm(fpo~emo_annoyed+as.factor(id), data=D_long)
  summary(mod_4)
  
  
  #Within Model - PLM Packet - wesentlich schneller
  
  D_plm<-pdata.frame(D_long, index=c("id","wave"),
                     drop.index=F, row.names =T)

  mod_5<-plm(fpo~emo_annoyed, data=D_plm,
             model="within", effect="individual")

  summary(mod_5)  
  
  #Vergleich 
   tidy(mod_4)%>%
    filter(term=="emo_annoyed")
  
  tidy(mod_5)%>%
    filter(term=="emo_annoyed")
    
      
