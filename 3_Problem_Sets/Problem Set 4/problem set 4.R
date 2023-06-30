#Setup---------------------
  library(tidyverse)
  library(plm)


#0.Data cleaning--------------------------------------
 D <- read_dta("6_Data/AUTNES/10017_da_en_v2_0.dta")
  
 #Emotions about politics-------------------------------------
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
   
      #Economic Conditions higher values worse 
        mutate(
    econ_perc_1 = car::recode(w1_q48, "88=NA; 99=NA"),
    econ_perc_2 = car::recode(w2_q31, "88=NA; 99=NA"),
    econ_perc_3 = car::recode(w3_q31, "88=NA; 99=NA"),
    econ_perc_4 = car::recode(w4_q36, "88=NA; 99=NA"))%>%
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
        starts_with("econ_"), starts_with("fpo"),
        starts_with("emo"),
        age,gender,edu,activity,income)
  
  
  D_long<-
    long_panel(D_select, 
               prefix = "_",
               begin =1, 
               end=4)%>%
    panel_data(., id=id, wave=wave)
    
  write_rds(D_long, file="problem_set_4.rds")
  
#1. Visualize worry about the economy over time -----------------------------
  fig_1<-
    D_long %>% 
      select(wave,starts_with("econ_"))%>%
      ungroup()%>%
      group_by(wave)%>%
      summarize(econ_perc=mean(econ_perc, na.rm=T))%>%
      ggplot()+
      aes(x=wave,y=econ_perc)+
      geom_line()+
      theme_bw()+
      ylim(2,3.5)
  
  
  
#2.Pooled Model: FPO and worry about the economy + Kontrolle ---------------------
  mod_1<-plm(fpo~econ_perc+age+gender+activity+edu+income, 
             data=D_long,
             model="pooling")
  
  summary(mod_1)
  
  
#3. Fixed Effect Model - within 
  mod_2<-plm(fpo~econ_perc,
             data=D_long, 
             model="within", effect="individual")
  
  
#4. Random Effect Model -within 
  mod_3<-plm(fpo~econ_perc,
             data=D_long, 
             model="random", effect="individual")
  
  
  
  summary(mod_3)

  #Random or Fixed effect Model? 
  phtest(mod_2, mod_3)
  
 
#5. Full model -------- other variables---------------------------
  mod_4<-plm(fpo~econ_perc+
               emo_annoyed+emo_worried+emo_anxious,
               data=D_long,
               model="within",effect="individual")

  summary(mod_4)
 