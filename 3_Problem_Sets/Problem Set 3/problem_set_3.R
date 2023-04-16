# Setup -------------------------------------------------------------------

## Pakete  ----------------------------------------------------------------
  library(tidyverse) #Tidyverse Framework
  library(stargazer) #Schöne Regressionstabellen und Export
  library(interactions) #Hilfsfunktionen für Interatktionseffekte
  library(broom.mixed)
  library(lme4) #Mehrbenenmodelle 
  library(sjPlot)
  library(sjmisc)
library(jtools) #Hilfsfunktionen für GLMs und andere Dinge


# Optional - Daten Vorbereiten --------------------------------------------


##Individualdaten einlesen ---------------------------------------------------------
  D<-read_rds("./data/ess_neu.rds")
  
  
##Nationale Daten hinzufügen---------------------------------------------------------
  ##Einzelne Variablen einlesen --------------------------------------------
    #GDP capita
    GDP<-read.csv("./data/gdp.csv") 
    names(GDP)<-c("country_name","GDP") 
    
    GDP<-GDP%>%
      filter(!is.na(GDP))

    #inequality/gini
    INEQUALITY<-read.csv("./data/8020.csv") 
    names(INEQUALITY)<-c("country_name","INEQ") 
    
    INEQUALITY<-INEQUALITY%>%
      filter(!is.na(INEQ))
 
    #unemployment benefit generosity / social spending
    SOCIAL<-read.csv("./data/socialprot.csv") 
    names(SOCIAL)<-c("country_name","SOCIAL") 
    
    SOCIAL<-SOCIAL%>%
      filter(!is.na(SOCIAL))
    
    ##Nationalen Datensatz hinzufügen----------------------------------
    COUNTRY<-
      GDP %>%
        #Zusammenfügen
        full_join(INEQUALITY, by="country_name")%>%
        full_join(SOCIAL,by="country_name")%>%
        filter(!is.na(country_name))%>%
        #Länder umbennen 
        mutate(country_name =fct_recode(country_name,
        "Germany" = "Germany (until 1990 former territory of the FRG)",
        "UK" = "United Kingdom"))%>%
      #Daten alle aus 2008 = Runde 4 des ESS (Achtung: Eigentlich zählt der Erhebungszeitraum)
      mutate(essround=4)

    ##Namen der Länder mit ESS Harmoniseren -------------------------------------
    COUNTRYCODE<-read_csv("./data/countrycodes.csv")
    COUNTRYCODE
    
    COUNTRY<-
      COUNTRY %>%
        full_join(COUNTRYCODE, by="country_name")
    
    
    ##Länder Daten mit Individualdaten zusammenfügen 
    D <-
      D %>%
      left_join(COUNTRY,by=c("cntry","essround")) 
    
    ##Nur Welle 4
    D<-
      D%>%
        filter(essround==4)%>%
        filter(!cntry %in% c("HR","IL","RU","UA"))%>%
        filter(eduyrs<30)
  
    

##Individualdaten Säubern --------------------------------------------------
  D$vote_bin<-as.factor(D$vote)
    summary(D$vote_bin) 
    
    #Recoding D$vote_bin 
    D$vote_bin <- fct_recode(D$vote_bin,
        "Ja" = "1",
        "Nein" = "2",
        NULL = "3",
        NULL = "7",
        NULL = "8",
        NULL = "9")
    #Baseline sollte "nicht wählen" sein
    D$vote_bin<-relevel(D$vote_bin, ref = "Nein")
    
    #Daten Speichern
    saveRDS(D, file = "./DATA/problem_set_3.rds")

# Frage 1: Bivariate Zusammenhang -----------------------------------------------------
  mod_1<-glm(vote_bin~eduyrs, data=D,
             family = binomial(link = "logit"))
    
  summary(mod_1)

  #Ergebnisse
  tidy(mod_1, exponentiate = T)%>%
    mutate_if(is.numeric, round, 5)
  
  #Visualsierung
  effect_plot(mod_1, pred = eduyrs, interval = TRUE)+
    ylim(0,1)
  

#Frage 2: Multivariater Zusammenhang -------------------------------------
  mod_2<-glm(vote_bin~eduyrs+
               trust_index_1, data=D,
              family = binomial(link = "logit"))

  #Ergebnisse
  tidy(mod_2, exponentiate = T)
  

  
  #Visualsierung
  pred_in<-make_new_data(mod_2, pred = "eduyrs",data=D,
                at =list(trust_index_1=c(0,5,10)))
  
  
  
  
  pred_out<-predict(mod_2,newdata=pred_in, type="response",se.fit=T)
  
  pred_final<-cbind(pred_in,pred_out)
  
  pred_final%>%
    ggplot()+
      aes(x=eduyrs, y=fit,color=as.factor(trust_index_1))+
      geom_line()+
      theme_bw()+
      xlab("Bildung")+ylab("Vertrauen/ Trust Index")+
      #Konfidenz-Interval
      geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit), alpha=0.2) 


#Frage 3: Multivariater Zusammenhang -------------------------------------
  mod_3<-glm(vote_bin~eduyrs+
               trust_index_1+
               gender+age+activity_simple+income_feel+
               cntry, data=D,
             family = binomial(link = "logit"))

  tidy(mod_3, exponentiate = T)%>%
    print(n=50)
  
  pred_in<-make_new_data(mod_3, pred="eduyrs", data=D, 
                         at=list(cntry=c("DE","DK","GR")))
  
  #Predicted 
  pred_out<-predict(mod_3,newdata=pred_in, type="response",se.fit=T)
  
  pred_final<-cbind(pred_in,pred_out)

  pred_final%>%
    ggplot()+
      aes(x=eduyrs, y=fit,color=cntry)+
      geom_line()+
      facet_wrap(.~cntry)+
      theme_bw()+
      xlab("Bildung")+ylab("Wahlwahrscheinlichkeit")+
      #Konfidenz-Interval
      geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit), alpha=0.2)+
      ylim(0,1)
  
  

# Frage 4 Interaktionseffekte--------------------------------------------------
  mod_4<-lm(trust_index_1~
              eduyrs*gender+age+activity_simple+income_feel+
              cntry,
            data=D)
  
  summary(mod_4)
 
  interact_plot(mod_4, pred = eduyrs, modx = gender,
               interval = T)
  
  #Koeffizienten /margins
  plot(sim_margins(mod_4, pred= eduyrs, modx = gender))
  
  

#Frage 5:Mehrebenenmodelle----------------------------------------------------
   mod_5<-lmer(trust_index_1~
                eduyrs+gender+age+activity_simple+income_feel+
                GDP+INEQ+SOCIAL+
                 (1|cntry),
                data=D)

  summary(mod_5)
  screenreg(mod_5)
 
  
#Frage 6:Mehrebenenmodelle Interaktion-------------------------------------------
   mod_6<-lmer(trust_index_1~
                eduyrs+gender+age+activity_simple+income_feel+
                GDP+INEQ+SOCIAL+
                (1|cntry),
                data=D)

  summary(mod_6)
  screenreg(mod_6)
  
  interact_plot(mod_6, pred = eduyrs, modx = GDP,
               interval = T)
  
  #Koeffizienten /margins
  plot(sim_margins(mod_6, pred= eduyrs, modx = GDP))


  


  

  
  
  
  
  

  