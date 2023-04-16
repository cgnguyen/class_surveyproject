# Setup -------------------------------------------------------------------
## Pakete  ----------------------------------------------------------------
  library(tidyverse)
  library(plotrix)
##Daten Einlesen ---------------------------------------------------------
  #über direkte Ansteuerung 
  ESS<-readRDS(file="./Data/ess.rds")

  # ESS<-readRDS(file="D:/OneDrive/Teaching/Kurse/Survey_r/Data/ess.rds")

# Deskriptive Daten -------------------------------------------------------
##  1 -----------------------------------------------------------------
  
  ESS%>%
    select(trstplt,lr)%>%
    summary(.)
  

##  2:Gruppiert nach Land--------------------------------------------------------------
  ESS%>%
    group_by(country)%>%
    summarize(trstplt_mittel=median(trstplt,na.rm=T),
             trstplt_durch=mean(trstplt,na.rm=T),
             lr_mittel=median(lr,na.rm=T),
             lr_durch=mean(lr,na.rm=T))
    
##  3:Gruppiert nach Zeit-----------------------------------------------------------
  ESS%>%
    group_by(essround)%>%
    summarize(trstplt_mittel=median(trstplt,na.rm=T),
             trstplt_durch=mean(trstplt,na.rm=T),
             lr_mittel=median(lr,na.rm=T),
             lr_durch=mean(lr,na.rm=T))
  
  
##  4: Zweifache Gruppierung + Pivot-----------------------------------------------------------
  ESS%>%
    group_by(essround,country)%>%
    summarize(trstplt_mittel=median(trstplt,na.rm=T),
             trstplt_durch=mean(trstplt,na.rm=T),
             lr_mittel=median(lr,na.rm=T),
             lr_durch=mean(lr,na.rm=T))%>%
    pivot_wider(names_from =essround, values_from = starts_with(c("trstplt","lr")))%>%
    print(n=30)

  #Schwer zu lesen- vereinfacht für eine Variable  
  ESS%>%
      group_by(essround,country)%>%
      summarize(trstplt_mittel=median(trstplt,na.rm=T))%>%
      pivot_wider(names_from =essround, values_from = starts_with(c("trstplt","lr")))%>%
      print(n=30)
  
  
  
##  5: Hauptfrequenz der Aktivität--------------------------------------------
  ESS %>%
    group_by(activity_simple) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))
  
  

## Bonus: Frequenz in bestimten Ländern--------------------------------------------------
  #Das funktioniert nicht - warum? Es sollten 49 % für Bezahlte Arbeit in Deutschland sein 
  ESS %>%
    filter(country %in% c("GB","DE","FR"))%>%
    group_by(activity_simple,country) %>%
    filter(!is.na(activity_simple))%>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))

  
  
  
  #N ist nicht ganz richtig gewesen
  ESS %>%
    filter(country %in% c("GB","DE","FR"))%>%
    group_by(activity_simple,country) %>%
    summarize(n=n())%>%
    group_by(country)%>%
    mutate(total =  sum(n),
           freq = n/total)
    
  

# Abbildungen -------------------------------------------------------------

##6: Balkendiagramm:Vertrauen und Aktivität---------------------------------------
    fig_se<-
      ESS%>%
        group_by(activity_simple)%>%
        summarize(trust_mean=mean(trstplt,na.rm=T),
                  se= std.error(trstplt,na.rm=T))%>%
        filter(!is.na(activity_simple))%>%
        filter(activity_simple!="Andere Tätigkeiten")%>%
        ggplot()+
          aes(x=activity_simple, y=trust_mean, 
              ymin=trust_mean-1.96*se,ymax=trust_mean+1.96*se)+
          geom_col(color="black",fill="gray")+
          geom_errorbar(width=0.3, color="red")+
          theme_minimal()+
          xlab("Aktivität")+
          ylab("Durschnittliches Politiker:innenvertrauen")+
          ggtitle("Durschnittliches Politiker:innenvertrauen in Europa")
  
    fig_se
    
    
##7: Balkendiagramm:Vertrauen und Aktivität in 5 Ländern---------------------------------------
    fig_country<-
      ESS%>%
        filter(country %in% c("GB","DE","FR","DK","GR"))%>%
        group_by(activity_simple,country)%>%
        summarize(trust_mean=mean(trstplt,na.rm=T),
                  se= std.error(trstplt,na.rm=T))%>%
        filter(!is.na(activity_simple))%>%
        filter(activity_simple!="Andere Tätigkeiten")%>%
        ggplot()+
          aes(x=activity_simple, y=trust_mean, 
              ymin=trust_mean-1.96*se,ymax=trust_mean+1.96*se)+
          geom_col(color="black",fill="gray")+
          geom_errorbar(width=0.3, color="red")+
          facet_wrap(~country)+
          theme_minimal()+
          xlab("Aktivität")+
          ylab("Durschnittliches Politiker:innenvertrauen")+
          ggtitle("Durschnittliches Politiker:innenvertrauen in Europa")
  
    fig_country
    
    


##8- Politikvertrauen über die Zeit ------------------------------------------
  ESS%>%
        group_by(essround)%>%
        summarize(trust_mean=mean(trstplt,na.rm=T))%>%
        ggplot()+
          aes(x=essround, y=trust_mean)+
          geom_line()+
          xlab("Jahr")+
          ylab("Durschnittliches Politiker:innenvertrauen")+
          ggtitle("Durschnittliches Politiker:innenvertrauen in Europa Über die Zeit")+
          theme_bw()+
          scale_x_continuous(breaks=seq(1:9),
                             labels=seq(from=2002, to=2018, by=2))
          
    
##9- Politikvertrauen über die Zeit -Ländervergleich----------------------------
  ESS%>%
    filter(country %in% c("GB","DE","DK","IT","GR"))%>%        
      group_by(essround,country)%>%
        summarize(trust_mean=mean(trstplt,na.rm=T))%>%
        ggplot()+
          aes(x=essround, y=trust_mean, color=country)+
          geom_line()+
          facet_wrap(~country)+
          geom_vline(aes(xintercept=4), linetype="dashed")+
          xlab("Jahr")+
          ylab("Durschnittliches Politiker:innenvertrauen")+
          ggtitle("Durschnittliches Politiker:innenvertrauen in Europa Über die Zeit")+
          theme_bw()+
          
          scale_x_continuous(breaks=seq(1:9),
                             labels=seq(from=2002, to=2018, by=2))


    
##10 Zusammenhang zwischen Vertrauen und Lebenszufriedenheit ------------------
   ESS%>%
    sample_n(10000)%>%
    ggplot()+
      aes(x=ppltrst, y=pplfair)+
      theme_bw()+
      geom_point(position=position_jitter(), alpha=0.4)+
      xlab("Lebenszufriedenheit")+
      ylab("Allgemeines Vertrauen")+
      theme_bw()+
      geom_smooth(method="lm")
    
    
    
    
#Killian - super Beispiel für Daten Reinigen 
    #Lrscale - nicht bereinigt 
    
    ESS%>%
      select(lrscale,lr)%>%
      summary()
    
    
    ESS%>%
        group_by(lrscale)%>%
        summarize(trustplt_mean=mean(trstplt,na.rm=T))%>%
         ggplot()+
     aes(x=lrscale, y=trustplt_mean)+
     geom_point ()+
     xlab("Links-Rechts-Skala")+
     ylab("Durchschnittliches Vertrauen in Politiker*innen")+
     ggtitle("Durchschnittliches Politiker*innenvertrauen im Links-Rechts-Vergleich")
             
             
  #LR Bereinigt 
  ESS%>%
        group_by(lr)%>%
        summarize(trustplt_mean=mean(trstplt,na.rm=T))%>%
         ggplot()+
     aes(x=lr, y=trustplt_mean)+
     geom_point ()+
     xlab("Links-Rechts-Skala")+
     ylab("Durchschnittliches Vertrauen in Politiker*innen")+
     ggtitle("Durchschnittliches Politiker*innenvertrauen im Links-Rechts-Vergleich")
  