####Setup####
  library(Rmisc)
  library(tidyverse)
  library(plotrix)
  #Based on setup_wave_1.R#




####Allgemeine Evaluation####
  ####*Evaluation Allgemein####
   fig_eval_gen<-
    DATA%>%
      filter(white_resp=="Yes")%>%
      select(eval_white,eval_asian,eval_black,eval_muslim)%>%
      pivot_longer(cols = everything())%>%
      summarySE(., measurevar = "value",groupvar="name")%>%
      ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se)+
        geom_col()+
        geom_errorbar(width=0.2)+
        theme_bw()+
        scale_y_continuous(limits = c(0,9), expand = c(0, 0))+
        ylab("Evaluation")+xlab("Bevölkerungsgruppe")

 ggsave(fig_eval_gen, file="./Figures/fig_eval_gen.png",
        width=6, height=6, unit="in")
        

  ####*Evaluation nach Geschlecht####
    DATA%>%
      filter(white_resp=="Yes")%>%
      select(eval_white,eval_asian,eval_black,eval_muslim,gender_resp)%>%
      pivot_longer(cols = starts_with("eval"))%>%
      summarySE(., measurevar = "value",groupvar=c("name","gender_resp"))%>%
      filter(gender_resp!="divers")%>%
      ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
            group=gender_resp, color=gender_resp, fill=gender_resp)+
        geom_col(position="dodge")+
        geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
        theme_bw()+
        scale_y_continuous(limits = c(0,9), expand = c(0, 0))+
        ylab("Evaluation")+xlab("Bevölkerungsgruppe")
        

####Social Distance Measures####
  ####*Clean data to make more binary####
  DATA<-
    DATA%>%
      mutate(asian_nachbar=case_when(soc_dist_asian_1 <0 ~ 1,
                                    soc_dist_asian_1 >=0 ~0),
             asian_colleague=case_when(soc_dist_asian_2 <0 ~ 1,
                                    soc_dist_asian_2 >=0 ~0),
             asian_family=case_when(soc_dist_asian_3 <0 ~ 1,
                                    soc_dist_asian_3 >=0 ~0),
             white_nachbar=case_when(soc_dist_white_1 <0 ~ 1,
                                    soc_dist_white_1 >=0 ~0),
             white_colleague=case_when(soc_dist_white_2 <0 ~ 1,
                                    soc_dist_white_2 >=0 ~0),
             white_family=case_when(soc_dist_white_3 <0 ~ 1,
                                    soc_dist_white_3 >=0 ~0),
             black_nachbar=case_when(soc_dist_black_1 <0 ~ 1,
                                    soc_dist_black_1 >=0 ~0),
             black_colleague=case_when(soc_dist_black_2 <0 ~ 1,
                                    soc_dist_black_2 >=0 ~0),
             black_family=case_when(soc_dist_black_3 <0 ~ 1,
                                    soc_dist_black_3 >=0 ~0),
             muslim_nachbar=case_when(soc_dist_muslim_1 <0 ~ 1,
                                    soc_dist_muslim_1 >=0 ~0),
             muslim_colleague=case_when(soc_dist_muslim_2 <0 ~ 1,
                                    soc_dist_muslim_2 >=0 ~0),
             muslim_family=case_when(soc_dist_muslim_3 <0 ~ 1,
                                    soc_dist_muslim_3 >=0 ~0))
             
    ####*Bar Charts for approval comparison - Neighbor-General####
    fig_dist_1_gen<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_nachbar,white_nachbar,black_nachbar,muslim_nachbar)%>%
        pivot_longer(cols = ends_with("nachbar"))%>%
        summarySE(., measurevar = "value",groupvar=c("name"), na.rm=T)%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              label=paste(100*round(value,3),"%"))+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.01))+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
 
    ggsave(fig_dist_1_gen, file="./Figures/fig_dist_1_gen.png",
        width=6, height=6, unit="in")
        
             
    ####*Bar Charts for approval comparison - Colleague-General####
    fig_dist_2_gen<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_colleague,white_colleague,black_colleague,muslim_colleague)%>%
        pivot_longer(cols = ends_with("colleague"))%>%
        summarySE(., measurevar = "value",groupvar=c("name"), na.rm=T)%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              label=paste(100*round(value,3),"%"))+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.01))+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
  
    fig_dist_2_gen

    ggsave(fig_dist_2_gen, file="./Figures/fig_dist_2_gen.png",
        width=6, height=6, unit="in")
    
    ####*Bar Charts for approval comparison - Colleague-General####
    fig_dist_3_gen<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_family,white_family,black_family,muslim_family)%>%
        pivot_longer(cols = ends_with("family"))%>%
        summarySE(., measurevar = "value",groupvar=c("name"), na.rm=T)%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              label=paste(100*round(value,3),"%"))+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.01))+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.4), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))

      fig_dist_3_gen
      
      ggsave(fig_dist_3_gen, file="./Figures/fig_dist_3_gen.png",
        width=6, height=6, unit="in")
      
      
####Evaluation of Asians and Asian Culture in Germany####
   DATA<-
    DATA%>%
      mutate(asian_trust=case_when(asian_trust <=0 ~ 0,
                                    asian_trust >0 ~1),
             asian_culture=case_when(asian_culture <=0 ~ 0,
                                    asian_culture >0 ~1),
             asian_children=case_when(asian_children <=0 ~ 0,
                                    asian_children >0 ~1))
             
 
  ####CHeck randomization- var####
      look_for(DATA,"random")
      (DATA$c_0065)

      DATA$ra_var_2<-as.numeric(DATA$c_0065)
      
      
    fig_asian_culture<-
      DATA%>%
        dplyr::filter(white_resp=="Yes")%>%
        dplyr::filter(ra_var_2==1)%>%
        select(asian_trust,asian_culture,asian_children)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name"), na.rm=T)%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              label=paste(100*round(value,3),"%"))+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.01))+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Kinder","Kultur","Freundschaft \nund Vertrauen"))
 
    ggsave(fig_asian_culture, file="./Figures/fig_asian_culture.png",
        width=6, height=6, unit="in")
           
    
    ####Blame for Corona####
    DATA<-
      DATA%>%
        mutate(asian_blame_1=case_when(asian_blame_1 <=0 ~ 0,
                                      asian_blame_1 >0 ~1),
               asian_blame_2=case_when(asian_blame_2 <=0 ~ 0,
                                      asian_blame_2 >0 ~1))
  
    fig_asian_blame<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_blame_1,asian_blame_2)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name"), na.rm=T)%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              label=paste(100*round(value,3),"%"))+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.01))+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Asiaten sind schuld","Asiaten sind an der \nschnellen Ausbreitung schuld"))
  
    ggsave(fig_asian_blame, file="./Figures/fig_asian_blame.png",
        width=6, height=6, unit="in")

    
    DATA%>%
      dplyr::filter(white_resp=="Yes")%>%
      dplyr::filter(ra_var_2==3)
    
####Check Subgroups-Corona Effect?####
    #Use being affected by corona, three groups - weak, medium, high#
    DATA<-
      DATA%>%
        mutate(corona_factor=factor(case_when(corona_affected <=3 ~ "low",
                                        corona_affected >3 & corona_affected <7 ~ 'medium',
                                        corona_affected >=7 ~'high')))%>%
        mutate(corona_factor=factor(corona_factor, levels=c("low","medium","high")))
    
    
    
    ####*General Evaluations####
    fig_eval_corona<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(eval_white,eval_asian,eval_black,eval_muslim,corona_factor)%>%
        pivot_longer(cols = starts_with("eval"))%>%
        summarySE(., measurevar = "value",groupvar= c("name","corona_factor"))%>%
        filter(!is.na(corona_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_factor,fill=corona_factor,
              label=paste(round(value,2)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.15), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,9), expand = c(0, 0))+
          ylab("Evaluation")+xlab("Bevölkerungsgruppe")

  ggsave(fig_eval_corona, file="./Figures/fig_eval_corona.png",
        width=10, height=6, unit="in")
        

  ####*Social Distance-Nachbar####
  fig_dist_1_corona<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_nachbar,white_nachbar,black_nachbar,muslim_nachbar,corona_factor)%>%
        pivot_longer(cols = ends_with("nachbar"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_factor"),na.rm=T)%>%
        filter(!is.na(corona_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_factor, color=corona_factor, fill=corona_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_1_corona
    ggsave(fig_dist_1_corona, file="./Figures/fig_dist_1_corona.png",
        width=10, height=6, unit="in")
                  
  
  ####*Social Distance-Kollege####
  fig_dist_2_corona<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_colleague,white_colleague,black_colleague,muslim_colleague,corona_factor)%>%
        pivot_longer(cols = ends_with("colleague"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_factor"),na.rm=T)%>%
        filter(!is.na(corona_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_factor, color=corona_factor, fill=corona_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_2_corona
    ggsave(fig_dist_2_corona, file="./Figures/fig_dist_2_corona.png",
        width=10, height=6, unit="in")
                  
                 
    
  ####*Social Distance-Familie####
  fig_dist_3_corona<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_family,white_family,black_family,muslim_family,corona_factor)%>%
        pivot_longer(cols = ends_with("family"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_factor"),na.rm=T)%>%
        filter(!is.na(corona_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_factor, color=corona_factor, fill=corona_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_3_corona
    ggsave(fig_dist_3_corona, file="./Figures/fig_dist_3_corona.png",
        width=10, height=6, unit="in")
                   
                   
    ####*Asian Culture####
    fig_asian_culture_corona<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_trust,asian_culture,asian_children,corona_factor)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_factor"), na.rm=T)%>%
        filter(!is.na(corona_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_factor, color=corona_factor, fill=corona_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Kinder","Kultur","Freundschaft \nund Vertrauen"))
 
      ggsave(fig_asian_culture_corona, file="./Figures/fig_asian_culture_corona.png",
        width=10, height=6, unit="in")
           
    ####*Asian Blame####
    fig_asian_blame_corona<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_blame_1,asian_blame_2,corona_factor)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_factor"), na.rm=T)%>%
        filter(!is.na(corona_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_factor, color=corona_factor, fill=corona_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Asiaten sind schuld","Asiaten sind an der \nschnellen Ausbreitung schuld"))
        
      ggsave(fig_asian_blame_corona, file="./Figures/fig_asian_blame_corona.png",
        width=10, height=6, unit="in")
####Check Subgroups-Risikgruppe?####
    #Use being affected by corona, three groups - weak, medium, high#
    DATA<-
      DATA%>%
        mutate(corona_risk_factor=factor(case_when(risk_group==1 ~ "No",
                                        risk_group==2 ~'Yes')))%>%
        mutate(corona_risk_factor=factor(corona_risk_factor, levels=c("No","Yes")))
    
    
    
    ####*General Evaluations####
    fig_eval_corona_risk<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(eval_white,eval_asian,eval_black,eval_muslim,corona_risk_factor)%>%
        pivot_longer(cols = starts_with("eval"))%>%
        summarySE(., measurevar = "value",groupvar= c("name","corona_risk_factor"))%>%
        filter(!is.na(corona_risk_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_risk_factor,fill=corona_risk_factor,
              label=paste(round(value,2)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.15), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,9), expand = c(0, 0))+
          ylab("Evaluation")+xlab("Bevölkerungsgruppe")
  fig_eval_corona_risk
  ggsave(fig_eval_corona_risk, file="./Figures/fig_eval_corona_risk.png",
        width=10, height=6, unit="in")
        

  ####*Social Distance-Nachbar####
  fig_dist_1_corona_risk<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_nachbar,white_nachbar,black_nachbar,muslim_nachbar,corona_risk_factor)%>%
        pivot_longer(cols = ends_with("nachbar"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_risk_factor"),na.rm=T)%>%
        filter(!is.na(corona_risk_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_risk_factor, color=corona_risk_factor, fill=corona_risk_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_1_corona_risk
    ggsave(fig_dist_1_corona_risk, file="./Figures/fig_dist_1_corona_risk.png",
        width=10, height=6, unit="in")
                  
  
  ####*Social Distance-Kollege####
  fig_dist_2_corona_risk<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_colleague,white_colleague,black_colleague,muslim_colleague,corona_risk_factor)%>%
        pivot_longer(cols = ends_with("colleague"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_risk_factor"),na.rm=T)%>%
        filter(!is.na(corona_risk_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_risk_factor, color=corona_risk_factor, fill=corona_risk_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_2_corona_risk
    ggsave(fig_dist_2_corona_risk, file="./Figures/fig_dist_2_corona_risk.png",
        width=10, height=6, unit="in")
                  
                 
    
  ####*Social Distance-Familie####
  fig_dist_3_corona_risk<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_family,white_family,black_family,muslim_family,corona_risk_factor)%>%
        pivot_longer(cols = ends_with("family"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_risk_factor"),na.rm=T)%>%
        filter(!is.na(corona_risk_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_risk_factor, color=corona_risk_factor, fill=corona_risk_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_3_corona_risk
    ggsave(fig_dist_3_corona_risk, file="./Figures/fig_dist_3_corona_risk.png",
        width=10, height=6, unit="in")
                   
                   
    ####*Asian Culture####
    fig_asian_culture_corona_risk<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_trust,asian_culture,asian_children,corona_risk_factor)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_risk_factor"), na.rm=T)%>%
        filter(!is.na(corona_risk_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_risk_factor, color=corona_risk_factor, fill=corona_risk_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Kinder","Kultur","Freundschaft \nund Vertrauen"))
  fig_asian_culture_corona_risk
      ggsave(fig_asian_culture_corona_risk, file="./Figures/fig_asian_culture_corona_risk.png",
        width=10, height=6, unit="in")
           
    ####*Asian Blame####
    fig_asian_blame_corona_risk<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_blame_1,asian_blame_2,corona_risk_factor)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","corona_risk_factor"), na.rm=T)%>%
        filter(!is.na(corona_risk_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=corona_risk_factor, color=corona_risk_factor, fill=corona_risk_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Asiaten sind schuld","Asiaten sind an der \nschnellen Ausbreitung schuld"))
      fig_asian_blame_corona_risk  
      ggsave(fig_asian_blame_corona_risk, file="./Figures/fig_asian_blame_corona_risk.png",
        width=10, height=6, unit="in")
      
      
      
      
####Political Effect?-Left right self placement?####
    #Use being affected by lr, three groups - weak, medium, high#
    DATA<-
      DATA%>%
        mutate(lr_factor=factor(case_when(lr <=4 ~ "left",
                                        lr >4 & lr <8 ~ 'middle',
                                        lr >=8 ~'right')))%>%
        mutate(lr_factor=factor(lr_factor, levels=c("left","middle","right")))
    
    
    
    ####*General Evaluations####
    fig_eval_lr<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(eval_white,eval_asian,eval_black,eval_muslim,lr_factor)%>%
        pivot_longer(cols = starts_with("eval"))%>%
        summarySE(., measurevar = "value",groupvar= c("name","lr_factor"))%>%
        filter(!is.na(lr_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=lr_factor,fill=lr_factor,
              label=paste(round(value,2)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.15), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,9), expand = c(0, 0))+
          ylab("Evaluation")+xlab("Bevölkerungsgruppe")
  fig_eval_lr
  ggsave(fig_eval_lr, file="./Figures/fig_eval_lr.png",
        width=10, height=6, unit="in")
        

  ####*Social Distance-Nachbar####
  fig_dist_1_lr<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_nachbar,white_nachbar,black_nachbar,muslim_nachbar,lr_factor)%>%
        pivot_longer(cols = ends_with("nachbar"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","lr_factor"),na.rm=T)%>%
        filter(!is.na(lr_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=lr_factor, color=lr_factor, fill=lr_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.6), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_1_lr
    ggsave(fig_dist_1_lr, file="./Figures/fig_dist_1_lr.png",
        width=10, height=6, unit="in")
                  
  
  ####*Social Distance-Kollege####
  fig_dist_2_lr<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_colleague,white_colleague,black_colleague,muslim_colleague,lr_factor)%>%
        pivot_longer(cols = ends_with("colleague"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","lr_factor"),na.rm=T)%>%
        filter(!is.na(lr_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=lr_factor, color=lr_factor, fill=lr_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_2_lr
    ggsave(fig_dist_2_lr, file="./Figures/fig_dist_2_lr.png",
        width=10, height=6, unit="in")
                  
                 
    
  ####*Social Distance-Familie####
  fig_dist_3_lr<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_family,white_family,black_family,muslim_family,lr_factor)%>%
        pivot_longer(cols = ends_with("family"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","lr_factor"),na.rm=T)%>%
        filter(!is.na(lr_factor))%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=lr_factor, color=lr_factor, fill=lr_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.7), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_3_lr
    ggsave(fig_dist_3_lr, file="./Figures/fig_dist_3_lr.png",
        width=10, height=6, unit="in")
                   
                   
    ####*Asian Culture####
    fig_asian_culture_lr<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_trust,asian_culture,asian_children,lr_factor)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","lr_factor"), na.rm=T)%>%
        filter(!is.na(lr_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=lr_factor, color=lr_factor, fill=lr_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.4), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Kinder","Kultur","Freundschaft \nund Vertrauen"))
    fig_asian_culture_lr
      ggsave(fig_asian_culture_lr, file="./Figures/fig_asian_culture_lr.png",
        width=10, height=6, unit="in")
           
    ####*Asian Blame####
    fig_asian_blame_lr<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_blame_1,asian_blame_2,lr_factor)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","lr_factor"), na.rm=T)%>%
        filter(!is.na(lr_factor))%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=lr_factor, color=lr_factor, fill=lr_factor,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Asiaten sind schuld","Asiaten sind an der \nschnellen Ausbreitung schuld"))
      fig_asian_blame_lr
      ggsave(fig_asian_blame_lr, file="./Figures/fig_asian_blame_lr.png",
        width=10, height=6, unit="in")
      
  
####Gender effects####
    ####*General Evaluations####
    fig_eval_gender_resp<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(eval_white,eval_asian,eval_black,eval_muslim,gender_resp)%>%
        pivot_longer(cols = starts_with("eval"))%>%
        summarySE(., measurevar = "value",groupvar= c("name","gender_resp"))%>%
        filter(!is.na(gender_resp))%>%
        filter(gender_resp!="divers")%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp,fill=gender_resp,
              label=paste(round(value,2)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.15), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,9), expand = c(0, 0))+
          ylab("Evaluation")+xlab("Bevölkerungsgruppe")
  fig_eval_gender_resp
  ggsave(fig_eval_gender_resp, file="./Figures/fig_eval_gender_resp.png",
        width=10, height=6, unit="in")
        

  ####*Social Distance-Nachbar####
  fig_dist_1_gender_resp<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_nachbar,white_nachbar,black_nachbar,muslim_nachbar,gender_resp)%>%
        pivot_longer(cols = ends_with("nachbar"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","gender_resp"),na.rm=T)%>%
        filter(!is.na(gender_resp))%>%
        filter(gender_resp!="divers")%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp, color=gender_resp, fill=gender_resp,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.6), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_1_gender_resp
    ggsave(fig_dist_1_gender_resp, file="./Figures/fig_dist_1_gender_resp.png",
        width=10, height=6, unit="in")
                  
  
  ####*Social Distance-Kollege####
  fig_dist_2_gender_resp<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_colleague,white_colleague,black_colleague,muslim_colleague,gender_resp)%>%
        pivot_longer(cols = ends_with("colleague"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","gender_resp"),na.rm=T)%>%
        filter(!is.na(gender_resp))%>%
        filter(gender_resp!="divers")%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp, color=gender_resp, fill=gender_resp,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_2_gender_resp
    ggsave(fig_dist_2_gender_resp, file="./Figures/fig_dist_2_gender_resp.png",
        width=10, height=6, unit="in")
                  
                 
    
  ####*Social Distance-Familie####
  fig_dist_3_gender_resp<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_family,white_family,black_family,muslim_family,gender_resp)%>%
        pivot_longer(cols = ends_with("family"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","gender_resp"),na.rm=T)%>%
        filter(!is.na(gender_resp))%>%
        filter(gender_resp!="divers")%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp, color=gender_resp, fill=gender_resp,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.7), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_3_gender_resp
    ggsave(fig_dist_3_gender_resp, file="./Figures/fig_dist_3_gender_resp.png",
        width=10, height=6, unit="in")
                   
                   
    ####*Asian Culture####
    fig_asian_culture_gender_resp<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_trust,asian_culture,asian_children,gender_resp)%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","gender_resp"), na.rm=T)%>%
        filter(!is.na(gender_resp))%>%
         filter(gender_resp!="divers")%>%
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp, color=gender_resp, fill=gender_resp,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.4), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Kinder","Kultur","Freundschaft \nund Vertrauen"))
    fig_asian_culture_gender_resp
      ggsave(fig_asian_culture_gender_resp, file="./Figures/fig_asian_culture_gender_resp.png",
        width=10, height=6, unit="in")
           
    ####*Asian Blame####
    fig_asian_blame_gender_resp<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_blame_1,asian_blame_2,gender_resp)%>%
        filter(gender_resp!="divers")%>%
        pivot_longer(cols = starts_with("asian"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","gender_resp"), na.rm=T)%>%
        filter(!is.na(gender_resp))%>%
        
        ggplot()+
          aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp, color=gender_resp, fill=gender_resp,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))+
          ylab("% Zustimmung")+xlab("")+
          scale_x_discrete(labels=c("Asiaten sind schuld","Asiaten sind an der \nschnellen Ausbreitung schuld"))
      fig_asian_blame_gender_resp
      ggsave(fig_asian_blame_gender_resp, file="./Figures/fig_asian_blame_gender_resp.png",
        width=10, height=6, unit="in")
        
    
    
  ####*Social Distance for Subgroups####
   ####**Gender####
    fig_dist_1_gender<-
      DATA%>%
        filter(white_resp=="Yes")%>%
        select(asian_nachbar,white_nachbar,black_nachbar,muslim_nachbar,gender_resp)%>%
        pivot_longer(cols = ends_with("nachbar"))%>%
        summarySE(., measurevar = "value",groupvar=c("name","gender_resp"),na.rm=T)%>%
        filter(gender_resp!="divers")%>%
        ggplot()+
        aes(x=name, y=value, ymin=value-1.96*se,ymax=value+1.96*se,
              group=gender_resp, color=gender_resp, fill=gender_resp,
              label=paste(100*round(value,3)),sep="")+
          geom_col(position="dodge")+
          geom_errorbar( position = position_dodge(width=0.9), colour="black", width=0.3)+
          geom_text(aes(y=value+1.96*se+0.005), color="black", 
                    position = position_dodge(width=0.9),
                    size=4)+
          theme_bw()+
          scale_y_continuous(limits = c(0,0.3), expand = c(0, 0))+
          ylab("% Unangenehm")+xlab("Bevölkerungsgruppe")+
          scale_x_discrete(labels=c("Asian","Black","Muslim","White"))
    
    fig_dist_1_gender 
    ggsave(fig_dist_1_gender, file="./Figures/fig_dist_1_gender.png",
        width=6, height=6, unit="in")
      
    
             
             
             
             
      





####General Evaluations####
  ####*General Evaluation, comparison between white/Asian####
  DATA%>%
    filter(white_resp=="Yes")%>%
    select(eval_asian, eval_white)%>%
    summary(.)

  
  ####*General Evaluation, by left-right self placement
  DATA%>%
    group_by(lr)%>% 
    dplyr::summarize(eval_asian=mean(eval_asian, na.rm=T),
              eval_white=mean(eval_white, na.rm=T),
              eval_black=mean(eval_black, na.rm=T))

####Social Distance Measures####
  ####*Nachbar####
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(asiate_nachbar=case_when(soc_dist_asian_1 <0 ~ 1,
                                    soc_dist_asian_1 >=0 ~0))%>%
    select(asiate_nachbar)%>%
    summary(.)
  
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(asiate_arbeit=case_when(soc_dist_asian_2 <0 ~ 1,
                                   soc_dist_asian_2 >=0 ~0))%>%
    select(asiate_arbeit)%>%
    summary(.)

   
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(asiate_fam=case_when(soc_dist_asian_3 <0 ~ 1,
                                   soc_dist_asian_3 >=0 ~0))%>%
    select(asiate_fam)%>%
    summary(.)
  
  #Vergleich zu Weissen 
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(white_fam=case_when(soc_dist_white_3 <0 ~ 1,
                                   soc_dist_white_3 >=0 ~0))%>%
    select(white_fam)%>%
    summary(.)
  
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(black_fam=case_when(soc_dist_black_3 <0 ~ 1,
                              soc_dist_black_3 >=0 ~0))%>%
    select(black_fam)%>%
    summary(.)
  
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(asian_fam=case_when(soc_dist_asian_3 <0 ~ 1,
                                   soc_dist_asian_3 >=0 ~0))%>%
    group_by(corona_fear)%>%
    dplyr::summarize(asian_fam=mean(asian_fam,na.rm=T))%>%
    filter(!is.na(corona_fear))%>%
    mutate(corona_fear=unclass(corona_fear))%>%
    ggplot()+
      aes(x=corona_fear,y=asian_fam)+
      geom_col()
  
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(white_fam=case_when(soc_dist_white_3 <0 ~ 1,
                                soc_dist_white_3 >=0 ~0))%>%
    group_by(corona_fear)%>%
    dplyr::summarize(white_fam=mean(white_fam,na.rm=T))%>%
    filter(!is.na(corona_fear))%>%
    mutate(corona_fear=unclass(corona_fear))%>%
    ggplot()+
      aes(x=corona_fear,y=white_fam)+
      geom_col()
  
  
  
  ####*Blame####
  
  summary(DATA$asian_blame)
  DATA%>%
    filter(asian_resp=="No")%>%
    mutate(asian_blame_1=case_when(asian_blame_1 >4 ~ 1,
                              asian_blame_1 <=4 ~0))%>%
    select(asian_blame_1)%>%
    summary(.)
  
    DATA%>%
    filter(asian_resp=="No")%>%
    mutate(asian_blame_2=case_when(asian_blame_2 >4 ~ 1,
                              asian_blame_2 <=4 ~0))%>%
    select(asian_blame_2)%>%
    summary(.)
  
  
  
  
  
  
  