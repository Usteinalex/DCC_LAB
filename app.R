#########################################################################################
#             PROJET TUTORE : ANALYSE DE DEFAILLANCE DE CAUSE COMMUNE                   #
#                                                                                       #
#
# TUTEUR: M.GOUNO                                 ACTEURS: AUGUSTIN ZOUNGRANA           #
#                                                          YACOUBA KONE                 #
#                                                          PIMA BAZIE                   #
# DATE DEBUT: 15/01/2018                            DATE DE FIN: 14/05/2018             #
########################################################################################

# http://www.sthda.com/french/wiki/ggplot2-graphique-lineaire-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
# https://idc9.github.io/stor390/notes/shiny.html

#install.packages("VGAM")
#install.packages("lawstat")
## Pour excecuter son application et voir le code a coté
# runApp("my_app", display.mode = "showcase")
# install.packages("gridExtra")
# install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(VGAM)
library(lawstat)
library(datasets)
library(data.table)
library(reshape2)
library(scales)
library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "DCC_Lab",
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 2, color = "blue",
                                        "Creation du classifieur"
                               ),
                               taskItem(value = 60, color = "aqua",
                                        "Evolution Project"
                               ),
                               taskItem(value = 2, color = "yellow",
                                        "Creation de package "
                               ),
                               taskItem(value = 50, color = "red",
                                        "Redaction du rapport"
                               )
                  ),
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Utilisateur",
                                 message = "manuel d'utilisation?",
                                 icon = icon("question"),
                                 time = Sys.Date()
                               ),
                               messageItem(
                                 from = "Contact",
                                 message = "M.GOUNO",
                                 icon = icon("life-ring"),
                                 href ="http://web.univ-ubs.fr/lmba/gouno/"
                               )
                  )
  ),
  dashboardSidebar(
    selectInput("categorie",
                "Dossiers",
                c("REX_visible","REX_ominiscient"))
    ,
    
    numericInput(inputId = "num",
                label = "Choix du fichier 1:100",
                value = 1, min = 1, max = 100)
    ,
    selectInput("methode",
                "Methode de traitement",
                c("Chronique unique","Ensemble des chroniques"))
    ,
    numericInput(inputId = "chro",
                label = "Numero de la chronique 1:50",
                value = 1, min = 1, max = 50)
    ,
    selectInput("Proc",
                "Modèle",
                c("PPH","PPL"))
  ,
  selectInput("test",
              "Test",
              c("Kolmogorov","Laplace"))
),
  dashboardBody(
    fluidPage(
      mainPanel(tabsetPanel(
        tabPanel("Présentation Générale",verbatimTextOutput("Accueil"),width=650),
        tabPanel("Plot",plotOutput("ploter",height = 500,width=650)),
        tabPanel("Inférence",height = 500,width=650,
                 fluidRow(height = 500,width=650,
                   tabBox(height = 500,width=650,
                    title = tagList(shiny::icon("gear"), ""),
                     tabPanel("Tests",height = 500,width=650,
                 dataTableOutput("stat"),
                box("dégré de liberté",width = 650,
                     sliderInput(inputId = "alpha",
                                 label = "Choix du risque d'erreur alpha",
                                 value = 0.05, min = 0.01, max = 0.1,step = 0.01)),
                box("Interpretation",width = 650,
                     verbatimTextOutput("comment"))),
               tabPanel("Estimations",height = 500,width=650,
                        dataTableOutput("estimation")))
        )),
        tabPanel("Table",dataTableOutput("tab"))
      )
      )
    )
  )
)
server <- function(input, output) {
  #________________________ MENU haut droite _____________________________  
  
  
  #________________________________Partie Plot____________________________________________    
  
  output$ploter <- renderPlot({
    val<-input$num  
    path1<-paste("REX_MANON_NEW/REX_ominiscient/REX_Complet_Omniscient_UBS_Sim_",val,".csv",sep="")
    path2 <-paste("REX_MANON_NEW/REX_visible/REX_Complet_Visible_UBS_Sim_",val,".csv",sep="") 
    
    if (input$categorie=="REX_ominiscient") {
      donnees<-read.csv(path1,header = T, sep=";")
    } 
    else {
      donnees<-read.csv(path2,header = T, sep=";")
    }
    names(donnees)<-c("Chro","Date","Type","Nombre","Cause")
    if (input$methode=="Chronique unique") {
        # selection d'observation et tableau croisé ------------------------------------------------------
        chronique<-subset(donnees,Chro ==input$chro )
      if (dim(chronique)[1]==0 | dim(chronique)[1]==1){
        ggplot(chronique,aes(x =seq(0,15):seq(0,1500,by=100))) +
          theme(legend.position="bottom") +
          theme_update(plot.title = element_text(hjust = 0.5)) +
          labs(title=paste0("Dans la chronique N° ",input$chro," le nombre d'evenement est très peu : inferieur à 2",sep=""),
               x = "Temps",y="Nombre cumulé de defaillance") 
      }
      else {
        # creation de la varaible Temps et Compo de DCC NL -----------------------------------------------
        date_i<-sort(as.Date(chronique$Date, format='%d/%m/%Y'))
        Temps_pph<-rep(0,length(chronique$Date))
        Temps_ppl<-rep(0,length(chronique$Date))
        compo_ppl<-rep(0,length(chronique$Date))
        M_inter<-rep(0,length(chronique$Date))
        chronique<-cbind(chronique,Temps_pph,Temps_ppl,compo_ppl,M_inter)
   
        
        for (j in 1:length(Temps_pph)){
          chronique$Temps_pph[j]<-as.integer(difftime(date_i[j],date_i[1], units = "auto"))
        }
        
       # for (j in 1:length(Temps_pph)-1) {
       #   chronique$Temps_pph[j]<-(chronique$Temps_pph[j+1]+chronique$Temps_pph[j])/2
        #}
        # Initialisation de M_inter
        
       for (j in 1:length(Temps_pph)-1) {
         chronique$M_inter[j]<-(chronique$Temps_pph[j+1]+chronique$Temps_pph[j])/2
       }
        chronique$M_inter[1]<-0
        chronique$Nombre[1]<-0
        chronique$Temps_pph[1]<-0
        chronique$Nombre<-cumsum(chronique$Nombre)
        
        if (input$Proc=="PPH") {
          ## L'intensité du processus PPH
        #  lambda_pph_mc<-(sum(chronique$Nombre*chronique$Temps_pph)/sum(chronique$Temps_pph^2))*chronique$Temps_pph
        #  lambda_pph_mle<-(max(chronique$Nombre)/max(chronique$Temps_pph))*chronique$Temps_pph
        # chronique<-cbind(chronique,lambda_pph_mc,lambda_pph_mle)
          
          lambda_pph_mc<-(sum(chronique$Nombre*chronique$M_inter)/sum(chronique$M_inter^2))*chronique$M_inter
           lambda_pph_mle<-(max(chronique$Nombre)/max(chronique$M_inter))*chronique$M_inter
           chronique<-cbind(chronique,lambda_pph_mc,lambda_pph_mle)
          
          # "dotted"  
          ## Test graphique du modele PPH -----------------------------------------------------------------
          ggplot(chronique[-length(Temps_pph),],aes(x =Temps_pph,y= Nombre)) +
            geom_point(color="red",size=2) +
            geom_step() +
           geom_line(show.legend=T,aes(fill="MC",x =M_inter,y=lambda_pph_mc),linetype="twodash", col="black") +
            geom_line(show.legend=T,aes(fill="MLE",x =M_inter,y=lambda_pph_mle),linetype="dotted", col="black") +
            theme(legend.position="bottom") +
            theme_update(plot.title = element_text(hjust = 0.5)) +
            labs(title=paste0(sep="","Chronique : N°",input$chro,"  -- R² = ",round(as.numeric((summary(lm(chronique$Nombre[-length(Temps_pph)]~chronique$Temps_pph[-length(Temps_pph)])))[8]),2)),
                 x = "t",y="E(N(t))",fill="Ajustement") 
        }
        else {
          # creation des donnÃ©es Processus Power law ------------------------------------------------------
          chronique$compo_ppl<-ifelse(chronique$Nombre!=0,log(chronique$Nombre),0)
          chronique$Temps_ppl<-ifelse(chronique$Temps_pph!=0,log(chronique$Temps_pph),0)
          
          ## L'intensitÃ© du processus PPL
          beta<-length(chronique$Temps_pph)/sum(ifelse(chronique$Temps_pph==0,0,log(max(chronique$Temps_pph)/chronique$Temps_pph)))
          alpha<-max(chronique$Temps_pph)/(length(chronique$Temps_pph)^(1/beta))
          lambda_ppl_mle<-beta*chronique$Temps_ppl-beta*log(alpha)
          lambda_ppl_mc<-(lm(chronique$compo_ppl~chronique$Temps_ppl))$coefficients[1]+(lm(chronique$compo_ppl~chronique$Temps_ppl))$coefficients[2]*chronique$Temps_ppl
          G_lambda<-(chronique$Temps_pph/alpha)^beta
          chronique<-cbind(chronique,lambda_ppl_mle,lambda_ppl_mc,G_lambda)
          
          ## Test graphique du modele PPH -----------------------------------------------------------------
          fig1<-ggplot(chronique[-length(Temps_pph),],aes(x =Temps_ppl,y= compo_ppl)) +
            geom_point(color="red",size=2) +
            geom_line(show.legend=T,aes(fill="MLE",x =Temps_ppl,y=lambda_ppl_mle),linetype="dotted", col="black") +
            theme_update(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position="right") +
            labs(title=paste0(sep="","Chronique : N°",input$chro,"  -- R² = ",round(as.numeric((summary(lm(chronique$compo_ppl[-length(compo_ppl)]~chronique$Temps_ppl[-length(compo_ppl)])))[8]),2)),
                 x = "log(t)",y="log(N(t))",fill="Ajustements") 
        
        # Seconde partie    
        fig2<-  ggplot(chronique[-length(Temps_pph),],aes(x =Temps_pph,y= Nombre)) +
            geom_point(color="red",size=2) +
            geom_step() +
            geom_line(show.legend=T,aes(fill="MLE",x =Temps_pph,y=G_lambda),linetype="dotted", col="black") +
            theme_update(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position="right") +
            labs(title=paste0(sep="","Chronique : N°",input$chro,"  -- R² = ",round(as.numeric((summary(lm(chronique$compo_ppl[-length(compo_ppl)]~chronique$Temps_ppl[-length(compo_ppl)])))[8]),2)),
                 x = "t",y="E(N(t))",fill="Ajustements") 
        grid.arrange(fig1, fig2 ,ncol=2, nrow = 1)
        
        }
      }
    }
    else {
      
      #************************ Analyse de l'essemble de chroniques **********************************
      data1<-donnees
      date_c<-sort(as.Date(data1$Date, format='%d/%m/%Y'))
      Temps_cpph<-rep(0,length(data1$Date))
      Temps_cppl<-rep(0,length(data1$Date))
      compo_cppl<-rep(0,length(data1$Date))
      M_inter_cppl<-rep(0,length(data1$Date))
      data1<-cbind(data1,Temps_cpph,Temps_cppl,compo_cppl,M_inter_cppl)
      
      # Traitement ensemble processus pph -------------------------------------------------------------
      
      for (j in 1:length(Temps_cpph)){
        data1$Temps_cpph[j]<-as.integer(difftime(date_c[j],date_c[1], units = "auto"))
      }
      for (j in 1:length(Temps_cpph)-1) {
        data1$M_inter_cppl[j]<-(data1$Temps_cpph[j+1]+data1$Temps_cpph[j])/2
      }
      data1$M_inter_cppl[1]<-0
      data1$Nombre[1]<-0
      data1$Temps_cpph[1]<-0
      data1$Nombre<-cumsum(data1$Nombre)
      
      # intensite du processus
      lambda_cpph_mc<-(sum((data1$Nombre*data1$M_inter_cppl)/sum(data1$M_inter_cppl^2)))*data1$M_inter_cppl
      lambda_cpph_mle<-(max(data1$Nombre)/max(data1$M_inter_cppl))*data1$M_inter_cppl
      data1<-cbind(data1,lambda_cpph_mc,lambda_cpph_mle)
      
      if (input$Proc=="PPH") {
        ggplot(data1[-length(Temps_cpph),],aes(x =Temps_cpph,y= Nombre)) +
          geom_point(color="red") +
          geom_step() +
          geom_line(show.legend=T,aes(fill="MC",x =M_inter_cppl,y=lambda_cpph_mc),linetype="twodash", col="black") +
          geom_line(show.legend=T,aes(fill="MLE",x =M_inter_cppl,y=lambda_cpph_mle),linetype="dotted", col="black") +
          theme_update(plot.title = element_text(hjust = 0.5)) +
          labs(title=paste0("Ensemble Chroniques -- R² = ",round(as.numeric((summary(lm(data1$Nombre[-length(Temps_cpph)]~data1$Temps_cpph[-length(Temps_cpph)])))[8]),2)),
               x = "t",y="E(N(t))",fill="Ajustement")
      }
      else {
        
        # Traitement ensemble processus ppl -------------------------------------------------------------
        
        # creation ensemble des données Processus Power law --------------------------------------------
        
        data1$compo_cppl<-ifelse(data1$Nombre!=0,log(data1$Nombre),0)
        data1$Temps_cppl<-ifelse(data1$Temps_cpph!=0,log(data1$Temps_cpph),0)
        
        ## Test graphique du modele PPL ----------------------------------------------------------------
        beta<-length(data1$Temps_cpph)/sum(ifelse(data1$Temps_cpph==0,0,log(max(data1$Temps_cpph)/data1$Temps_cpph)))
        alpha<-max(data1$Temps_cpph)/(length(data1$Temps_cpph)**(1/beta))
        lambda_cppl_mle<-beta*data1$Temps_cppl-beta*log(alpha)
        lambda_ppl_mc<-(lm(data1$compo_cppl~data1$Temps_cppl))$coefficients[1]+(lm(data1$compo_cppl~data1$Temps_cppl))$coefficients[2]*data1$Temps_cppl
        G_lambda<-(data1$Temps_cpph/alpha)^beta
        data1<-cbind(data1,lambda_cppl_mle,lambda_ppl_mc,G_lambda)
        
        fig11<-ggplot(data1[-length(Temps_cppl),],aes(x =Temps_cpph,y= Nombre)) +
          geom_point(color="blue") +
          geom_line() +
          geom_line(show.legend=T,aes(fill="MLE",x =Temps_cpph,y=G_lambda),linetype="dotted", col="black") +
          theme_update(plot.title = element_text(hjust = 0.5)) +
          labs(title=paste0("Ensemble Chroniques -- R²=",round(as.numeric((summary(lm(data1$compo_cppl[-length(Temps_cppl)]~data1$Temps_cppl[-length(Temps_cppl)])))[8]),2)),
               x = "t",y="E(N(t))",fill="Ajsutements")
        
        fig12<-ggplot(data1[-length(Temps_cppl),],aes(x =Temps_cppl,y= compo_cppl)) +
          geom_point(color="blue") +
          geom_line() +
          geom_line(show.legend=T,aes(fill="MLE",x =Temps_cppl,y=lambda_cppl_mle),linetype="dotted", col="black") +
          theme_update(plot.title = element_text(hjust = 0.5)) +
          labs(title=paste0("Ensemble Chroniques -- R²=",round(as.numeric((summary(lm(data1$compo_cppl[-length(Temps_cppl)]~data1$Temps_cppl[-length(Temps_cppl)])))[8]),2)),
               x = "log(t)",y="log(N(t))",fill="Ajsutements")
        grid.arrange(fig12, fig11, ncol=2, nrow = 1)
      }
    }
    #________________________________Triatement par ordre ____________________________________________    
  }) 
  
  #________________________________Partie Table____________________________________________    
  
  output$tab<- renderDataTable({
    val<-input$num  
    path1<-paste("REX_MANON_NEW/REX_ominiscient/REX_Complet_Omniscient_UBS_Sim_",val,".csv",sep="")
    path2 <-paste("REX_MANON_NEW/REX_visible/REX_Complet_Visible_UBS_Sim_",val,".csv",sep="") 
    
    if (input$categorie=="REX_ominiscient") {
      donnees<-read.csv(path1,header = T, sep=";")
    } 
    else {
      donnees<-read.csv(path2,header = T, sep=";")
    }
    names(donnees)<-c("Chro","Date","Type","Nombre","Cause")
    if(input$methode=="Chronique unique"){   
      #   selection d'observation et tableau croisé ------------------------------------------------------
      chronique<-subset(donnees,Chro ==input$chro) 
      dcast(chronique, formula = Type+Nombre ~ Cause,margins =TRUE)
    }
    else {
      dcast(donnees, formula = Type+Nombre ~ Cause,margins =TRUE)
    }
  })
  
  #________________________________Partie statistiques____________________________________________    
  output$stat<-renderDataTable({
    val<-input$num  
    path1<-paste("REX_MANON_NEW/REX_ominiscient/REX_Complet_Omniscient_UBS_Sim_",val,".csv",sep="")
    path2 <-paste("REX_MANON_NEW/REX_visible/REX_Complet_Visible_UBS_Sim_",val,".csv",sep="") 
    
    if (input$categorie=="REX_ominiscient") {
      donnees<-read.csv(path1,header = T, sep=";")
    } 
    else {
      donnees<-read.csv(path2,header = T, sep=";")
    }
    names(donnees)<-c("Chro","Date","Type","Nombre","Cause")
    if (input$methode=="Chronique unique") {
        # selection d'observation et tableau croisé ------------------------------------------------------
        chronique<-subset(donnees,Chro ==input$chro)
      if (dim(chronique)[1]==0 | dim(chronique)[1]==1){
        return()
      }
      else{
        # creation de la varaible Temps et Compo de DCC NL -----------------------------------------------
        date_i<-sort(as.Date(chronique$Date, format='%d/%m/%Y'))
        Temps_pph<-NULL
        Temps_ppl<-NULL
        for (j in 1:length(date_i)-1){
          Temps_pph[j]<-as.integer(difftime(date_i[j+1],date_i[j], units = "auto"))
        }
      if(input$test=="Kolmogorov"){
        libele<-c("Statistique","P.value","risque d'erreur alpha")
        if (input$Proc=="PPH") {
          t<-ks.test(Temps_pph,"pexp",mean(Temps_pph),sd(Temps_pph))
          valeurs<-c(t$statistic,t$p.value,input$alpha)
          cbind(libele,valeurs)
        }
        else {
          Temps_ppl<-ifelse(Temps_pph!=0,log(Temps_pph),0)
          t<-ks.test(Temps_ppl,"pexp",mean(Temps_ppl),sd(Temps_ppl))
          valeurs<-c(t$statistic,t$p.value,input$alpha)
          cbind(libele,valeurs)
        }
      }
     else {
       libele<-c("Statistique","risque d'erreur alpha")
         if (input$Proc=="PPH") {
         t<-laplace.test(Temps_pph)
         valeurs<-c(t$A2,input$alpha)
         cbind(libele,valeurs)
       }
       else {
         Temps_ppl<-ifelse(Temps_pph!=0,log(Temps_pph),0)
         t<-laplace.test(Temps_ppl)
         valeurs<-c(t$A2,input$alpha)
         cbind(libele,valeurs)
        }
       }
      }
    }
  else {
        # selection d'observation et tableau croisé ------------------------------------------------------
        data1<-subset(donnees,Chro ==input$chro )
      if (dim(data1)[1]==0 | dim(data1)[1]==1){
        return()
      }
      else{
        data1<-donnees
        date_c<-sort(as.Date(data1$Date, format='%d/%m/%Y'))
        Temps_cpph<-NULL
        Temps_cppl<-NULL
        for (j in 1:length(date_c)-1){
          Temps_cpph[j]<-as.integer(difftime(date_c[j+1],date_c[j], units = "auto"))
        }
      if(input$test=="Kolmogorov"){
        libele<-c("Statistique","P.value","risque d'erreur alpha")
         if (input$Proc=="PPH") {
          # Traitement ensemble processus pph -------------------------------------------------------------
          t<-ks.test(Temps_cpph,"pexp",mean(Temps_cpph),sd(Temps_cpph))
          valeurs<-c(t$statistic,t$p.value,input$alpha)
          cbind(libele,valeurs)
        }
        else{
          Temps_cppl<-ifelse(Temps_cpph!=0,log(Temps_cpph),0)
          t<-ks.test(Temps_cppl,"pexp",mean(Temps_cppl),sd(Temps_cppl))
          valeurs<-c(t$statistic,t$p.value,input$alpha)
          cbind(libele,valeurs)
        }
       }
      else{
        libele<-c("Statistique","risque d'erreur alpha")
        if (input$Proc=="PPH") {
          # Traitement ensemble processus pph -------------------------------------------------------------
          t<-laplace.test(Temps_cpph)
          valeurs<-c(t$A2,input$alpha)
          cbind(libele,valeurs)
        }
        else{
          Temps_cppl<-ifelse(Temps_cpph!=0,log(Temps_cpph),0)
          t<-laplace.test(Temps_cppl)
          valeurs<-c(t$A2,input$alpha)
          cbind(libele,valeurs)
        }
      }
    }
  }
  })
  
#_____________________ Estimation des parammètes alpha et beta et lambda
  output$estimation<-renderDataTable({
    val<-input$num  
    path1<-paste("REX_MANON_NEW/REX_ominiscient/REX_Complet_Omniscient_UBS_Sim_",val,".csv",sep="")
    path2 <-paste("REX_MANON_NEW/REX_visible/REX_Complet_Visible_UBS_Sim_",val,".csv",sep="") 
    
    if (input$categorie=="REX_ominiscient") {
      donnees<-read.csv(path1,header = T, sep=";")
    } 
    else {
      donnees<-read.csv(path2,header = T, sep=";")
    }
    names(donnees)<-c("Chro","Date","Type","Nombre","Cause")
    if (input$methode=="Chronique unique") {
      # selection d'observation et tableau croisé ------------------------------------------------------
      chronique<-subset(donnees,Chro ==input$chro )
      if (dim(chronique)[1]==0 | dim(chronique)[1]==1){
        return()
      }
      else {
        # creation de la varaible Temps et Compo de DCC NL -----------------------------------------------
        date_i<-sort(as.Date(chronique$Date, format='%d/%m/%Y'))
        Temps_pph<-rep(0,length(chronique$Date))
        Temps_ppl<-rep(0,length(chronique$Date))
        compo_ppl<-rep(0,length(chronique$Date))
        M_inter<-rep(0,length(chronique$Date))
        chronique<-cbind(chronique,Temps_pph,Temps_ppl,compo_ppl,M_inter)

        for (j in 1:length(Temps_pph)){
          chronique$Temps_pph[j]<-as.integer(difftime(date_i[j],date_i[1], units = "auto"))
        }
    
        for (j in 1:length(Temps_pph)-1) {
          chronique$M_inter[j]<-(chronique$Temps_pph[j+1]+chronique$Temps_pph[j])/2
        }
        chronique$M_inter[1]<-0
        chronique$Nombre[1]<-0
        chronique$Temps_pph[1]<-0
        chronique$Nombre<-cumsum(chronique$Nombre)
        
        if (input$Proc=="PPH") {
        
          lambda_pph_mc<-(sum(chronique$Nombre*chronique$M_inter)/sum(chronique$M_inter^2))
          lambda_pph_mle<-(max(chronique$Nombre)/max(chronique$M_inter))

          ## Test graphique du modele PPH -----------------------------------------------------------------
          Parametre<-c("MC","MLE")
          Estimation<-c(lambda_pph_mc,lambda_pph_mle)
          cbind(Parametre,Estimation)
        }
        else {
          # creation des donnÃ©es Processus Power law ------------------------------------------------------
          chronique$compo_ppl<-ifelse(chronique$Nombre!=0,log(chronique$Nombre),0)
          chronique$Temps_ppl<-ifelse(chronique$Temps_pph!=0,log(chronique$Temps_pph),0)
          
          ## L'intensitÃ© du processus PPL
          beta<-length(chronique$Temps_pph)/sum(ifelse(chronique$Temps_pph==0,0,log(max(chronique$Temps_pph)/chronique$Temps_pph)))
          alpha<-max(chronique$Temps_pph)/(length(chronique$Temps_pph)^(1/beta))
          Parametre<-c("alpha","Beta")
          Estimation<-c(alpha,beta)
           cbind(Parametre,Estimation)
        }
      }
    }
      else {
        
        #************************ Analyse de l'essemble de chroniques **********************************
        data1<-donnees
        date_c<-sort(as.Date(data1$Date, format='%d/%m/%Y'))
        Temps_cpph<-rep(0,length(data1$Date))
        Temps_cppl<-rep(0,length(data1$Date))
        compo_cppl<-rep(0,length(data1$Date))
        M_inter_cppl<-rep(0,length(data1$Date))
        data1<-cbind(data1,Temps_cpph,Temps_cppl,compo_cppl,M_inter_cppl)
        
        # Traitement ensemble processus pph -------------------------------------------------------------
        
        for (j in 1:length(Temps_cpph)){
          data1$Temps_cpph[j]<-as.integer(difftime(date_c[j],date_c[1], units = "auto"))
        }
        for (j in 1:length(Temps_cpph)-1) {
          data1$M_inter_cppl[j]<-(data1$Temps_cpph[j+1]+data1$Temps_cpph[j])/2
        }
        data1$M_inter_cppl[1]<-0
        data1$Nombre[1]<-0
        data1$Temps_cpph[1]<-0
        data1$Nombre<-cumsum(data1$Nombre)
        
        # intensite du processus
        lambda_cpph_mc<-(sum((data1$Nombre*data1$M_inter_cppl)/sum(data1$M_inter_cppl^2)))
        lambda_cpph_mle<-(max(data1$Nombre)/max(data1$M_inter_cppl))

        if (input$Proc=="PPH") {
          Parametre<-c("MC","MLE")
          Estimation<-c(lambda_cpph_mc,lambda_cpph_mle)
          cbind(Parametre,Estimation)
        }
        else {
          
          # Traitement ensemble processus ppl -------------------------------------------------------------
          
          # creation ensemble des données Processus Power law --------------------------------------------
          
          data1$compo_cppl<-ifelse(data1$Nombre!=0,log(data1$Nombre),0)
          data1$Temps_cppl<-ifelse(data1$Temps_cpph!=0,log(data1$Temps_cpph),0)
          
          ## Test graphique du modele PPL ----------------------------------------------------------------
          beta<-length(data1$Temps_cpph)/sum(ifelse(data1$Temps_cpph==0,0,log(max(data1$Temps_cpph)/data1$Temps_cpph)))
          alpha<-max(data1$Temps_cpph)/(length(data1$Temps_cpph)**(1/beta))
          Parametre<-c("alpha","Beta")
          Estimation<-c(alpha,beta)
          cbind(Parametre,Estimation)
        }
      }
  })
  
  #_______________________ Partie text __________________________  
  output$comment<-renderText({
  #  paste("Un processus de Poisson est dit homogène si la statistique du test de Kolmogorov-Smirnov sur la fonction de répartiton des interarrivées Kn=sup x dans R |Fn(t)−F(t)| > kn(α). On rejète alors l'hypothèse nulle i.e interarrivées exponentielles ou si p.values < α = ", input$alpha ," où kn(α) est lu dans la table des quantiles de la statistique de Kolmogorov-Smirnov.")
  })
  output$Accueil<-renderText({
  paste0("DCC_LAB : Un Utilitaire SHINY pour la Modé-\t lisation et l'Analyse Statistique de Données de\t Défaillance de Cause Commune en Fiabilité\n
DCC_LAB est un outil de modélisation et d'analyse en fiabilité qui permet l'automatisation du \t
traitement des données de défaillances de causes communes. Il est développé par trois étudiants en master Data Science et Modélisation Statistique à l'Université de Bretagne Sud (UBS). \n 
Ce logiciel propose des analyses pour les modèles classiquement utilisés dans\t l'industrie et  dans d'autres secteurs d'activités. \n
DCC_LAB analyse les données en se basant sur une modélisation par le Processus de Poisson Homogène (PPH) ou Non Homogène Processus Power Law (PPL).Il propose un test graphique d'ajustement, un \t test d'homogénéité (Test de Kolmogorov-Smirnov,\t Test de Laplace) et une estimation des paramètres du modèle (Maximum de Vraisemblance ou Moindres Carrées).\n
Il donne également un tableau de contingence\t croisant ordre de la défaillance et nature de\t celle-ci pour une chronique donnée.")  
    
  })
  
}   




# Run the application 
shinyApp(ui = ui, server = server)

