library(knitr)
library(shiny)
library(xlsx)
library(evaluate)
shinyServer(function(input, output, ClientData, session) {
  #tags$head(includeScript("GoogleAnalytics.js"))


#Step1: number of subjects in trial
   number <- eventReactive(input$basedata, {
 
    isolate ({  
    inFile <- input$file1
      if (is.null(inFile))
      {paste0(('Please upload data.'))
        } else {if (input$NTD=='1')
            {data <- read.xlsx(inFile$datapath,1)
              data$n <- 1
              n <- length(data[,1])
              # S haematobium
              data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
              data$shF <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
              
              # S mansoni
              data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
              data$smF <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))
              
              # S japonicum
              data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
              data$sjF <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))
              
              data$inf <- ifelse(data$sh > -2 | data$sm > -2 | data$sj > -2, 1, 0)
              data$inf2 <- ifelse(data$shF > -2 | data$smF > -2 | data$sjF > -2, 1, 0)
              
              
              if(mean(data$inf)==0 | mean(data$inf2) == 0) {paste('Please provide egg count data.')}
              else {
                if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$shF)>-2 & mean(data$smF)>-2)
                   {
                  n <- length(data[,1])
                  data$shB <-  data[,input$Shbas]  
                  data$smB <-  data[,input$Smbas] 
                  data$shF <-  data[,input$Shfol]  
                  data$smF <-  data[,input$Smfol] 
                  data$shp <- ifelse(data$shB>0,1,0) 
                  data$smp <- ifelse(data$smB>0,1,0)
                  data$shf <- ifelse(data$shF>=0,1,0) 
                  data$smf <- ifelse(data$smF>=0,1,0) 
                  data$shc <- data$shp + data$shf
                  data$smc <- data$smp + data$smf
                  data$com <- data$shc + data$smc
                  mix <- dim(subset(data, smB>0 & shB>0))[1]
                  com <- dim(subset(data, shc==2 | smc ==2))[1]
                  nsm <- dim(subset(data, smB>0))[1]
                  nsm2 <- dim(subset(data, smB>0 & smF>=0))[1]
                  nsh <- dim(subset(data,shB>0))[1]
                  nsh2 <- dim(subset(data, shB>0 & shF>=0))[1]
                  paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. 
<em>Schistosoma haematobium</em> infections were observed in', nsh, 'subjects (', round(100*nsh/n,1), '% ), 
<em>S. mansoni</em> infections in', nsm, 'subjects (', round(100*nsm/n,1),'% ). Mixed <em>Schistosoma</em> infections 
                          were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). 
                        Complete data were available for', com, 'subjects, 
                        including', nsh2, 'cases of <em>S. haematobium</em>, and', nsm2, 'cases of <em>S. mansoni</em>.')
                   }
                else{
                  if(mean(data$sh)>-2 & mean(data$shF)>-2)
                  {
                    n <- length(data[,1])
                    data$shB <-  data[,input$Shbas]  
                    data$shF <-  data[,input$Shfol]  
                    data$shp <- ifelse(data$shB>0,1,0) 
                    data$shf <- ifelse(data$shF>=0,1,0) 
                    data$shc <- data$shp + data$shf
                    nsh <- dim(subset(data,shB>0))[1]
                    nsh2 <- dim(subset(data, shB>0 & shF>=0))[1]
                    paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Schistosoma haematobium</em> infections were observed in', nsh, 
                          'subjects (', round(100*nsh/n,1), '% ). Complete data were available for', nsh2, 'subjects.')
                    }
                    else{
                    if(mean(data$sm)>-2 & mean(data$smF)>-2){
                      n <- length(data[,1])
                      data$smB <-  data[,input$Smbas] 
                      data$smF <-  data[,input$Smfol] 
                      data$smp <- ifelse(data$smB>0,1,0)
                      data$smf <- ifelse(data$smF>=0,1,0) 
                      data$smc <- data$smp + data$smf
                      nsm <- dim(subset(data, smB>0))[1]
                      nsm2 <- dim(subset(data, smB>0 & smF>=0))[1]
                      paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Schistosoma mansoni</em> infections were observed in', nsm, 
                            'subjects (', round(100*nsm/n,1), '% ). Complete data were available for', nsm2, 'subjects.')
                      
                      }
                    else{
                      if(mean(data$sj)>-2 & mean(data$sjF>-2)){
                        n <- length(data[,1])
                        data$sjB <-  data[,input$Sjbas] 
                        data$sjF <-  data[,input$Sjfol] 
                        data$sjp <- ifelse(data$sjB>0,1,0)
                        data$sjf <- ifelse(data$sjF>=0,1,0) 
                        data$sjc <- data$sjp + data$sjf
                        nsj <- dim(subset(data, sjB>0))[1]
                        nsj2 <- dim(subset(data, sjB>0 & sjF>=0))[1]
                        paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Schistosoma japonicum</em> infections were observed in', nsj, 'subjects (', round(100*nsj/n,1), '% ). Complete data was available for', nsj2, 'subjects')
                        
                        
                        }
                      else{paste('Please match egg count data.')}  
                        }   
                      }
                    } 
                   }
              }
           else {
# Soil-transmitted helminthiasis
             if (input$NTD=='2')
             {
             data <- read.xlsx(inFile$datapath,1)
             n <- length(data[,1])
             # roundworms
             data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
             data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))
            
             # whipworms
             data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
             data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
             
             # hookworms
             data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
             data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
             
             data$inf <- ifelse(data$Rb > -2 | data$Tb > -2 | data$Hb > -2, 1, 0)
             data$inf2 <- ifelse(data$Rf > -2 | data$Tf > -2 | data$Hf > -2, 1, 0)
             
             if(mean(data$inf)==0 | mean(data$inf2)==0) {paste('Please provide egg count data.')}
             else {
               if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
               {
                 data$RB <-  data[,input$Rbas]  
                 data$TB <-  data[,input$Tbas] 
                 data$HB <-  data[,input$Hbas] 
                 
                 data$RF <-  data[,input$Rfol]  
                 data$TF <-  data[,input$Tfol]
                 data$HF <-  data[,input$Hfol]
                 
                 data$Rp <- ifelse(data$RB>0,1,0) 
                 data$Tp <- ifelse(data$TB>0,1,0)
                 data$Hp <- ifelse(data$HB>0,1,0)
                 
                 data$Rpf <- ifelse(data$RF>=0,1,0) 
                 data$Tpf <- ifelse(data$TF>=0,1,0) 
                 data$Hpf <- ifelse(data$HF>=0,1,0) 
                 
                 data$Rc <- data$Rp + data$Rpf
                 data$Tc <- data$Tp + data$Tpf
                 data$Hc <- data$Hp + data$Hpf
                 data$mix <- data$Rp + data$Tp + data$Hp
                 mix <- dim(subset(data,mix>1))[1]
                 com <- dim(subset(data, Rc==2 | Tc ==2 | Hc ==2))[1]
                 nR <- dim(subset(data, RB>0))[1]
                 nR2 <- dim(subset(data, RB>0 & RF>=0))[1]
                 nT <- dim(subset(data, TB>0))[1]
                 nT2 <- dim(subset(data, TB>0 & TF>=0))[1]
                 nH <- dim(subset(data, HB>0))[1]
                 nH2 <- dim(subset(data, HB>0 & HF>=0))[1]
                 
                 paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ), <em>T. trichiura</em> infections in', nT, 'subjects (', round(100*nT/n,1),'% ) and hookworms in', nH, '(',round (100*nH/n,1),'% ) subjects. Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). Complete data were available for', com, 'subjects, including', nR2, 'cases of <em>A. lumbricoides</em>,', nT2, 'cases of <em>T. trichiura</em>, and',nH2, 'cases of hookworms.')
               }
               else{ 
                 if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
                 {
                   data$RB <-  data[,input$Rbas]  
                   data$TB <-  data[,input$Tbas] 
                   
                   data$RF <-  data[,input$Rfol]  
                   data$TF <-  data[,input$Tfol]
                   
                   data$Rp <- ifelse(data$RB>0,1,0) 
                   data$Tp <- ifelse(data$TB>0,1,0)
                   
                   data$Rf <- ifelse(data$RF>=0,1,0) 
                   data$Tf <- ifelse(data$TF>=0,1,0) 
                   
                   data$Rc <- data$Rp + data$Rf
                   data$Tc <- data$Tp + data$Tf

                   data$mix <- data$Rp + data$Tp
                   mix <- dim(subset(data, mix>1))[1]
                   com <- dim(subset(data, Rc==2 | Tc ==2))[1]
                   nR <- dim(subset(data, RB>0))[1]
                   nR2 <- dim(subset(data, RB>0 & RF>=0))[1]
                   nT <- dim(subset(data, TB>0))[1]
                   nT2 <- dim(subset(data, TB>0 & TF>=0))[1]
                   
                   paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ), <em>T. trichiura</em> infections in', nT, 'subjects (', round(100*nT/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of <em>A. lumbricoides</em>, and ', nT2, 'cases of <em>T. trichiura</em>.')
                  }
                 else{
                   if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
                     {
                     data$RB <-  data[,input$Rbas]  
                     data$HB <-  data[,input$Hbas] 
                     
                     data$RF <-  data[,input$Rfol]  
                     data$HF <-  data[,input$Hfol]
                     
                     data$Rp <- ifelse(data$RB>0,1,0) 
                     data$Hp <- ifelse(data$HB>0,1,0)
                     
                     data$Rf <- ifelse(data$RF>=0,1,0) 
                     data$Hf <- ifelse(data$HF>=0,1,0) 
                     
                     data$Rc <- data$Rp + data$Rf
                     data$Hc <- data$Hp + data$Hf
                     
                     data$mix <- data$Rp + data$Hp
                     mix <- dim(subset(data, mix>1))[1]
                     com <- dim(subset(data, Rc==2 | Hc ==2))[1]
                     nR <- dim(subset(data, RB>0))[1]
                     nR2 <- dim(subset(data, RB>0 & RF>=0))[1]
                     nH <- dim(subset(data, HB>0))[1]
                     nH2 <- dim(subset(data, HB>0 & HF>=0))[1]
                     
                     paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of <em>A. lumbricoides</em>, and ', nH2, 'cases of hookworms.')
                   }
                   else{
                     if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
                       data$TB <-  data[,input$Tbas]  
                       data$HB <-  data[,input$Hbas] 
                       
                       data$TF <-  data[,input$Tfol]  
                       data$HF <-  data[,input$Hfol]
                       
                       data$Tp <- ifelse(data$TB>0,1,0) 
                       data$Hp <- ifelse(data$HB>0,1,0)
                       
                       data$Tf <- ifelse(data$TF>=0,1,0) 
                       data$Hf <- ifelse(data$HF>=0,1,0) 
                       
                       data$Tc <- data$Tp + data$Tf
                       data$Hc <- data$Hp + data$Hf
                       
                       data$mix <- data$Tp + data$Hp
                       mix <- dim(subset(data,mix>1))[1]
                       com <- dim(subset(data, Tc ==2 | Hc ==2))[1]
                       nT <- dim(subset(data, TB>0))[1]
                       nT2 <- dim(subset(data, TB>0 & TF>=0))[1]
                       nH <- dim(subset(data, HB>0))[1]
                       nH2 <- dim(subset(data, HB>0 & HF>=0))[1]
                       
                       paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Trichuris trichiura</em> infections were observed in', nT, 'subjects (', round(100*nT/n,1), '% ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nT2, 'cases of <em>T. trichiura</em>, and ', nH2, 'cases of hookworms.')
                     }
                     else{
                       if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
                         data$RB <-  data[,input$Rbas]  
                         data$RF <-  data[,input$Rfol]  
                         nR <- dim(subset(data, RB>0))[1]
                         nR2 <- dim(subset(data, RB>0 & RF>=0))[1]
                          paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ). In total,', nR2, 'infected subjects provided a sample at both baseline and follow-up.')
                         }
                          else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
                              data$TB <-  data[,input$Tbas]  
                              data$TF <-  data[,input$Tfol]  
                              nT <- dim(subset(data, TB>0))[1]
                              nT2 <- dim(subset(data, TB>0 & TF>=0))[1]
                              paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Trichuris trichiura</em> infections were observed in', nT, 'subjects (', round(100*nT/n,1), '% ). In total,', nT2, 'infected subjects provided a sample at both baseline and follow-up.')
                            }
                            
                            else{
                              if(mean(data$Hb)>-2 & mean(data$Hf)>-2){
                                data$HB <-  data[,input$Hbas]  
                                data$HF <-  data[,input$Hfol]  
                                data$Hp <- ifelse(data$HB>0,1,0) 
                                data$Hf <- ifelse(data$HF>=0,1,0) 
                                data$Hc <- data$Hp + data$Hf
                                nH <- dim(subset(data, HB>0))[1]
                                nH2 <- dim(subset(data, HB>0 & HF>=0))[1]
                                paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. Hookworms infections were observed in', nH, 'subjects (', round(100*nH/n,1), '% ). In total,', nH2, 'infected subjects provided a sample at both baseline and follow-up.')
                              }
                              else{paste('Please match egg counting data.')}
                              }
                          }
                         }  
                       }   
                    }
                  } 
                }
             }
                }
          }
      })
})  

output$number <- renderText({
  inFile <- input$file1
  if (is.null(inFile))
  {paste0(('Please upload data.'))}
  else{number()} })
  

#bastime <- Sys.time() + 18
#check <- eventReactive(input$basedata, {
#  isolate ({   
#  invalidateLater(1000, session)
#  paste("The baseline statistics will be available in ", round(difftime(bastime, Sys.time(), units='secs')), 'secs')
#}) })

#output$countbase <- renderText({
#  check() })


# Step 2: Intensity of infections
int <- eventReactive(input$basedata, {
  
  isolate ({ 
    inFile <- input$file1
    if (is.null(inFile))
    {paste0('Please upload data.')
      } else {if (input$NTD=='1')
    {data <- read.xlsx(inFile$datapath,1)
    n <- length(data[,1])
    # S haematobium
    data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
    data$shF <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
    
    # S mansoni
    data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
    data$smF <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))

    
    # S japonicum
    data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
    data$sjF <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))

    data$inf <- ifelse(data$sh > -2 | data$sm > -2 | data$sj > -2, 1, 0)
    data$inf2 <- ifelse(data$shF > -2 | data$smF > -2 | data$sjF > -2, 1, 0)
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {paste('Please provide egg count data.')}
    else {
      if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$smF)>-2 & mean(data$shF> - 2))
      {
      data$shB <-  data[,input$Shbas]  
      data$smB <-  data[,input$Smbas] 
      data$shF <-  data[,input$Shfol]  
      data$smF <-  data[,input$Smfol] 
      sh <- subset(data, shB >0 &  shF >=0)
      sm <- subset(data, smB >0 &  smF >=0)
      NshH <- sum(ifelse(sh$shB>=50,1,0))
      NshL <- sum(ifelse(sh$shB>0 & sh$shB<50,1,0))
      
      NsmH <- sum(ifelse(sm$smB>=400,1,0))
      NsmM <- sum(ifelse(sm$smB>=100 & sm$smB<400,1,0))
      NsmL <- sum(ifelse(sm$smB>0 & sm$smB<100,1,0))
      q25sh <- round(quantile(sh$shB, probs=c(0.25)),1)
      q75sh <- round(quantile(sh$shB, probs=c(0.75)),1)
      
      q25sm <- round(quantile(sm$smB, probs=c(0.25)),1)
      q75sm <- round(quantile(sm$smB, probs=c(0.75)),1)
     
      Msm <- round(mean(sm$smB),1)
      Msh <- round(mean(sh$shB),1)
      
      nsm <- length(sm$smB)
      nsh <- length(sh$shB)
paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>S. haematobium</em> egg count equaled',Msh,'(',q25sh,';',q75sh,') eggs per 10 ml of urine. The mean 
<em>S. mansoni</em> egg count equaled',Msm,'(',q25sm,';',q75sm,') eggs per gram of stool. Low and highy-intensity <em>S. haematobium</em>  
infections were observed in', NshL,'(',round(100*NshL/nsh,1),'% ) and', NshH, '(',round(100*NshH/nsh,1),'% ) subjects,  respectively. 
      For <em>S. mansoni</em>, the number of low, moderate and high-intensity infections were', NsmL,'(',round(100*NsmL/nsm,1),'% ),', NsmM,
      '(',round(100*NsmM/nsm,1),'% ) and', NsmH, '(',round(100*NsmH/nsm,1),'% ), respectively.')
      }
      else{ 
        if(mean(data$sh)>-2 & mean(data$shF >-2)){
          data$shB <-  data[,input$Shbas] 
          data$shF <-  data[,input$Shfol] 
          sh <- subset(data, shB >0 &  shF >=0)
          NshH <- sum(ifelse(sh$shB>=50,1,0))
          NshL <- sum(ifelse(sh$shB>0 & sh$shB<50,1,0))
          q25sh <- round(quantile(sh$shB, probs=c(0.25)),1)
          q75sh <- round(quantile(sh$shB, probs=c(0.75)),1)
          Msh <- round(mean(sh$shB),1)
          nsh <- length(sh$shB)

          paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) <em>S. haematobium</em> egg count equaled',Msh,'(',q25sh,';',q75sh,') eggs per 10 ml of urine. 
Low and high-intensity <em>S. haematobium</em> infections were observed in', NshL,'(',round(100*NshL/nsh,1),'% ) and', NshH, '(',round(100*NshH/nsh,1),'% ) 
                subjects, respectively.')
        }
        else{
          if(mean(data$sm)>-2 & mean(data$smF)> -2){
            data$smB <-  data[,input$Smbas] 
            data$smF <-  data[,input$Smfol] 
            sm <- subset(data, smB >0 &  smF >=0)
            NsmH <- sum(ifelse(sm$smB>=400,1,0))
            NsmM <- sum(ifelse(sm$smB>=100 & sm$smB<400,1,0))
            NsmL <- sum(ifelse(sm$smB>0 & sm$smB<100,1,0))
            q25sm <- round(quantile(sm$smB, probs=c(0.25)),1)
            q75sm <- round(quantile(sm$smB, probs=c(0.75)),1)
            Msm <- round(mean(sm$smB),1)
            nsm <- length(sm$smB)
            paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) <em>S. mansoni</em> egg count equaled',Msm,'(',q25sm,';',q75sm,') eggs per gram of stool. 
Low, moderate and high-intensity  <em>S. mansoni</em> infections were observed in',NsmL,'(',round(100*NsmL/nsm,1),'% ),', NsmM,'(',round(100*NsmM/nsm,1),'% ) 
                  and', NsmH, '(',round(100*NsmH/nsm,1),'% ) subjects, respectively.')
          }
          else{
            if(mean(data$sj)>-2 & mean(data$sjF) >- 2){
              data$sjB <-  data[,input$Sjbas] 
              data$sjF <-  data[,input$Sjfol] 
              sj <- subset(data, sjB >0 &  sjF >=0)
              NsjH <- sum(ifelse(sj$sjB>=400,1,0))
              NsjM <- sum(ifelse(sj$sjB>=100 & sj$sjB<400,1,0))
              NsjL <- sum(ifelse(sj$sjB>0 & sj$sjB<100,1,0))
              q25sj <- round(quantile(sj$sjB, probs=c(0.25)),1)
              q75sj <- round(quantile(sj$sjB, probs=c(0.75)),1)
              Msj <- round(mean(sj$sjB),1)
              nsj <- length(sj$sjB)
              paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) <em>S. japonicum</em> egg count equaled',Msj,'(',q25sj,';',q75sj,') eggs per gram of stool. Low, moderate and 
high-intensity <em>S. japonicum</em> infections were observed in',NsjL,'(',round(100*NsjL/nsj,1),'% ),', NsjM,'(',round(100*NsjM/nsj,1),'% ) and', NsjH, 
                    '(',round(100*NsjH/nsj,1),'% ) subjects, respectively.')
            }
            else{paste('Please match egg count data.')}  
          }   
        }
      } 
      }
    }
      else {
        # Number of cases of soil-transmitted helminthiasis
        if (input$NTD=='2')
        {
        data <- read.xlsx(inFile$datapath,1)
        n <- length(data[,1])
        # roundworms
        data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
        data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))

        # whipworms
        data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
        data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
        
        # hookworms
        data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
        data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
        
        data$inf <- ifelse(data$Rb > -2 | data$Tb > -2 | data$Hb > -2, 1, 0)
        data$inf2 <- ifelse(data$Rf > -2 | data$Tf > -2 | data$Hf > -2, 1, 0)
        
        
        if(mean(data$inf)==0 | mean(data$inf2)==0) {paste('Please provide egg count data.')}
        else {
          
          if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
          {
            data$RB <-  data[,input$Rbas]  
            data$TB <-  data[,input$Tbas]
            data$HB <-  data[,input$Hbas] 
            data$RF <-  data[,input$Rfol]  
            data$TF <-  data[,input$Tfol] 
            data$HF <-  data[,input$Hfol] 
            R <- subset(data, RB> 0 & RF >= 0)
            Tr <- subset(data, TB> 0 & TF >= 0)
            H <- subset(data, HB> 0 & HF >= 0)
            
            NRH <- sum(ifelse(R$RB>=50000,1,0))
            NRM <- sum(ifelse(R$RB>=5000 & R$RB<50000,1,0))
            NRL <- sum(ifelse(R$RB>0 & R$RB<5000,1,0))
            
            NTH <- sum(ifelse(Tr$TB>=10000,1,0))
            NTM <- sum(ifelse(Tr$TB>=1000 & Tr$TB<10000,1,0))
            NTL <- sum(ifelse(Tr$TB>0 & Tr$TB<1000,1,0)) 
            
            NHH <- sum(ifelse(H$HB>=4000,1,0))
            NHM <- sum(ifelse(H$HB>=2000 & H$HB<4000,1,0))
            NHL <- sum(ifelse(H$HB>0 & H$HB<2000,1,0)) 
            
            q25R <- round(quantile(R$RB, probs=c(0.25)),1)
            q75R <- round(quantile(R$RB, probs=c(0.75)),1) 
            q25T <- round(quantile(Tr$TB, probs=c(0.25)),1)
            q75T <- round(quantile(Tr$TB, probs=c(0.75)),1) 
            q25H <- round(quantile(H$HB, probs=c(0.25)),1)
            q75H <- round(quantile(H$HB, probs=c(0.75)),1)
            
            MR <- round(mean(R$RB),1)
            MT <- round(mean(Tr$TB),1)
            MH <- round(mean(H$HB),1)
            
            nR <- length(R$RB)
            nT <- length(Tr$RB)
            nH <- length(H$HB)
            
            paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean 
<em>T. trichiura</em> egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') 
                  eggs per gram of stool. Low, moderate and high-intensity <em>A. lumbricoides</em> infections were observed in', NRL,'(',round(100*NRL/nR,1),'% ),'
                  , NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects,  respectively.
            For <em>T. trichiura</em>, these numbers were', NTL,'(',round(100*NTL/nT,1),'% ),', NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ),
                  respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'% ),', NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/n,1),
                  '% ), respectively.')
          }
          
          
          else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
            {
              data$RB <-  data[,input$Rbas]  
              data$TB <-  data[,input$Tbas]
              data$RF <-  data[,input$Rfol]  
              data$TF <-  data[,input$Tfol] 
              R <- subset(data, RB>0 & RF >= 0)
              Tr <- subset(data, TB> 0 & TF >= 0)
              
              NRH <- sum(ifelse(R$RB>=50000,1,0))
              NRM <- sum(ifelse(R$RB>=5000 & R$RB<50000,1,0))
              NRL <- sum(ifelse(R$RB>0 & R$RB<5000,1,0))
              
              NTH <- sum(ifelse(Tr$TB>=10000,1,0))
              NTM <- sum(ifelse(Tr$TB>=1000 & Tr$TB<10000,1,0))
              NTL <- sum(ifelse(Tr$TB>0 & Tr$TB<1000,1,0)) 
          
              q25R <- round(quantile(R$RB, probs=c(0.25)),1)
              q75R <- round(quantile(R$RB, probs=c(0.75)),1) 
              q25T <- round(quantile(Tr$TB, probs=c(0.25)),1)
              q75T <- round(quantile(Tr$TB, probs=c(0.75)),1) 
      
              MR <- round(mean(R$RB),1)
              MT <- round(mean(Tr$TB),1)

              
              nR <- length(R$RB)
              nT <- length(Tr$TB)

              
              paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean <em>T. trichiura</em>
egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity <em>A. lumbricoides</em> infections were observed 
                    in', NRL,'(',round(100*NRL/nR,1),'% ),' , NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects,  respectively.
            For <em>T. trichiura</em>, these numbers were', NTL,'(',round(100*NTL/nT,1),'% ),', NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ),
                    respectively.')
            }
            
            else{
              if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
              {
                data$RB <-  data[,input$Rbas]  
                data$HB <-  data[,input$Hbas] 
                data$RF <-  data[,input$Rfol]  
                data$HF <-  data[,input$Hfol] 
                R <- subset(data, RB>0 & RF >= 0)
                H <- subset(data, HB>0 & HF >= 0)
                
                NRH <- sum(ifelse(R$RB>=50000,1,0))
                NRM <- sum(ifelse(R$RB>=5000 & R$RB<50000,1,0))
                NRL <- sum(ifelse(R$RB>0 & R$RB<5000,1,0))
            
                NHH <- sum(ifelse(H$HB>=4000,1,0))
                NHM <- sum(ifelse(H$HB>=2000 & H$HB<4000,1,0))
                NHL <- sum(ifelse(H$HB>0 & H$HB<2000,1,0)) 
                
                q25R <- round(quantile(R$RB, probs=c(0.25)),1)
                q75R <- round(quantile(R$RB, probs=c(0.75)),1) 
                q25H <- round(quantile(H$HB, probs=c(0.25)),1)
                q75H <- round(quantile(H$HB, probs=c(0.75)),1)
                
                MR <- round(mean(R$RB),1)
                MH <- round(mean(H$HB),1)
                
                nR <- length(R$RB)
                nH <- length(H$HB)
                
                paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. 
                      The mean hookworm egg counts equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. 
                      Low, moderate and high - intensity <em>A. lumbricoides</em> infections were observed in', NRL,'(',round(100*NRL/nR,1),'% ),', 
NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects, respectively. 
                      For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'% ),', NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/n,1),'% ),
                      respectively.')
              }
              
              
              else{
                if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
                {
                   data$TB <-  data[,input$Tbas]
                  data$HB <-  data[,input$Hbas] 
                   data$TF <-  data[,input$Tfol] 
                  data$HF <-  data[,input$Hfol] 
                  Tr <- subset(data, TB> 0 & TF >= 0)
                  H <- subset(data, HB>0 & HF >= 0)
                  
                  NTH <- sum(ifelse(Tr$TB>=10000,1,0))
                  NTM <- sum(ifelse(Tr$TB>=1000 & Tr$TB<10000,1,0))
                  NTL <- sum(ifelse(Tr$TB>0 & Tr$TB<1000,1,0)) 
                  
                  NHH <- sum(ifelse(H$HB>=4000,1,0))
                  NHM <- sum(ifelse(H$HB>=2000 & H$HB<4000,1,0))
                  NHL <- sum(ifelse(H$HB>0 & H$HB<2000,1,0)) 
                  
                  q25T <- round(quantile(Tr$TB, probs=c(0.25)),1)
                  q75T <- round(quantile(Tr$TB, probs=c(0.75)),1) 
                  q25H <- round(quantile(H$HB, probs=c(0.25)),1)
                  q75H <- round(quantile(H$HB, probs=c(0.75)),1)
                  
                  MT <- round(mean(Tr$TB),1)
                  MH <- round(mean(H$HB),1)
                  
                  nT <- length(Tr$TB)
                  nH <- length(H$HB)
                  
                  paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>T. trichiura</em> egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. 
                        The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensity <em>T. trichiura</em>
                        infections were observed in', NTL,'(',round(100*NTL/nT,1),'% ),', NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ) subjects, 
                        respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'% ),', NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/nH,1),'% ), respectively.')
                  }
                else{
                  if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
                  {
                    data$RB <-  data[,input$Rbas]  
                    data$RF <-  data[,input$Rfol]  
                    R <- subset(data, RB>0 & RF >= 0)
                    NRH <- sum(ifelse(R$RB>=50000,1,0))
                    NRM <- sum(ifelse(R$RB>=5000 & R$RB<50000,1,0))
                    NRL <- sum(ifelse(R$RB>0 & R$RB<5000,1,0))
                    q25R <- round(quantile(R$RB, probs=c(0.25)),1)
                    q75R <- round(quantile(R$RB, probs=c(0.75)),1) 
                    MR <- round(mean(R$RB),1)
                    nR <- length(R$RB)
                    paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NRL,'(',round(100*NRL/nR,1),'% ),'
                          , NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects, respectively.')
                  }
                  
                  else{
                    if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                    {
                      data$TB <-  data[,input$Tbas]
                      data$TF <-  data[,input$Tfol] 
                      Tr <- subset(data, TB> 0 & TF >= 0)
                      NTH <- sum(ifelse(Tr$TB>=5000,1,0))
                      NTM <- sum(ifelse(Tr$TB>=1000 & Tr$TB<5000,1,0))
                      NTL <- sum(ifelse(Tr$TB>=1 & Tr$TB<1000,1,0)) 
                      q25T <- round(quantile(Tr$TB, probs=c(0.25)),1)
                      q75T <- round(quantile(Tr$TB, probs=c(0.75)),1) 
                      MT <- round(mean(Tr$TB),1)
                      nT <- length(Tr$TB)
                      paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
<em>T. trichiura</em> egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NTL,'(',round(100*NTL/nT,1),'% ),'
                            , NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ) subjects,  respectively.')
                    }
                    
                    else{
                      if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                      {
                        data$HB <-  data[,input$Hbas] 
                        data$HF <-  data[,input$Hfol] 
                        H <- subset(data, HB> 0 & HF >= 0)
                        NHH <- sum(ifelse(H$HB>=4000,1,0))
                        NHM <- sum(ifelse(H$HB>=2000 & H$HB<4000,1,0))
                        NHL <- sum(ifelse(H$HB>=1 & H$HB<2000,1,0)) 
                        
                        q25H <- round(quantile(H$HB, probs=c(0.25)),1)
                        q75H <- round(quantile(H$HB, probs=c(0.75)),1)
                        
                        MH <- round(mean(H$HB),1)
                        nH <- length(H$HB)
                        
                        paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensty infections were observed in', NHL,'(',round(100*NHL/nH,1),'% ),'
                              , NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/nH,1),'% ) subjects,  respectively.')
                      }
                      
                      else{paste('Please match egg counting data.')}
                    }
                  }
                }  
              }   
            }
          } 
          }
        }
    }
  } 
})
  
})  
output$int <- renderText({  
  inFile <- input$file1
  if (is.null(inFile))
  {paste0(('Please upload data.'))}
  else{int()} })

### Distribution of egg counts
distegg <- eventReactive(input$basedata, {
    isolate ({    
    inFile <- input$file1
    if (is.null(inFile))
    {paste('Please upload data.') 
      } else {if (input$NTD=='1')
      {data <- read.xlsx(inFile$datapath,1) 
      n <- length(data[,1])
      data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
      data$shf <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
       
      # S mansoni
      data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
      data$smf <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))
      
      # S japonicum
      data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
      data$sjf <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))
     
       data$inf <- ifelse(data$sh > -2 | data$sm > -2 | data$sj > -2, 1, 0)
       data$inf2 <- ifelse(data$shf > -2 | data$smf > -2 | data$sjf > -2, 1, 0)
        if(mean(data$inf)==0 |mean(data$inf2)==0) {}
      else 
        {if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$smf)>-2 & mean(data$shf> - 2)){
          data$shB <-  data[,input$Shbas]  
          data$smB <-  data[,input$Smbas] 
          data$shF <-  data[,input$Shfol]  
          data$smF <-  data[,input$Smfol] 
          sh <- subset(data, shB >0 &  shF >=0)
          sm <- subset(data, smB >0 &  smF >=0)
          par(mfrow=c(1,2))
          hist(sh$shB, col = "#EB4C4C", main=expression(italic(Schistosoma~haematobium)), freq=T,ylab = 'Number of subjects', xlab='Urine egg counts (eggs per 10 ml)')
          hist(sm$smB, col = "#EB4C4C", main=expression(italic(Schistosoma~mansoni)), freq=T, ylab='Number of subjects', xlab='Fecal egg counts (eggs per gram of stool)')}
          else
            {if(mean(data$sh)>-2 & mean(data$shf> - 2)){
              data$shB <-  data[,input$Shbas]  
              data$shF <-  data[,input$Shfol]  
              sh <- subset(data, shB >0 &  shF >=0)
              hist(sh$shB, col = "#EB4C4C", main=expression(italic(Schistosoma~haematobium)), freq=T, ylab='Number of subjects',xlab='Urine egg counts (eggs per 10 ml)')}
              else 
              {if(mean(data$sm)>-2 & mean(data$smf)>-2) {
                data$smB <-  data[,input$Smbas] 
                data$smF <-  data[,input$Smfol] 
                sm <- subset(data, smB >0 &  smF >=0)
                hist(sm$smB, col = "#EB4C4C", main=expression(italic(Schistosoma~mansoni)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')}
              else
                {if(mean(data$sj)>-2 & mean(data$sjf)>-2) {
                  data$sjB <-  data[,input$Sjbas]  
                  data$sjF <-  data[,input$Sjfol]  
                  sj <- subset(data, sjB >0 &  sjF >=0)
                hist(sj$sjB, col = "#EB4C4C", main=expression(italic(Schistosoma~japonicum)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')}
                else  {  }
                }
              }
            }
        }
    }
    else  {if (input$NTD=='2'){
      data <- read.xlsx(inFile$datapath,1)
      n <- length(data[,1])
      # roundworms
      data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
      data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))
      
      # whipworms
      data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
      data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
 
      # hookworms
      data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
      data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
      
      data$inf <- ifelse(data$Rb > -2 | data$Tb > -2 | data$Hb > -2, 1, 0)
      data$inf2 <- ifelse(data$Rf > -2 | data$Tf > -2 | data$Hf > -2, 1, 0)
      if(mean(data$inf)==0 | mean(data$inf2)==0) {}
      else
        {if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
        {
        data$RB <-  data[,input$Rbas]  
        data$TB <-  data[,input$Tbas]
        data$HB <-  data[,input$Hbas] 
        data$RF <-  data[,input$Rfol]  
        data$TF <-  data[,input$Tfol] 
        data$HF <-  data[,input$Hfol] 
        R <- subset(data, RB> 0 & RF >= 0)
        Tr <- subset(data, TB> 0 & TF >= 0)
        H <- subset(data, HB> 0 & HF >= 0)
        
        par(mfrow=c(1,3))
        hist(R$RB, col = "#EB4C4C", main=expression(italic(Ascaris~lumbricoides)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
        hist(Tr$TB, col = "#EB4C4C", main=expression(italic(Trichuris~trichiura)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')
        hist(H$HB, col = "#EB4C4C", main='Hookworm', freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')
        }
      else{ 
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
        {
          data$RB <-  data[,input$Rbas]  
          data$TB <-  data[,input$Tbas]
          data$RF <-  data[,input$Rfol]  
          data$TF <-  data[,input$Tfol] 
          R <- subset(data, RB> 0 & RF >= 0)
          Tr <- subset(data, TB> 0 & TF >= 0)
          
          par(mfrow=c(1,2))
          hist(R$RB, col = "#EB4C4C", main=expression(italic(Ascaris~lumbricoides)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')
          hist(Tr$TB, col = "#EB4C4C", main=expression(italic(Trichuris~trichiura)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')
        }
        else{  
          if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
          {
            data$RB <-  data[,input$Rbas]  
            data$HB <-  data[,input$Hbas] 
            data$RF <-  data[,input$Rfol]  
            data$HF <-  data[,input$Hfol] 
            R <- subset(data, RB> 0 & RF >= 0)
            H <- subset(data, HB> 0 & HF >= 0)
            
            par(mfrow=c(1,2))
            hist(R$RB, col = "#EB4C4C", main=expression(italic(Ascaris~lumbricoides)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
            hist(H$HB, col ="#EB4C4C", main='Hookworm', freq=T, ylab='Number of subjects', xlab='Fecal egg counts (eggs per gram of stool)')
          }
          else{  
            if(mean(data$Hb)>-2 & mean(data$Tb)>-2 & mean(data$Hf)>-2 & mean(data$Tf)>-2)
            {
              data$TB <-  data[,input$Tbas]
              data$HB <-  data[,input$Hbas] 
              data$TF <-  data[,input$Tfol] 
              data$HF <-  data[,input$Hfol] 
              Tr <- subset(data, TB> 0 & TF >= 0)
              H <- subset(data, HB> 0 & HF >= 0)
              
              par(mfrow=c(1,2))
              hist(Tr$TB, col = "#EB4C4C", main=expression(italic(Trichuris~trichiura)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
              hist(H$HB, col = "#EB4C4C", main='Hookworm', freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')
            }
            else{  
              if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
              {
                data$RB <-  data[,input$Rbas]  
                data$RF <-  data[,input$Rfol]  
                R <- subset(data, RB> 0 & RF >= 0)
                 
                par(mfrow=c(1,1))
                hist(R$RB, col = "#EB4C4C", main=expression(italic(Ascaris~lumbricoides)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
              }
              else{  
                if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                {
                  data$TB <-  data[,input$Tbas]
                  data$TF <-  data[,input$Tfol] 
                  Tr <- subset(data, TB> 0 & TF >= 0)
                  par(mfrow=c(1,1))
                  hist(Tr$TB, col = "#EB4C4C", main=expression(italic(Trichuris~trichiura)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
                }
                else{  
                  if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                  {
                    data$HB <-  data[,input$Hbas] 
                    data$HF <-  data[,input$Hfol] 
                    H <- subset(data, HB> 0 & HF >= 0)
                    
                    par(mfrow=c(1,1))
                    hist(H$HB, col = "#EB4C4C", main='Hookworm', ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
                  }
                  else{}
                }
              }
            }  
          }   
        }
      } 
        }
    }
    }
    } 
    })
  
})  
output$distegg <- renderPlot({distegg()})



##### Follow-up
followup <- eventReactive(input$basedata, {
  
  isolate ({ 
    inFile <- input$file1
    if (is.null(inFile))
    {paste0('Please upload data.')
      } else {if (input$NTD=='1')
    {data <- read.xlsx(inFile$datapath,1)
    n <- length(data[,1])
    # S haematobium
    data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
    data$shF <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
    
    # S mansoni
    data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
    data$smF <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))
    
    
    # S japonicum
    data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
    data$sjF <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))
    
   data$FU <- ifelse(input$followup=='Not recorded',rep(-2,n), ifelse(data[,input$followup]>=0,1,0))
  
    if(mean(data$FU)==-2) {'Please match follow-up period data'}
    else {
      if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$smF)>-2 & mean(data$shF> - 2) & mean(data$FU>-2))
      {
        data$fol <-  data[,input$followup] 
        data$shB <-  data[,input$Shbas]  
        data$smB <-  data[,input$Smbas] 
        data$shF2 <-  data[,input$Shfol]  
        data$smF2 <-  data[,input$Smfol] 
        data$shp <- ifelse(data$shB>0,1,0) 
        data$smp <- ifelse(data$smB>0,1,0)
        data$shf <- ifelse(data$shF2>=0,1,0) 
        data$smf <- ifelse(data$smF2>=0,1,0) 
        data$shc <- data$shp + data$shf
        data$smc <- data$smp + data$smf
        data2 <- subset(data, data$shc == 2 | data$smc == 2)
        min <- round(quantile(data2$fol, probs=c(0)),1)
        max <- round(quantile(data2$fol, probs=c(1)),1)
        med <- round(quantile(data2$fol, probs=c(0.50)),1)
        nc <- length(data2$fol)
        ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
        paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')
      }
      else{ 
        if(mean(data$sh)>-2 & mean(data$shF >-2) & mean(data$FU>-2)){
          data$fol <-  data[,input$followup] 
          data$shB <-  data[,input$Shbas]  
          data$shF2 <-  data[,input$Shfol]  
          data$shp <- ifelse(data$shB>0,1,0) 
          data$shf <- ifelse(data$shF2>=0,1,0) 
          data$shc <- data$shp + data$shf
          data2 <- subset(data, data$shc == 2)
          min <- round(min(data2$fol),1)
          max <- round(max(data2$fol),1)
          med <- round(quantile(data2$fol, probs=c(0.50)),1)
          nc <- length(data2$fol)
          ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
          paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')
        }
        else{
          if(mean(data$sm)>-2 & mean(data$smF)> -2 & mean(data$FU>-2)){
            data$fol <-  data[,input$followup]  
            data$smB <-  data[,input$Smbas] 
            data$smF2 <-  data[,input$Smfol] 
            data$smp <- ifelse(data$smB>0,1,0)
            data$smf <- ifelse(data$smF2>=0,1,0) 
            data$smc <- data$smp + data$smf
            data2 <- subset(data, data$smc == 2)
            min <- round(min(data2$fol),1)
            max <- round(max(data2$fol),1)
            med <- round(quantile(data2$fol, probs=c(0.50)),1)
            nc <- length(data2$fol)
            ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
            paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')    }
          
          else{
            if(mean(data$sj)>-2 & mean(data$sjF) >- 2 & mean(data$FU>-2)){
              data$fol <-  data[,input$followup] 
              data$sjB <-  data[,input$Sjbas]  
              data$sjF2 <-  data[,input$Sjfol] 
              data$sjp <- ifelse(data$sjB>0,1,0)
              data$sjf <- ifelse(data$sjF2>=0,1,0) 
              data$sjc <- data$sjp + data$sjf
              data2 <- subset(data, data$sjc == 2)
              min <- round(min(data2$fol),1)
              max <- round(max(data2$fol),1)
              med <- round(quantile(data2$fol, probs=c(0.50)),1)
              nc <- length(data2$fol)
              ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
              paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')
            }
            else{paste('Please match egg count data.')}  
          }   
          }
          } 
        }
    }
      else {
        # Number of cases of soil-transmitted helminthiasis
        if (input$NTD=='2')
        {
          data <- read.xlsx(inFile$datapath,1)
          n <- length(data[,1])
          # roundworms
          data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
          data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))
          
          # whipworms
          data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
          data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
          
          # hookworms
          data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
          data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
          data$FU <- ifelse(input$followup=='Not recorded',rep(-2,n), ifelse(data[,input$followup]>=0,1,0))
          
          if(mean(data$FU)==-2) {'Please match follow-up period data'}
          else {
            
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2))
            {
              data$fol <-  data[,input$followup] 
              data$RB <-  data[,input$Rbas]  
              data$TB <-  data[,input$Tbas] 
              data$HB <-  data[,input$Hbas] 
              
              data$RF <-  data[,input$Rfol]  
              data$TF <-  data[,input$Tfol]
              data$HF <-  data[,input$Hfol]
              
              data$Rp <- ifelse(data$RB>0,1,0) 
              data$Tp <- ifelse(data$TB>0,1,0)
              data$Hp <- ifelse(data$HB>0,1,0)
              
              data$Rf <- ifelse(data$RF>=0,1,0) 
              data$Tf <- ifelse(data$TF>=0,1,0) 
              data$Hf <- ifelse(data$HF>=0,1,0) 
              
              data$Rc <- data$Rp + data$Rf
              data$Tc <- data$Tp + data$Tf
              data$Hc <- data$Hp + data$Hf
              
              data2 <- subset(data, Rc==2 | Tc ==2 | Hc ==2)
              min <- round(quantile(data2$fol, probs=c(0)),1)
              max <- round(quantile(data2$fol, probs=c(1)),1)
              med <- round(quantile(data2$fol, probs=c(0.50)),1)
              nc <- length(data2$fol)
              ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
              paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
            }
            
            
            else{ 
              if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$FU>-2))
              {
                data$fol <-  data[,input$followup] 
                data$RB <-  data[,input$Rbas]  
                data$TB <-  data[,input$Tbas] 
                data$RF <-  data[,input$Rfol]  
                data$TF <-  data[,input$Tfol]
                data$Rp <- ifelse(data$RB>0,1,0) 
                data$Tp <- ifelse(data$TB>0,1,0)
                data$Rf <- ifelse(data$RF>=0,1,0) 
                data$Tf <- ifelse(data$TF>=0,1,0) 
                data$Rc <- data$Rp + data$Rf
                data$Tc <- data$Tp + data$Tf
                
                data2 <- subset(data, Rc==2 | Tc ==2)
                min <- round(quantile(data2$fol, probs=c(0)),1)
                max <- round(quantile(data2$fol, probs=c(1)),1)
                med <- round(quantile(data2$fol, probs=c(0.50)),1)
                nc <- length(data2$fol)
                ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                
              }
              
              else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2))
                {
                  
                  data$RB <-  data[,input$Rbas]  
                  data$HB <-  data[,input$Hbas] 
                  
                  data$RF <-  data[,input$Rfol]  
                  data$HF <-  data[,input$Hfol]
                  
                  data$Rp <- ifelse(data$RB>0,1,0) 
                  data$Hp <- ifelse(data$HB>0,1,0)
                  
                  data$Rf <- ifelse(data$RF>=0,1,0) 
                  data$Hf <- ifelse(data$HF>=0,1,0) 
                  
                  data$Rc <- data$Rp + data$Rf
                  data$Hc <- data$Hp + data$Hf
                  data$fol <-  data[,input$followup] 
                  data2 <- subset(data, Rc==2 | Hc ==2)
                  min <- round(quantile(data2$fol, probs=c(0)),1)
                  max <- round(quantile(data2$fol, probs=c(1)),1)
                  med <- round(quantile(data2$fol, probs=c(0.50)),1)
                  nc <- length(data2$fol)
                  ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                  
                  paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                  
                }
                
                else{
                  if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2))
                  {
                    data$fol <-  data[,input$followup] 
                    data$TB <-  data[,input$Tbas] 
                    data$HB <-  data[,input$Hbas] 
                    
                    data$TF <-  data[,input$Tfol]
                    data$HF <-  data[,input$Hfol]
                    
                    data$Tp <- ifelse(data$TB>0,1,0)
                    data$Hp <- ifelse(data$HB>0,1,0)
                    
                    data$Tf <- ifelse(data$TF>=0,1,0) 
                    data$Hf <- ifelse(data$HF>=0,1,0) 
                    
                    data$Tc <- data$Tp + data$Tf
                    data$Hc <- data$Hp + data$Hf
                    
                    data2 <- subset(data, Tc ==2 | Hc ==2)
                    min <- round(quantile(data2$fol, probs=c(0)),1)
                    max <- round(quantile(data2$fol, probs=c(1)),1)
                    med <- round(quantile(data2$fol, probs=c(0.50)),1)
                    nc <- length(data2$fol)
                    ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                    paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                  }
                  else{
                    if(mean(data$Rb)>-2 & mean(data$Rf)>-2 & mean(data$FU>-2))
                    {
                      data$fol <-  data[,input$followup] 
                      data$RB <-  data[,input$Rbas]  
                      data$RF <-  data[,input$Rfol]  
                      data$Rp <- ifelse(data$RB>0,1,0) 
                      data$Rf <- ifelse(data$RF>=0,1,0) 
                      data$Rc <- data$Rp + data$Rf
                      data2 <- subset(data, Rc==2)
                      min <- round(quantile(data2$fol, probs=c(0)),1)
                      max <- round(quantile(data2$fol, probs=c(1)),1)
                      med <- round(quantile(data2$fol, probs=c(0.50)),1)
                      nc <- length(data2$fol)
                      ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                      paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                    }
                    
                    else{
                      if(mean(data$Tb)>-2 & mean(data$Tf)>-2 & mean(data$FU>-2))
                      {
                        data$fol <-  data[,input$followup] 
                        data$TB <-  data[,input$Tbas] 
                        data$TF <-  data[,input$Tfol]
                        data$Tp <- ifelse(data$TB>0,1,0)
                        data$Tf <- ifelse(data$TF>=0,1,0) 
                        data$Tc <- data$Tp + data$Tf
                        data2 <- subset(data,Tc ==2)
                        min <- round(quantile(data2$fol, probs=c(0)),1)
                        max <- round(quantile(data2$fol, probs=c(1)),1)
                        med <- round(quantile(data2$fol, probs=c(0.50)),1)
                        nc <- length(data2$fol)
                        ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                        paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')   }
                      
                      else{
                        if(mean(data$Hb)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2))
                        {
                          data$fol <-  data[,input$followup] 
                          data$HB <-  data[,input$Hbas] 
                          data$HF <-  data[,input$Hfol]
                          data$Hp <- ifelse(data$HB>0,1,0)
                          data$Hf <- ifelse(data$HF>=0,1,0) 
                          data$Hc <- data$Hp + data$Hf
                          
                          data2 <- subset(data, Hc ==2)
                          min <- round(quantile(data2$fol, probs=c(0)),1)
                          max <- round(quantile(data2$fol, probs=c(1)),1)
                          med <- round(quantile(data2$fol, probs=c(0.50)),1)
                          nc <- length(data2$fol)
                          ncont <- sum(ifelse(data2$fol>=7 & data2$fol<=21,1,0))
                          paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.') 
                        }
                        
                        else{paste('Please match egg counting data.')}
                      }
                    }
                    }  
                  }   
                }
              } 
      }
        }
    }
} 
    })
  
  })  


output$followup <- renderText({
  inFile <- input$file1
  if (is.null(inFile))
  {paste0(('Please upload data.'))}
  else{followup()} 
  })

#### Egg reduction rate
err <- eventReactive(input$eggreducrate, {
    isolate ({ 
    inFile <- input$file1
    if (is.null(inFile))
    {paste('Please upload data.')
      } else  {if (input$NTD=='1')
    {data <- read.xlsx(inFile$datapath,1)
    data$n <- 1
    n <- length(data[,1])
    # S haematobium
    data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
    data$shf <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
 
       # S mansoni
    data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
    data$smf <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))
    
    # S japonicum
    data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
    data$sjf <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))
   
     data$inf <- ifelse(data$sh > -2 | data$sm > -2 | data$sj > -2, 1, 0)
    data$inf2 <- ifelse(data$shf > -2 | data$smf > -2 | data$sjf > -2, 1, 0)
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {paste('Please provide egg count data.')}
    else {
      if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$shf)>-2 & mean(data$smf)>-2)
      {
        data$shB <-  data[,input$Shbas]  
        data$smB <-  data[,input$Smbas] 
        data$shF <-  data[,input$Shfol]  
        data$smF <-  data[,input$Smfol] 
        sh <- subset(data, shB >0 &  shF >=0)
        sm <- subset(data, smB >0 &  smF >=0)
        ERRSH <- (1- mean(sh$shF)/mean(sh$shB))
        term1SH <- (mean(sh$shF)/mean(sh$shB))**2; term2SH <- ifelse(mean(sh$shF)==0,0,var(sh$shF)/mean(sh$shF)**2); term3SH <- var(sh$shB)/mean(sh$shB)**2
        term4SH <- ifelse(mean(sh$shF)==0,0,-2*cor(sh$shB,sh$shF)*sqrt(var(sh$shF))*sqrt(var(sh$shB))/(mean(sh$shB)*mean(sh$shF)))
        VARSH <-  term1SH*(term2SH+term3SH+term4SH); varSH <- VARSH / length(sh$shB)
        aSH <- ((1-ERRSH)**2)/varSH; bSH <- varSH/(1-ERRSH)
        ULSH <- 1-qgamma(0.025,shape = aSH, scale = bSH)
        LLSH <- 1-qgamma(0.975,shape = aSH, scale = bSH)
      
        ERRSM <- (1- mean(sm$smF)/mean(sm$smB))
        term1SM <- (mean(sm$smF)/mean(sm$smB))**2; term2SM <- ifelse(mean(sm$smF)==0,0,var(sm$smF)/mean(sm$smF)**2); term3SM <- var(sm$smB)/mean(sm$smB)**2
        term4SM <- ifelse(mean(sm$smF)==0,0,-2*cor(sm$smB,sm$smF)*sqrt(var(sm$smF))*sqrt(var(sm$smB))/(mean(sm$smB)*mean(sm$smF)))
        VARSM <-  term1SM*(term2SM+term3SM+term4SM); varSM <- VARSM / length(sm$smB)
        aSM <- ((1-ERRSM)**2)/varSM; bSM <- varSM/(1-ERRSM)
        ULSM <- 1-qgamma(0.025,shape = aSM, scale = bSM)
        LLSM <- 1-qgamma(0.975,shape = aSM, scale = bSM)
        
        if(input$Sdrug == 1)  {
        paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>S. haematobium</em> equaled',round(100*ERRSH,1),
'% (',round(100*LLSH,1), ';',round(100*ULSH,1), '). For <em>S. mansoni</em>, the ERR equaled',round(100*ERRSM,1),'% 
(',round(100*LLSM,1),';', round(100*ULSM,1),'). The figures below classify the ERR estimates according to WHO criteria (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
        }else {
          paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>S. haematobium</em> equaled',round(100*ERRSH,1),'%
(',round(100*LLSH,1), ';',round(100*ULSH,1), '). For <em>S. mansoni</em>, the ERR equaled',round(100*ERRSM,1),'% (',round(100*LLSM,1),';', round(100*ULSM,1),'). 
The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
          
        }
      } else {
        if(mean(data$sh)>-2 & mean(data$shf)>-2){
          data$shB <-  data[,input$Shbas]  
          data$shF <-  data[,input$Shfol]  
          sh <- subset(data, shB >0 &  shF >=0)
          ERRSH <- (1- mean(sh$shF)/mean(sh$shB))
          term1SH <- (mean(sh$shF)/mean(sh$shB))**2; term2SH <- ifelse(mean(sh$shF)==0,0,var(sh$shF)/mean(sh$shF)**2); term3SH <- var(sh$shB)/mean(sh$shB)**2
          term4SH <- ifelse(mean(sh$shF)==0,0,-2*cor(sh$shB,sh$shF)*sqrt(var(sh$shF))*sqrt(var(sh$shB))/(mean(sh$shB)*mean(sh$shF)))
          VARSH <-  term1SH*(term2SH+term3SH+term4SH); varSH <- VARSH / length(sh$shB)
          aSH <- ((1-ERRSH)**2)/varSH; bSH <- varSH/(1-ERRSH)
          ULSH <- 1-qgamma(0.025,shape = aSH, scale = bSH)
          LLSH <- 1-qgamma(0.975,shape = aSH, scale = bSH)
          
          if(input$Sdrug == 1){
          paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>S. haematobium</em> equaled',round(100*ERRSH,1),'%
(',round(100*LLSH,1), ';',round(100*ULSH,1),'). The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
                and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
            } else {
            paste('The egg reduction rate (95% confidence intervals) of the drug against <em>S. haematobium</em> equaled',round(100*ERRSH,1),
                  '% (',round(100*LLSH,1), ';',round(100*ULSH,1), ').
                  The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
          }
          
            } else {
          if(mean(data$sm)>-2 & mean(data$smf)>-2){
            data$smB <-  data[,input$Smbas] 
            data$smF <-  data[,input$Smfol] 
            sm <- subset(data, smB >0 &  smF >=0)
      
            ERRSM <- (1- mean(sm$smF)/mean(sm$smB))
            term1SM <- (mean(sm$smF)/mean(sm$smB))**2; term2SM <- ifelse(mean(sm$smF)==0,0,var(sm$smF)/mean(sm$smF)**2); term3SM <- var(sm$smB)/mean(sm$smB)**2
            term4SM <- ifelse(mean(sm$smF)==0,0,-2*cor(sm$smB,sm$smF)*sqrt(var(sm$smF))*sqrt(var(sm$smB))/(mean(sm$smB)*mean(sm$smF)))
            VARSM <-  term1SM*(term2SM+term3SM+term4SM); varSM <- VARSM / length(sm$smB)
            aSM <- ((1-ERRSM)**2)/varSM; bSM <- varSM/(1-ERRSM)
            ULSM <- 1-qgamma(0.025,shape = aSM, scale = bSM)
            LLSM <- 1-qgamma(0.975,shape = aSM, scale = bSM)
            
            if(input$Sdrug == 1){ 
            paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>S. mansoni</em> equaled',round(100*ERRSM,1),'% (',round(100*LLSM,1), ';',
round(100*ULSM,1),'). The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
              } else {
              paste('The egg reduction rate (95% confidence intervals) of the drug against <em>S. mansoni</em> equaled',round(100*ERRSM,1),
                    '% (',round(100*LLSM,1),';', round(100*ULSM,1),'). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
              
            }
            
            } else {
            if(mean(data$sj)>-2 & mean(data$sjf>-2)){
              data$sjB <-  data[,input$Sjbas] 
              data$sjF <-  data[,input$Sjfol] 
              sj <- subset(data, sjB >0 &  sjF >=0)
              ERRSJ <- (1- mean(sj$sjF)/mean(sj$sjB))
              term1SJ <- (mean(sj$sjF)/mean(sj$sjB))**2; term2SJ <- ifelse(mean(sj$sjF)==0,0,var(sj$sjF)/mean(sj$sjF)**2); term3SJ <- var(sj$sjB)/mean(sj$sjB)**2              
              term4SJ <- ifelse(mean(sj$sjF)==0,0,-2*cor(sj$sjB,sj$sjF)*sqrt(var(sj$sjF))*sqrt(var(sj$sjB))/(mean(sj$sjB)*mean(sj$sjF)))
              VARSJ <-  term1SJ*(term2SJ+term3SJ+term4SJ); varSJ <- VARSJ / length(sj$sjB)
              aSJ <- ((1-ERRSJ)**2)/varSJ; bSJ <- varSJ/(1-ERRSJ)
              ULSJ <- 1-qgamma(0.025,shape = aSJ, scale = bSJ)
              LLSJ <- 1-qgamma(0.975,shape = aSJ, scale = bSJ)
              if(input$Sdrug == 1){
                paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>S. japonicum</em> equaled',
round(100*ERRSJ,1),'% (',round(100*LLSJ,1), ';',round(100*ULSJ,1),'). The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')} else {
              paste('The egg reduction rate (95% confidence intervals) of the drug against <em>S. japonicum</em> 
equaled',round(100*ERRSJ,1),'% (',round(100*LLSJ,1), ';',round(100*ULSJ,1),').
               The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
               }
              
              } else{paste('Please match egg count data.')}  
          }   
        }
      } 
      }
    }
      else {
        # Soil-transmitted helminthiasis
        if (input$NTD=='2')
        {
          data <- read.xlsx(inFile$datapath,1)
          n <- length(data[,1])
          # roundworms
          data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
          data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))
          
          # whipworms
          data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
          data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
          
          # hookworms
          data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
          data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
          
          data$inf <- ifelse(data$Rb > -2 | data$Tb > -2 | data$Hb > -2, 1, 0)
          data$inf2 <- ifelse(data$Rf > -2 | data$Tf > -2 | data$Hf > -2, 1, 0)
          
        if(mean(data$inf)==0 | mean(data$inf2)==0 ) {paste('Please provide egg count data.')}
        else {
          if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
          {
            data$RB <-  data[,input$Rbas]  
            data$TB <-  data[,input$Tbas]
            data$HB <-  data[,input$Hbas] 
            data$RF <-  data[,input$Rfol]  
            data$TF <-  data[,input$Tfol] 
            data$HF <-  data[,input$Hfol] 
            R <- subset(data, RB> 0 & RF >= 0)
            Tr <- subset(data, TB> 0 & TF >= 0)
            H <- subset(data, HB> 0 & HF >= 0)
            
            ERRR <- (1- mean(R$RF)/mean(R$RB))
            term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
            term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
            VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
            aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
            ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
            LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
     
            ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
            term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
            term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
            VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
            aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
            ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
            LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
            
            ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
            term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
            term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
            VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
            aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
            ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
            LLH <- 1-qgamma(0.975,shape = aH, scale = bH)
               
            if(input$STHdrug == 1 | input$STHdrug == 2) { 
            paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', round(100*ULT,1),').
For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', round(100*ULH,1),').
The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
            } else{paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', 
                         round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),
                         ';', round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
              
              }
              } else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
            {
              data$RB <-  data[,input$Rbas]  
              data$TB <-  data[,input$Tbas]
              data$RF <-  data[,input$Rfol]  
              data$TF <-  data[,input$Tfol] 
              R <- subset(data, RB> 0 & RF >= 0)
              Tr <- subset(data, TB> 0 & TF >= 0)
             
              ERRR <- (1- mean(R$RF)/mean(R$RB))
              term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
              term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
              VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
              aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
              ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
              LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
              
              ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
              term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
              term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
              VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
              aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
              ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
              LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
              if(input$STHdrug == 1 | input$STHdrug == 2) { 
               paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', round(100*ULT,1),'). The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
              } else {paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', 
                            round(100*ULT,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
                }
               } else {
              if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
              {
                data$RB <-  data[,input$Rbas]  
                data$HB <-  data[,input$Hbas] 
                data$RF <-  data[,input$Rfol]  
                data$HF <-  data[,input$Hfol] 
                R <- subset(data, RB> 0 & RF >= 0)
                H <- subset(data, HB> 0 & HF >= 0)
                
                ERRR <- (1- mean(R$RF)/mean(R$RB))
                term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
                term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
                VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
                aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
                ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
                LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
                
                ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
                term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
                VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
                aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
                ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
                LLH <- 1-qgamma(0.975,shape = aH, scale = bH)   
                if(input$STHdrug == 1 | input$STHdrug == 2){ 
                paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',
round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', round(100*ULH,1),'). The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                } else { paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', 
                               round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
                  
                  }
               } else {
                if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
                {
                  data$TB <-  data[,input$Tbas]
                  data$HB <-  data[,input$Hbas] 
                  data$TF <-  data[,input$Tfol] 
                  data$HF <-  data[,input$Hfol] 
                  Tr <- subset(data, TB> 0 & TF >= 0)
                  H <- subset(data, HB> 0 & HF >= 0)
                  
                  ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
                  term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
                  term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
                  VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
                  aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
                  ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
                  LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
                  
                  ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                  term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
                  term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
                  VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
                  aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
                  ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
                  LLH <- 1-qgamma(0.975,shape = aH, scale = bH)
                  if(input$STHdrug == 1 | input$STHdrug == 2){
                  paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>T. trichiura</em> equaled',round(100*ERRT,1),'% 
(',round(100*LLT,1),';', round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', round(100*ULH,1),'). 
The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                  } else {paste('The egg reduction rate (ERR; 95% confidence intervals) of the 
                                drug <em>T. trichiura</em> equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';',
                                round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'% 
                                (',round(100*LLH,1),';', round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
                    
                    }
                  
                  } else {
                  if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
                  {
                    data$RB <-  data[,input$Rbas]  
                    data$RF <-  data[,input$Rfol]  
                    R <- subset(data, RB> 0 & RF >= 0)
                    
                    ERRR <- (1- mean(R$RF)/mean(R$RB))
                    term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
                    term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
                    VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
                    aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
                    ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
                    LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
                    
                    if(input$STHdrug == 1 | input$STHdrug == 2){
                    paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                    } else {paste('The egg reduction rate (95% confidence intervals) of the drug 
                                  against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), 
                                  ';',round(100*ULR,1), '). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
                      
                      } 
                      } else {
                    if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                    {
                      data$TB <-  data[,input$Tbas]
                      data$TF <-  data[,input$Tfol] 
                      Tr <- subset(data, TB> 0 & TF >= 0)
                      
                      ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
                      term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
                      term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
                      VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
                      aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
                      ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
                      LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
                      if(input$STHdrug == 1 | input$STHdrug == 2){
                      paste('The egg reduction rate (ERRR; 95% confidence intervals) of the drug against <em>T. trichiura</em> equaled',round(100*ERRT,1),
'% (',round(100*LLT,1),';', round(100*ULT,1),'). The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                      } else { 
                        paste('The egg reduction rate (95% confidence intervals) of the drug against 
                              <em>T. trichiura</em> equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', 
                              round(100*ULT,1),'). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
                        
                        }
                        } else {
                        if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                        {
                          data$HB <-  data[,input$Hbas] 
                          data$HF <-  data[,input$Hfol] 
                          H <- subset(data, HB> 0 & HF >= 0)
                          
                          ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                          term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
                          term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
                          VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
                          aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
                          ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
                          LLH <- 1-qgamma(0.975,shape = aH, scale = bH)  
                          if(input$STHdrug == 1 | input$STHdrug == 2){ 
                          paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against hookworms equaled',round(100*ERRH,1),'%
(',round(100*LLH,1),';', round(100*ULH,1),'). 
The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                          } else {
                            paste('The egg reduction rate (95% confidence intervals) of the drug against hookworms equaled',round(100*ERRH,1),
                                  '% (',round(100*LLH,1),';', round(100*ULH,1),').  The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
                            
                            }                            
                
                          
                          } else {paste('Please match egg counting data.')}
                    }
                  }
                }  
              }   
            }
          } 
          }
        }
    }
  } 
})
  })

  
  output$err <- renderText({  
    inFile <- input$file1
    if (is.null(inFile))
    {paste0(('Please upload data.'))}
    else{err()} })
  
  
### Conlusions
concl<- eventReactive(input$eggreducrate, {
  isolate ({ 
    inFile <- input$file1
    if (is.null(inFile))
    {paste0('Please upload data.')
      } else {if (input$NTD=='1')
    {data <- read.xlsx(inFile$datapath,1)
    data$n <- 1
    n <- length(data[,1])
    # S haematobium
    data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
    data$shf <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
    
    # S mansoni
    data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
    data$smf <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))
    
    # S japonicum
    data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
    data$sjf <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))
    data$inf <- mean(ifelse(data$sh > -2 | data$sm > -2 | data$sj > -2, 1, 0))
    data$inf2 <- mean(ifelse(data$shf > -2 | data$smf > -2 | data$sjf > -2, 1, 0))
    
    if(mean(data$inf)==0 | mean(data$inf2)==0 ) {paste('Please provide egg count data.')}
    else {
      if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$shf)>-2 & mean(data$smf)>-2)
      {
        data$shB <-  data[,input$Shbas]  
        data$smB <-  data[,input$Smbas] 
        data$shF <-  data[,input$Shfol]  
        data$smF <-  data[,input$Smfol] 
        sh <- subset(data, shB >0 &  shF >=0)
        sm <- subset(data, smB >0 &  smF >=0)
        ERRSH <- 100*(1- mean(sh$shF)/mean(sh$shB))
        ERRSM <- 100*(1- mean(sm$smF)/mean(sm$smB))
        if(input$Sdrug == 1){
        shstar <- ifelse(ERRSH>=90, 1, ifelse(ERRSM<80, 3,2))
        smstar <- ifelse(ERRSM>=90, 1, ifelse(ERRSM<80, 3,2))
        if (shstar == 1 & smstar == 2){
        paste('The efficacy of the drug administered is satisfactory for both <em>S. haematobium</em> and <em>S. mansoni</em> infections.')
        } else {
          if (shstar == 2 & smstar ==1){paste('The efficacy of the drug administered is satisfactory for <em>S. mansoni</em> infections,
but is below the expected efficacy of 90% for <em>S. haematobium</em> infections.')
            
          } else {
            if (shstar == 1 & smstar == 2)
            {paste('The efficacy of the drug administered is satisfactory for <em>S. mansoni</em> infections, but is below the expected
efficacy of 90% for <em>S. haematobium</em> infections. Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
              
            } else {
              paste('The efficacy of the drug administered is below the expected efficacy of 90% for both <em>S. mansoni</em> and
<em>S. haematobium</em> infections. Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
              
              }
            }
            }
        
        } else {paste('Currently, an expected efficacy has only been defined for a single oral dose of praziquantel (40 mg / kg). 
                                      Consequently, no conclusions can be drawn on the efficacy of this drug or drug regimen against schistosomiasis.')}
      } else {
        if(mean(data$sh)>-2 & mean(data$shf)>-2){
          data$shB <-  data[,input$Shbas]  
          data$shF <-  data[,input$Shfol]  
          sh <- subset(data, shB >0 &  shF >=0)
          ERRSH <- 100*(1- mean(sh$shF)/mean(sh$shB))
          if(input$Sdrug == 1) {
          shstar <- ifelse(ERRSH>=90, 1, ifelse(ERRSH<80, 3,2))
          if (shstar == 1){
            paste('The efficacy of the drug administered is satisfactory for <em>S. haematobium</em> infections.')
          } else {
            paste('The efficacy of the drug administered is below the expected efficacy of 90% for <em>S. haematobium</em> infections. Please inform the 
local authorities (e.g., Ministry of Health) about this finding. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
            
              
          }
          } else {paste('Currently, an expected efficacy has only been defined for a single oral dose of praziquantel (40 mg / kg). 
                                      Consequently, no conclusions can be drawn on the efficacy of this drug or drug regimen against schistosomiasis.')}
          } else {
          if(mean(data$sm)>-2 & mean(data$smf)>-2){
            data$smB <-  data[,input$Smbas] 
            data$smF <-  data[,input$Smfol] 
            sm <- subset(data, smB >0 &  smF >=0)
            
            ERRSM <- 100*(1- mean(sm$smF)/mean(sm$smB))
            if(input$Sdrug == 1) { 
            smstar <- ifelse(ERRSM>=90, 1, ifelse(ERRSM<80, 3,2))
            if (smstar == 1){
              paste('The efficacy of the drug administered is satisfactory for <em>S. mansoni</em> infections.')
            } else {
              paste('The efficacy of the drug administered is below the expected drug efficacy of 90 percent <em>S. mansoni</em> infections. 
Please inform the local authorities (e.g., Ministry of Health) about this finding. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
              
              
            } 
            } else {paste('Currently, an expected efficacy has only been defined for a single oral dose of praziquantel (40 mg / kg). 
                                      Consequently, no conclusions can be drawn on the efficacy of this drug or drug regimen against schistosomiasis.')}
          } else {
            if(mean(data$sj)>-2 & mean(data$sjf>-2)){
              data$sjB <-  data[,input$Sjbas] 
              data$sjF <-  data[,input$Sjfol] 
              sj <- subset(data, sjB >0 &  sjF >=0)
              ERRSJ <- 100*(1- mean(sj$sjF)/mean(sj$sjB))
              if(input$Sdrug == 1) { 
              sjstar <- ifelse(ERRSJ>=90, 1, ifelse(ERRSJ<80, 3,2))
              if (sjstar == 1){
                paste('The efficacy of the drug administered is satisfactory for <em>S. japonicum</em> infections.')
              } else {
                paste('The efficacy of the drug administered is below the expected efficacy of 90% for <em>S. japonicum</em> infections. 
Please inform the local authorities (e.g., Ministry of Health) about this finding. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                
                
              } 
              } else {paste('Currently, an expected efficacy has only been defined for a single oral dose of praziquantel (40 mg / kg). 
                                      Consequently, no conclusions can be drawn on the efficacy of this drug or drug regimen against schistosomiasis.')}
            } else {paste('Please match egg count data.')}  
          }   
          }
        } 
    }
      }
      else {
        # Soil-transmitted helminthiasis
        if (input$NTD=='2')
        {
          data <- read.xlsx(inFile$datapath,1)
          n <- length(data[,1])
          # roundworms
          data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
          data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))
          
          # whipworms
          data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
          data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
          
          # hookworms
          data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
          data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
          
          data$inf <- ifelse(data$Rb > -2 | data$Tb > -2 | data$Hb > -2, 1, 0)
          data$inf2 <- ifelse(data$Rf > -2 | data$Tf > -2 | data$Hf > -2, 1, 0)
          
          if(mean(data$inf)==0 | mean(data$inf2)==0) {paste('Please provide egg count data.')}
          else {
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
            {
              data$RB <-  data[,input$Rbas]  
              data$TB <-  data[,input$Tbas]
              data$HB <-  data[,input$Hbas] 
              data$RF <-  data[,input$Rfol]  
              data$TF <-  data[,input$Tfol] 
              data$HF <-  data[,input$Hfol] 
              R <- subset(data, RB> 0 & RF >= 0)
              Tr <- subset(data, TB> 0 & TF >= 0)
              H <- subset(data, HB> 0 & HF >= 0)
              
              ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
              ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
              ERRH <- 100*(1- mean(H$HF)/mean(H$HB))
               
              if(input$STHdrug == 1) { 
                rstar<-ifelse(ERRR >=95,1,2)
                tstar<-ifelse(ERRT >=50,1,2)
                hstar<-ifelse(ERRH >=90,1,2)
                if(rstar == 1 & tstar == 2 & hstar == 2){
                  paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em>, but is below the expected efficacy for 
both <em>T. trichiura</em> (50%) and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                }  else  { 
                  if(rstar == 2 & tstar == 1 & hstar == 2){
                    paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em>, but
is below the expected efficacy for both <em>A. lumbricoides</em> (95%) and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                    
                  } else {  
                    if(rstar == 2 & tstar == 2 & hstar == 1){
                      paste('The efficacy of the drug administered is satisfactory for hookworms, but is below the expeceted efficacy for 
both <em>A. lumbricoides</em> (95%) and <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                    } else { 
                      if(rstar == 2 & tstar == 2 & hstar == 2){
                        paste('The efficacy of the drug administered is below the expected efficacy for <em>A. lumbricoides</em> (95%), <em>T. trichiura</em> (<50%) and hookworm
infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        
                      } else  { 
                          if(rstar == 1 & tstar == 1 & hstar == 2){
                            paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and <em>T. trichiura</em> infections, but is
below the expected efficcacy for hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            
                          } else  { 
                          if(rstar == 1 & tstar == 2 & hstar == 1){
                            paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and hookworm infections, but is
below the expected efficacy for <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            
                          } else { 
                            if(rstar == 2 & tstar == 1 & hstar == 1){
                              paste('The efficacy of the drug administered is satisfactory for both <em>T. trichiura</em> and hookworm infections, but
is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                              
                            } else { 
                              paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em>, <em>T. trichiura</em> and hookworm infections.')
                            
                            }
                        
                          }
                              
                          
                        }
                        }
                      }
                    } 
                }             
                
                
                } else {
                  if(input$STHdrug == 2){
                    rstar<-ifelse(ERRR >=95,1,2)
                    tstar<-ifelse(ERRT >=50,1,2)
                    hstar<-ifelse(ERRH >=70,1,2)
                    if(rstar == 1 & tstar == 2 & hstar == 2){
                      paste('The efficacy of the drug administered is satisfactory against <em>A. lumbricoides</em>, but is below 
the expected efficacy for both <em>T. trichiura</em> (50%) and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                      
                    }  else  { 
                      if(rstar == 2 & tstar == 1 & hstar == 2){
                        paste('The efficacy of the drug administered is satisfactory against <em>T. trichiura</em>, but is below
the expected efficacy for both <em>A. lumbricoides</em> (95%) and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        
                      } else {  
                        if(rstar == 2 & tstar == 2 & hstar == 1){
                          paste('The efficacy of the drug administered is satisfactory against hookorms, but is below the expeceted efficacy 
for both <em>A. lumbricoides</em> (95%) and <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                          
                        } else { 
                          if(rstar == 2 & tstar == 2 & hstar == 2){
                            paste('The efficacy of the drug administered is below the expected efficacy for <em>A. lumbricoides</em> (95%), 
<em>T. trichiura</em> (50%) and hookworm 
infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            
                            
                          } else  { 
                            if(rstar == 1 & tstar == 1 & hstar == 2){
                              paste('The efficacy of the drug administered is satisfactory against both <em>A. lumbricoides</em> and <em>T. trichiura</em> infections, but 
is below the expected efficcacy for hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                              
                            } else  { 
                              if(rstar == 1 & tstar == 2 & hstar == 1){
                                paste('The efficacy of the drug administered is satisfactory against both <em>A. lumbricoides</em> and hookworm infections, but 
is below the expected efficacy for <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                
                              } else { 
                                if(rstar == 2 & tstar == 1 & hstar == 1){
                                  paste('The efficacy of the drug administered is satisfactory against both <em>T. trichiura</em> and hookworm infections, 
but is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                  
                                } else { 
                                  paste('The efficacy of the drug administered is satisfactory against <em>A. lumbricoides</em>, <em>T. trichiura</em> and hookworm infections.')
                                  
                                }
                                
                              }
                              
                              
                          }
                          }
                        }
                      } 
                }  
                     
                   ## new drug 
                  } else {paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')}
                  
                }
              # end
              }
            else{ 
              if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
              {
                data$RB <-  data[,input$Rbas]  
                data$TB <-  data[,input$Tbas]
                data$RF <-  data[,input$Rfol]  
                data$TF <-  data[,input$Tfol] 
                R <- subset(data, RB> 0 & RF >= 0)
                Tr <- subset(data, TB> 0 & TF >= 0)
                
                ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
                ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
                   
                if(input$STHdrug == 1 | input$STHdrug ==2)
                { 
                rstar <-ifelse(ERRR >= 95,1,2)
                tstar <- ifelse(ERRT >=50,1,2)
                
                if(rstar == 1 & tstar == 1){
                paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and <em>T. trichiura</em> infections.')
                } else {
                if(rstar == 1 & tstar == 2) {
                  paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em> infections, 
but is below the expected efficacy for <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                  
                } else  {
                  if(rstar == 2 & tstar == 1) {
                    paste('The efficacy of the drug administered is satisfactory against <em>T. trichiura</em> infections, but 
is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                    
                  } else  {
                    if(rstar == 2 & tstar == 2) {
                      paste('The efficacy of the drug administered is below the expected efficacy for both <em>A. lumbricoides</em> (95%) 
and <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                      
                    }
                  }
                }
                }
                } else {paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')
                }
                } else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
                {
                  data$RB <-  data[,input$Rbas]  
                  data$HB <-  data[,input$Hbas] 
                  data$RF <-  data[,input$Rfol]  
                  data$HF <-  data[,input$Hfol] 
                  R <- subset(data, RB> 0 & RF >= 0)
                  H <- subset(data, HB> 0 & HF >= 0)
                  
                  ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
                  ERRH <- 100*(1- mean(H$HF)/mean(H$HB))
                  
                  if(input$STHdrug == 1) { 
                    rstar<-ifelse(ERRR >=95,1,2)
                    hstar<-ifelse(ERRH >=90,1,2)
                    if(rstar == 1 & hstar == 2){
                      paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em> infections, but
is below the expected efficacy hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                    }  else  { 
                      if(rstar == 2 & hstar == 1){
                        paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for <em>A. lumbricoides</em> infection (95%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                      } else {  
                        if(rstar == 2 & hstar == 2){
                          paste('The efficacy of the drug administered is below the expected efficacy for both <em>A. lumbricoides</em> (95%) 
and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        } else { 
                          if(rstar == 1 & hstar == 1){
                            paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and hookworm infections.')
                                }
                                
                              }
                          }
                          }
                        
                } else {
                  if(input$STHdrug == 2){
                    rstar<-ifelse(ERRR >=95,1,2)
                    hstar<-ifelse(ERRH >=70,1,2)
                    if(rstar == 1 & hstar == 2){
                      paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em> infections, 
but is below the expected efficacy hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                    }  else  { 
                      if(rstar == 2 & hstar == 1){
                        paste('The efficacy of the drug administered is satisfactory against hookworm infections, but is below the expected efficacy for <em>A. lumbricoides</em> infection (95%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                      } else {  
                        if(rstar == 2 & hstar == 2){
                          paste('The efficacy of the drug administered is below the expected efficacy for both <em>A. lumbricoides</em> (95%) 
and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        } else { 
                          if(rstar == 1 & hstar == 1){
                            paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and hookworm infections.')
                          }
                          
                        }
                    }
                  }
                    
                    ## new drug 
                  } else {paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')}
                  
                    } 
                  
                  } else{
                  if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
                  {
                    data$TB <-  data[,input$Tbas]
                    data$HB <-  data[,input$Hbas] 
                    data$TF <-  data[,input$Tfol] 
                    data$HF <-  data[,input$Hfol] 
                    Tr <- subset(data, TB> 0 & TF >= 0)
                    H <- subset(data, HB> 0 & HF >= 0)
                    
                    ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
                    ERRH <- 100*(1- mean(H$HF)/mean(H$HB))
                    
                    if(input$STHdrug == 1) { 
                      tstar<-ifelse(ERRT >=50,1,2)
                      hstar<-ifelse(ERRH >=90,1,2)
                      if(tstar == 1 & hstar == 2){
                        paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em> infections, 
but is below the expected efficacy hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                      }  else  { 
                        if(tstar == 2 & hstar == 1){
                          paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for <em>T. trichiura</em> infection (50%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                        } else {  
                          if(tstar == 2 & hstar == 2){
                            paste('The efficacy of the drug administered is below the expected efficacy for both whipwworm (50%) 
and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                          } else { 
                            if(tstar == 1 & hstar == 1){
                              paste('The efficacy of the drug administered is satisfactory for both <em>T. trichiura</em> and hookworm infections.')
                            }
                            
                          }
                      }
                    }
                      
                      } else {
                        if(input$STHdrug == 2){
                          tstar<-ifelse(ERRT >=50,1,2)
                          hstar<-ifelse(ERRH >=70,1,2)
                          if(tstar == 1 & hstar == 2){
                            paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em> infections, 
but is below the expected efficacy hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                          }  else  { 
                            if(tstar == 2 & hstar == 1){
                              paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for <em>T. trichiura</em> infection (50%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                            } else {  
                              if(tstar == 2 & hstar == 2){
                                paste('The efficacy of the drug administered is below the expected efficacy for both <em>T. trichiura</em> (50%) 
and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                              } else { 
                                if(tstar == 1 & hstar == 1){
                                  paste('The efficacy of the drug administered is satisfactory for both <em>T. trichiura</em> and hookworm infections.')
                                }
                                
                              }
                          }
                        }
                          
                          ## new drug 
                        } else {paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')}
                        
                      } 
                  
                    
                    } else{
                    if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
                    {
                      data$RB <-  data[,input$Rbas]  
                      data$RF <-  data[,input$Rfol]  
                      R <- subset(data, RB> 0 & RF >= 0)
                      
                      ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
          
                      if(input$STHdrug == 1 | input$STHdrug == 2)
                      {
                      rstar <- ifelse(ERRR>=95,1,2)
                      if(rstar == 1) {
                      paste('The efficacy of the drug administered is satisfactory.')  
                      } else {
                        paste('The efficacy of the drug administered is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                      }
                      } else {paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                        Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against <em>A. lumbricoides</em> infections.')}
                      
                      } else {
                      if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                      {
                        data$TB <-  data[,input$Tbas]
                        data$TF <-  data[,input$Tfol] 
                        Tr <- subset(data, TB> 0 & TF >= 0)
                        
                        ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
                        
                        if(input$STHdrug == 1 | input$STHdrug == 2)
                        { 
                        tstar <- ifelse(ERRR>=50,1,2)
                        if(tstar == 1) {
                          paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em> infections.')  
                        } else {
                          paste('The efficacy of the drug administered is below the expected efficacy for <em>T. trichiura</em> infections (50%). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        }
                        } else {paste('Currently, the expected efficacy is only determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against <em>T. trichiura</em> infections.')}
                        
                        } else{
                        if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                        {
                          data$HB <-  data[,input$Hbas] 
                          data$HF <-  data[,input$Hfol] 
                          H <- subset(data, HB> 0 & HF >= 0)
                          
                          ERRH <- 100*(1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                  
                          if(input$STHdrug == 1) { 
                            hstar<-ifelse(ERRH >=90,1,2)
                            if(hstar == 2){
                              paste('The efficacy of the drug administered is below the expected efficacy hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            }  else  { 
                              if(hstar == 1){
                                paste('The efficacy of the drug administered is satisfactory against hookworm infections.')
                                        } 
                                    }
                            } else {
                              if(STHdrug == 2){
                                hstar<-ifelse(ERRH >=70,1,2)
                                if(hstar == 2){
                                  paste('The efficacy of the drug administered is below the expected efficacy hookworm infections (70%). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                }  else  { 
                                  if(hstar == 1){
                                    paste('The efficacy of the drug administered is satisfactory against hookworm infections.')
                                  }    
                                }
                              
                                
                                ## new drug 
                              } else {paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequenlty, no conclusions can be drawn on the efficacy for this drug or drug regimen against hookworm infections.')}
                              
                            } 
                          
                          }
                        
                        else{paste('Please match egg counting data.')}
                        }
                      }
                  }  
                    }   
                }
              } 
          }
  }
        }
} 
})        
})  
output$concl <- renderText({  
  inFile <- input$file1
  if (is.null(inFile))
  {paste0(('Please upload data.'))}
  else{concl()} })

## graphs  
disterr <- eventReactive(input$eggreducrate, {
    isolate ({ 
      inFile <- input$file1
    if (is.null(inFile))
    {paste('Please upload data.')
      } else {if (input$NTD=='1')
    {data <- read.xlsx(inFile$datapath,1) 
    n <- length(data[,1])
    data$sh <- ifelse(input$Shbas=='Not recorded',rep(-2,n), ifelse(data[,input$Shbas]>0,1,0))
    data$shf <- ifelse(input$Shfol=='Not recorded',rep(-2,n), ifelse(data[,input$Shfol]>=0,1,0))
    
    # S mansoni
    data$sm <- ifelse(input$Smbas=='Not recorded',rep(-2,n), ifelse(data[,input$Smbas]>0,1,0))
    data$smf <- ifelse(input$Smfol=='Not recorded',rep(-2,n), ifelse(data[,input$Smfol]>=0,1,0))
    
    # S japonicum
    data$sj <- ifelse(input$Sjbas=='Not recorded',rep(-2,n), ifelse(data[,input$Sjbas]>0,1,0))
    data$sjf <- ifelse(input$Sjfol=='Not recorded',rep(-2,n), ifelse(data[,input$Sjfol]>=0,1,0))
  
      data$inf <- mean(ifelse(data$sh > -2 | data$sm > -2 | data$sj > -2, 1, 0))
      data$inf2 <- mean(ifelse(data$shf > -2 | data$smf > -2 | data$sjf > -2, 1, 0))
      if(mean(data$inf)==0 | mean(data$inf)==0) {}
    else 
    {
      if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$smf)>-2 & mean(data$shf)> - 2){
        data$shB <-  data[,input$Shbas]  
        data$smB <-  data[,input$Smbas] 
        data$shF <-  data[,input$Shfol]  
        data$smF <-  data[,input$Smfol] 
        sh <- subset(data, shB > 0 &shF >=0)
        sm <- subset(data, smB > 0 & smF >=0)
        ERRSH <- (1- mean(sh$shF)/mean(sh$shB))
        term1SH <- (mean(sh$shF)/mean(sh$shB))**2; term2SH <- ifelse(mean(sh$shF)==0,0,var(sh$shF)/mean(sh$shF)**2); term3SH <- var(sh$shB)/mean(sh$shB)**2
        term4SH <- ifelse(mean(sh$shF)==0,0,-2*cor(sh$shB,sh$shF)*sqrt(var(sh$shF))*sqrt(var(sh$shB))/(mean(sh$shB)*mean(sh$shF)))
        VARSH <-  term1SH*(term2SH+term3SH+term4SH); varSH <- VARSH / length(sh$shB)
        aSH <- ((1-ERRSH)**2)/varSH; bSH <- varSH/(1-ERRSH)
        
        ERRSM <- (1- mean(sm$smF)/mean(sm$smB))
        term1SM <- (mean(sm$smF)/mean(sm$smB))**2; term2SM <- ifelse(mean(sm$smF)==0,0,var(sm$smF)/mean(sm$smF)**2); term3SM <- var(sm$smB)/mean(sm$smB)**2
        term4SM <- ifelse(mean(sm$smF)==0,0,-2*cor(sm$smB,sm$smF)*sqrt(var(sm$smF))*sqrt(var(sm$smB))/(mean(sm$smB)*mean(sm$smF)))
        VARSM <-  term1SM*(term2SM+term3SM+term4SM); varSM <- VARSM / length(sm$smB)
        aSM <- ((1-ERRSM)**2)/varSM; bSM <- varSM/(1-ERRSM)
        
        
      if(input$Sdrug==1) { 
        
      par(mfrow=c(1,2))
        plot(c(0,100),c(0,5), ylab='', main=expression(italic(Schistosoma~haematobium)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
        t <- seq(0,80,1)
        t2 <- seq(80,90,1)
        t3 <- seq(90,100,1)
        polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
        polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
        polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
        abline(v=100*ERRSH, lwd=4)
        
        plot(c(0,100),c(0,5), ylab='', main=expression(italic(Schistosoma~mansoni)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
        t <- seq(0,80,1)
        t2 <- seq(80,90,1)
        t3 <- seq(90,100,1)
        polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
        polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
        polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
        abline(v=100*ERRSM, lwd=4)
        
        } else { 
        par(mfrow=c(1,2))
        SH <- rgamma(10000,shape = aSH, scale = bSH)
        hist(100*(1-SH),main=expression(italic(Schistosoma~haematobium)), ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
        axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
        abline(v=100*(1-qgamma(0.025,shape = aSH, scale = bSH)),lty= 2, lwd=4)
        abline(v=100*(1-qgamma(0.975,shape = aSH, scale = bSH)),lty=2, lwd=4)
        abline(v=100*ERRSH, lwd=4)
        
        SM <- rgamma(10000,shape = aSM, scale = bSM)
        hist(100*(1-SM), main=expression(italic(Schistosoma~mansoni)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
        axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
        abline(v=100*(1-qgamma(0.025,shape = aSM, scale = bSM)), lty=2,lwd=4)
        abline(v=100*(1-qgamma(0.975,shape = aSM, scale = bSM)), lty=2,lwd=4)
        abline(v=100*ERRSM, lwd=4)
        }
      
      } else {
        if(mean(data$sh)>-2 & mean(data$shf)> - 2){
          data$shB <-  data[,input$Shbas]  
          data$shF <-  data[,input$Shfol]  
          
          sh <- subset(data, shB >0 &  shF >=0)
          ERRSH <- (1- mean(sh$shF)/mean(sh$shB))
          term1SH <- (mean(sh$shF)/mean(sh$shB))**2; term2SH <- ifelse(mean(sh$shF)==0,0,var(sh$shF)/mean(sh$shF)**2); term3SH <- var(sh$shB)/mean(sh$shB)**2
          term4SH <- ifelse(mean(sh$shF)==0,0,-2*cor(sh$shB,sh$shF)*sqrt(var(sh$shF))*sqrt(var(sh$shB))/(mean(sh$shB)*mean(sh$shF)))
          VARSH <-  term1SH*(term2SH+term3SH+term4SH); varSH <- VARSH / length(sh$shB)
          aSH <- ((1-ERRSH)**2)/varSH; bSH <- varSH/(1-ERRSH)
          ULSH <- 1-qgamma(0.025,shape = aSH, scale = bSH)
          LLSH <- 1-qgamma(0.975,shape = aSH, scale = bSH)
          
          if(input$Sdrug == 1) {           
            plot(c(0,100),c(0,5), ylab='', main=expression(italic(Schistosoma~haematobium)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,80,1)
            t2 <- seq(80,90,1)
            t3 <- seq(90,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRSH, lwd=4)
          } else { 
            par(mfrow=c(1,1))
            SH <- rgamma(10000,shape = aSH, scale = bSH)
            hist(100*(1-SH),main=expression(italic(Schistosoma~haematobium)), ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
            axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
            abline(v=100*(1-qgamma(0.025,shape = aSH, scale = bSH)),lty= 2, lwd=4)
            abline(v=100*(1-qgamma(0.975,shape = aSH, scale = bSH)),lty=2, lwd=4)
            abline(v=100*ERRSH, lwd=4)
            }
          
          } else {
          if(mean(data$sm)>-2 & mean(data$smf)> - 2){
            data$smB <-  data[,input$Smbas] 
            data$smF <-  data[,input$Smfol] 
            sm <- subset(data, smB >0 &  smF >=0)
      
            ERRSM <- (1- mean(sm$smF)/mean(sm$smB))
            term1SM <- (mean(sm$smF)/mean(sm$smB))**2; term2SM <- ifelse(mean(sm$smF)==0,0,var(sm$smF)/mean(sm$smF)**2); term3SM <- var(sm$smB)/mean(sm$smB)**2
            term4SM <- ifelse(mean(sm$smF)==0,0,-2*cor(sm$smB,sm$smF)*sqrt(var(sm$smF))*sqrt(var(sm$smB))/(mean(sm$smB)*mean(sm$smF)))
            VARSM <-  term1SM*(term2SM+term3SM+term4SM); varSM <- VARSM / length(sm$smB)
            aSM <- ((1-ERRSM)**2)/varSM; bSM <- varSM/(1-ERRSM)
            ULSM <- 1-qgamma(0.025,shape = aSM, scale = bSM)
            LLSM <- 1-qgamma(0.975,shape = aSM, scale = bSM)
            
            if(input$Sdrug == 1) { 
            par(mfrow=c(1,1))
              plot(c(0,100),c(0,5), ylab='', main=expression(italic(Schistosoma~mansoni)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
              t <- seq(0,80,1)
              t2 <- seq(80,90,1)
              t3 <- seq(90,100,1)
              polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
              polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
              polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
              abline(v=100*ERRSM, lwd=4)
            } else {
              par(mfrow=c(1,1))
              SM <- rgamma(10000,shape = aSM, scale = bSM)
              hist(100*(1-SM), main=expression(italic(Schistosoma~mansoni)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
              axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
              abline(v=100*(1-qgamma(0.025,shape = aSM, scale = bSM)), lty=2,lwd=4)
              abline(v=100*(1-qgamma(0.975,shape = aSM, scale = bSM)), lty=2,lwd=4)
              abline(v=100*ERRSM, lwd=4)
            }
            
            } else {
            if(mean(data$sj)>-2 & mean(data$sjf)> - 2){
              data$sjB <-  data[,input$Sjbas] 
              data$sjF <-  data[,input$Sjfol] 
              sj <- subset(data, sjB >0 &  sjF >=0)
              ERRSJ <- (1- mean(sj$sjF)/mean(sj$sjB))
              term1SJ <- (mean(sj$sjF)/mean(sj$sjB))**2; term2SJ <- ifelse(mean(sj$sjF)==0,0,var(sj$sjF)/mean(sj$sjF)**2); term3SJ <- var(sj$sjB)/mean(sj$sjB)**2              
              term4SJ <- ifelse(mean(sj$sjF)==0,0,-2*cor(sj$sjB,sj$sjF)*sqrt(var(sj$sjF))*sqrt(var(sj$sjB))/(mean(sj$sjB)*mean(sj$sjF)))
              VARSJ <-  term1SJ*(term2SJ+term3SJ+term4SJ); varSJ <- VARSJ / length(sj$sjB)
              aSJ <- ((1-ERRSJ)**2)/varSJ; bSJ <- varSJ/(1-ERRSJ)
              ULSJ <- 1-qgamma(0.025,shape = aSJ, scale = bSJ)
              LLSJ <- 1-qgamma(0.975,shape = aSJ, scale = bSJ)
              
              if(input$Sdrug == 1) {
              par(mfrow=c(1,1))
                plot(c(0,100),c(0,5), ylab='', main=expression(italic(Schistosoma~japonicum)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                t <- seq(0,80,1)
                t2 <- seq(80,90,1)
                t3 <- seq(90,100,1)
                polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                abline(v=100*ERRSJ, lwd=4)   } else  {  
                  SJ <- rgamma(10000,shape = aSJ, scale = bSJ)
                  hist(100*(1-SJ), main=expression(italic(Schistosoma~japonicum)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                  axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                  abline(v=100*(1-qgamma(0.025,shape = aSJ, scale = bSJ)), lty=2,lwd=4)
                  abline(v=100*(1-qgamma(0.975,shape = aSJ, scale = bSJ)), lty=2,lwd=4)
                  abline(v=100*ERRSJ, lwd=4)      }
          }
       }
      }
      }
    }
    } else  {if (input$NTD=='2'){
        data <- read.xlsx(inFile$datapath,1)
        n <- length(data[,1])
        # roundworms
        data$Rb <- ifelse(input$Rbas=='Not recorded',rep(-2,n), ifelse(data[,input$Rbas]>0,1,0))
        data$Rf <- ifelse(input$Rfol=='Not recorded',rep(-2,n), ifelse(data[,input$Rfol]>=0,1,0))
        
        # whipworms
        data$Tb <- ifelse(input$Tbas=='Not recorded',rep(-2,n), ifelse(data[,input$Tbas]>0,1,0))
        data$Tf <- ifelse(input$Tfol=='Not recorded',rep(-2,n), ifelse(data[,input$Tfol]>=0,1,0))
        
        # hookworms
        data$Hb <- ifelse(input$Hbas=='Not recorded',rep(-2,n), ifelse(data[,input$Hbas]>0,1,0))
        data$Hf <- ifelse(input$Hfol=='Not recorded',rep(-2,n), ifelse(data[,input$Hfol]>=0,1,0))
        
        data$inf <- ifelse(data$Rb > -2 | data$Tb > -2 | data$Hb > -2, 1, 0)}
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
        {
          data$RB <-  data[,input$Rbas]  
          data$TB <-  data[,input$Tbas]
          data$HB <-  data[,input$Hbas] 
          data$RF <-  data[,input$Rfol]  
          data$TF <-  data[,input$Tfol] 
          data$HF <-  data[,input$Hfol] 
          R <- subset(data, RB> 0 & RF >= 0)
          Tr <- subset(data, TB> 0 & TF >= 0)
          H <- subset(data, HB> 0 & HF >= 0)
          
          ERRR <- (1- mean(R$RF)/mean(R$RB))
          term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
          term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
          VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
          aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
          ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
          LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
          
          ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
          term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
          term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
          VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
          aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
          ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
          LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
          
          ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
          term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
          term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
          VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
          aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
          ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
          LLH <- 1-qgamma(0.975,shape = aH, scale = bH)
         
          if(input$STHdrug == 1)
          { 
          par(mfrow=c(1,3))
            plot(c(0,100),c(0,5), ylab='', main=expression(italic(Ascaris~lumbricoides)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,85,1)
            t2 <- seq(85,95,1)
            t3 <- seq(95,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRR, lwd=4)
            
            plot(c(0,100),c(0,5), ylab='', main=expression(italic(Trichuris~trichiura)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,40,1)
            t2 <- seq(40,50,1)
            t3 <- seq(50,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRT, lwd=4)
            
            plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,80,1)
            t2 <- seq(80,90,1)
            t3 <- seq(90,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRH, lwd=4)
   
          } else {
          
          if(input$STHdrug == 2)
          { 
            par(mfrow=c(1,3))
            plot(c(0,100),c(0,5), ylab='', main=expression(italic(Ascaris~lumbricoides)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,85,1)
            t2 <- seq(85,95,1)
            t3 <- seq(95,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRR, lwd=4)
            
            plot(c(0,100),c(0,5), ylab='', main=expression(italic(Trichuris~trichiura)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,40,1)
            t2 <- seq(40,50,1)
            t3 <- seq(50,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRT, lwd=4)
            
            plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
            t <- seq(0,60,1)
            t2 <- seq(60,70,1)
            t3 <- seq(70,100,1)
            polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
            polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
            polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
            abline(v=100*ERRH, lwd=4)
            
          } else {
            par(mfrow=c(1,3))
            R <- rgamma(10000,shape = aR, scale = bR)
            hist(100*(1-R), main=expression(italic(Ascaris~lumbricoides)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
            axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
            abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
            abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
            abline(v=100*ERRR, lwd=4) 
            
            T <- rgamma(10000,shape = aT, scale = bT)
            hist(100*(1-T), main=expression(italic(Trichuris~trichiura)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
            axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
            abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
            abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
            abline(v=100*ERRT, lwd=4) 
            
            H <- rgamma(10000,shape = aH, scale = bH)
            hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
            axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
            abline(v=100*(1-qgamma(0.025,shape = aH, scale = bH)), lty=2,lwd=4)
            abline(v=100*(1-qgamma(0.975,shape = aH, scale = bH)), lty=2,lwd=4)
            abline(v=100*ERRH, lwd=4)
        }
          }
            
        } else { 
          if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
          {
            data$RB <-  data[,input$Rbas]  
            data$TB <-  data[,input$Tbas]
            data$RF <-  data[,input$Rfol]  
            data$TF <-  data[,input$Tfol] 
            R <- subset(data, RB> 0 & RF >= 0)
            Tr <- subset(data, TB> 0 & TF >= 0)
            
            ERRR <- (1- mean(R$RF)/mean(R$RB))
            term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
            term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
            VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
            aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
            ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
            LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
            
            ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
            term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
            term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
            VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
            aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
            ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
            LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
            
            if(input$STHdrug == 1 | input$STHdrug == 2)
            {
            par(mfrow=c(1,2))
              plot(c(0,100),c(0,5), ylab='', main=expression(italic(Ascaris~lumbricoides)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
              t <- seq(0,85,1)
              t2 <- seq(85,95,1)
              t3 <- seq(95,100,1)
              polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
              polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
              polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
              abline(v=100*ERRR, lwd=4)
              
              plot(c(0,100),c(0,5), ylab='', main=expression(italic(Trichuris~trichiura)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
              t <- seq(0,40,1)
              t2 <- seq(40,50,1)
              t3 <- seq(50,100,1)
              polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
              polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
              polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
              abline(v=100*ERRT, lwd=4)
              } else {
              par(mfrow=c(1,2))
                R <- rgamma(10000,shape = aR, scale = bR)
                hist(100*(1-R), main=expression(italic(Ascaris~lumbricoides)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                abline(v=100*ERRR, lwd=4)
                
                T <- rgamma(10000,shape = aT, scale = bT)
                hist(100*(1-T), main=expression(italic(Trichuris~trichiura)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
                abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
                abline(v=100*ERRT, lwd=4)
            }
            
            } else{  
            if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
            {
              data$RB <-  data[,input$Rbas]  
              data$HB <-  data[,input$Hbas] 
              data$RF <-  data[,input$Rfol]  
              data$HF <-  data[,input$Hfol] 
              R <- subset(data, RB> 0 & RF >= 0)
              H <- subset(data, HB> 0 & HF >= 0)
              
              ERRR <- (1- mean(R$RF)/mean(R$RB))
              term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
              term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
              VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
              aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
              ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
              LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
              
              ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
              term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
              term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
              VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
              aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
              ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
              LLH <- 1-qgamma(0.975,shape = aH, scale = bH)
              
             if (input$STHdrug==1){  
              par(mfrow=c(1,2))
               plot(c(0,100),c(0,5), ylab='', main=expression(italic(Ascaris~lumbricoides)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
               t <- seq(0,85,1)
               t2 <- seq(85,95,1)
               t3 <- seq(95,100,1)
               polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
               polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
               polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
               abline(v=100*ERRR, lwd=4)
               
               plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
               t <- seq(0,80,1)
               t2 <- seq(80,90,1)
               t3 <- seq(90,100,1)
               polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
               polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
               polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
               abline(v=100*ERRH, lwd=4) 
              } else {
              if(input$STHdrug == 2)
              {
                par(mfrow=c(1,2))
                plot(c(0,100),c(0,5), ylab='', main=expression(italic(Ascaris~lumbricoides)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                t <- seq(0,85,1)
                t2 <- seq(85,95,1)
                t3 <- seq(95,100,1)
                polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                abline(v=100*ERRR, lwd=4)
                
                
                plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                t <- seq(0,60,1)
                t2 <- seq(60,70,1)
                t3 <- seq(70,100,1)
                polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                abline(v=100*ERRH, lwd=4)
                } else {
                par(mfrow=c(1,2))
                  R <- rgamma(10000,shape = aR, scale = bR)
                  hist(100*(1-R), main=expression(italic(Ascaris~lumbricoides)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                  axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                  abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                  abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                  abline(v=100*ERRR, lwd=4)
                  
                  H <- rgamma(10000,shape = aH, scale = bH)
                  hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                  axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                  abline(v=100*(1-qgamma(0.025,shape = aH, scale = bH)), lty=2,lwd=4)
                  abline(v=100*(1-qgamma(0.975,shape = aH, scale = bH)), lty=2,lwd=4)
                  abline(v=100*ERRH, lwd=4)
                  }
                
              }   
            }  else{  
              if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
              {
                data$TB <-  data[,input$Tbas]
                data$HB <-  data[,input$Hbas] 
                data$TF <-  data[,input$Tfol] 
                data$HF <-  data[,input$Hfol] 
                Tr <- subset(data, TB> 0 & TF >= 0)
                H <- subset(data, HB> 0 & HF >= 0)
                
                ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
                term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
                term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
                VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
                aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
                ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
                LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
                
                ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
                term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
                VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
                aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
                ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
                LLH <- 1-qgamma(0.975,shape = aH, scale = bH)
                
                if(input$STHdrug == 1)
                {
                par(mfrow=c(1,2))
                  plot(c(0,100),c(0,5), ylab='', main=expression(italic(Trichuris~trichiura)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                  t <- seq(0,40,1)
                  t2 <- seq(40,50,1)
                  t3 <- seq(50,100,1)
                  polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                  polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                  polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                  abline(v=100*ERRT, lwd=4)
                  
                  plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                  t <- seq(0,80,1)
                  t2 <- seq(80,90,1)
                  t3 <- seq(90,100,1)
                  polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                  polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                  polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                  abline(v=100*ERRH, lwd=4)
                  
                } else {
                if (input$STHdrug==2){ 
                  par(mfrow=c(1,2))
                  plot(c(0,100),c(0,5), ylab='', main=expression(italic(Trichuris~trichiura)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                  t <- seq(0,40,1)
                  t2 <- seq(40,50,1)
                  t3 <- seq(50,100,1)
                  polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                  polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                  polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                  abline(v=100*ERRT, lwd=4)
                  
                  plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                  t <- seq(0,60,1)
                  t2 <- seq(60,70,1)
                  t3 <- seq(70,100,1)
                  polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                  polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                  polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                  abline(v=100*ERRH, lwd=4)
                  } else {           
                par(mfrow=c(1,2))
                    T <- rgamma(10000,shape = aT, scale = bT)
                    hist(100*(1-T), main=expression(italic(Trichuris~trichiura)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                    axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                    abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
                    abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
                    abline(v=100*ERRT, lwd=4)
                    
                    H <- rgamma(10000,shape = aH, scale = bH)
                    hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                    axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                    abline(v=100*(1-qgamma(0.025,shape = aH, scale = bH)), lty=2,lwd=4)
                    abline(v=100*(1-qgamma(0.975,shape = aH, scale = bH)), lty=2,lwd=4)
                    abline(v=100*ERRH, lwd=4)
                    }
                }  
              } else {  
                if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
                {
                  data$RB <-  data[,input$Rbas]  
                  data$RF <-  data[,input$Rfol]  
                  R <- subset(data, RB> 0 & RF >= 0)
                  
                  ERRR <- (1- mean(R$RF)/mean(R$RB))
                  term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
                  term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
                  VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
                  aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
                  ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
                  LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
                  
                  if(input$STHdrug == 1 | input$STHdrug ==2){
                  par(mfrow=c(1,1))
                    plot(c(0,100),c(0,5), ylab='', main=expression(italic(Ascaris~lumbricoides)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                    t <- seq(0,85,1)
                    t2 <- seq(85,95,1)
                    t3 <- seq(95,100,1)
                    polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                    polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                    polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                    abline(v=100*ERRR, lwd=4)
                  }
                  else {
                    par(mfrow=c(1,1))
                    R <- rgamma(10000,shape = aR, scale = bR)
                    hist(100*(1-R), main=expression(italic(Ascaris~lumbricoides)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                    axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                    abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                    abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                    abline(v=100*ERRR, lwd=4)
                    }
                  
                  } else{  
                    if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                    {
                      data$TB <-  data[,input$Tbas]
                      data$TF <-  data[,input$Tfol] 
                      Tr <- subset(data, TB> 0 & TF >= 0)
                      
                      ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
                      term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
                      term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
                      VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
                      aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
                      ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
                      LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
                      
                      if(input$STHdrug == 1 | input$STHdrug ==2){
                      par(mfrow=c(1,1))
                        plot(c(0,100),c(0,5), ylab='', main=expression(italic(Trichuris~trichiura)),yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                        t <- seq(0,40,1)
                        t2 <- seq(40,50,1)
                        t3 <- seq(50,100,1)
                        polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                        polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                        polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                        abline(v=100*ERRT, lwd=4)    
                        } else {
                      par(mfrow=c(1,1)) 
                          T <- rgamma(10000,shape = aT, scale = bT)
                          hist(100*(1-T), main=expression(italic(Trichuris~trichiura)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                          axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                          abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
                          abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
                          abline(v=100*ERRT, lwd=4)     
                      }
                      
                      } else {  
                    if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                    {
                      data$HB <-  data[,input$Hbas] 
                      data$HF <-  data[,input$Hfol] 
                      H <- subset(data, HB> 0 & HF >= 0)
                      
                      ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                      term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
                      term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
                      VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
                      aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
                      ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
                      LLH <- 1-qgamma(0.975,shape = aH, scale = bH)
                      
                      par(mfrow=c(1,1))
                      if (input$STHdrug==1){ 
                        plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                        t <- seq(0,80,1)
                        t2 <- seq(80,90,1)
                        t3 <- seq(90,100,1)
                        polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                        polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                        polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                        abline(v=100*ERRH, lwd=4)
                        } else {
                      if(input$STHdrug == 2){
                        plot(c(0,100),c(0,5), ylab='', main='Hookworm',yaxt='n', type='n', xlab= 'Drug efficacy (%)', bty='n')
                        t <- seq(0,60,1)
                        t2 <- seq(60,70,1)
                        t3 <- seq(70,100,1)
                        polygon(c(t,rev(t)),c(rep(0,length(t)), rep(5,length(t))),col="#EB4C4C", border=NA)
                        polygon(c(t2,rev(t2)),c(rep(0,length(t2)), rep(5,length(t2))),col="grey", border=NA)
                        polygon(c(t3,rev(t3)),c(rep(0,length(t3)), rep(5,length(t3))),col="#56A435", border=NA)
                        abline(v=100*ERRH, lwd=4)
                        } else { 
                          H <- rgamma(10000,shape = aH, scale = bH)
                          hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (% )')
                          axis(side=2, at=seq(0,10000,1000),lab = seq(0,100,10))
                          abline(v=100*(1-qgamma(0.025,shape = aH, scale = bH)), lty=2,lwd=4)
                          abline(v=100*(1-qgamma(0.975,shape = aH, scale = bH)), lty=2,lwd=4)
                          abline(v=100*ERRH, lwd=4)
                          }
                      } 
                    }
                  }
                }
                
                
              }
              
              
            }
            
          }
        }   
        
      }   
      
    } 
  })  
})    
output$disterr <- renderPlot ({ 
inFile <- input$file1
if (is.null(inFile))
{}
else{disterr()} })


## Matching headers with input    
    observe({ 
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      data <- read.xlsx(inFile$datapath,1)
      updateSelectInput(session,'baseline',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'incl',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'age',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'sex',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Shbas',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Smbas',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Sjbas',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Rbas',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Tbas',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Hbas',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'followup',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Shfol',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Smfol',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Sjfol',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Rfol',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Tfol',choices=c('Not recorded',names(data)))
      updateSelectInput(session,'Hfol',choices=c('Not recorded',names(data)))
    })
    output$report = downloadHandler(
      filename = 'report.pdf',
      content = function(file) {
        out = knit2pdf('input2.Rnw', clean = TRUE, texi2dvi="pdflatex")
        file.rename(out, file) # move pdf to file for downloading
      },
      contentType = 'application/pdf'
    )
    
       })