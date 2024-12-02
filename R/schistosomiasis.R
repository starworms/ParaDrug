
#' @title Analysis of Schistosomiasis (subjects)
#' @description Analysis of Schistosomiasis (subjects)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Shbas column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP
#' @param Shfol column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP
#' @param Smbas column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Smfol column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Sjbas column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Sjfol column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param ... not used yet
#' @export
#' @return TODO
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_schistosomiasis_n(x)
paradrug_schistosomiasis_n <- function(object, 
                            Shbas = "BL_KK2_AL_EPG", Shfol = "FU_KK2_AL_EPG", 
                            Smbas = "BL_KK2_TT_EPG", Smfol = "FU_KK2_TT_EPG", 
                            Sjbas = "BL_KK2_HW_EPG", Sjfol = "FU_KK2_HW_EPG", 
                            ...){
    data <- object$data
    input <- list(Shbas = Shbas, Shfol = Shfol, 
                  Smbas = Smbas, Smfol = Smfol, 
                  Sjbas = Sjbas, Sjfol = Sjfol)
    
    n <- nrow(data)
    
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
    
    
    if(mean(data$inf)==0 | mean(data$inf2) == 0) {number <- paste('No egg count data was provided.')}
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
            mix <- dim(subset(data, data$smB>0 & data$shB>0))[1]
            com <- dim(subset(data, data$shc==2 | data$smc ==2))[1]
            nsm <- dim(subset(data, data$smB>0))[1]
            nsm2 <- dim(subset(data, data$smB>0 & data$smF>=0))[1]
            nsh <- dim(subset(data, data$shB>0))[1]
            nsh2 <- dim(subset(data, data$shB>0 & data$shF>=0))[1]
            number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. 
$Schistosoma$ $haematobium$ infections were observed in', nsh, 'subjects (', round(100*nsh/n,1), 'percent ), 
$S.$ $mansoni$ infections in', nsm, 'subjects (', round(100*nsm/n,1),'percent ). Mixed $Schistosoma$ infections 
                          were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). 
                        Complete data were available for', com, 'subjects, 
                        including', nsh2, 'cases of $S.$ $haematobium$, and', nsm2, 'cases of $S.$ $mansoni$.')
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
                nsh <- dim(subset(data, data$shB>0))[1]
                nsh2 <- dim(subset(data, data$shB>0 & data$shF>=0))[1]
                number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Schistosoma$ $haematobium$ infections were observed in', nsh, 
                                'subjects (', round(100*nsh/n,1), 'percent ). Complete data were available for', nsh2, 'subjects.')
            }
            else{
                if(mean(data$sm)>-2 & mean(data$smF)>-2){
                    n <- length(data[,1])
                    data$smB <-  data[,input$Smbas] 
                    data$smF <-  data[,input$Smfol] 
                    data$smp <- ifelse(data$smB>0,1,0)
                    data$smf <- ifelse(data$smF>=0,1,0) 
                    data$smc <- data$smp + data$smf
                    nsm <- dim(subset(data, data$smB>0))[1]
                    nsm2 <- dim(subset(data, data$smB>0 & data$smF>=0))[1]
                    number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Schistosoma$ $mansoni$ infections were observed in', nsm, 
                                    'subjects (', round(100*nsm/n,1), 'percent ). Complete data were available for', nsm2, 'subjects.')
                    
                }
                else{
                    if(mean(data$sj)>-2 & mean(data$sjF>-2)){
                        n <- length(data[,1])
                        data$sjB <-  data[,input$Sjbas] 
                        data$sjF <-  data[,input$Sjfol] 
                        data$sjp <- ifelse(data$sjB>0,1,0)
                        data$sjf <- ifelse(data$sjF>=0,1,0) 
                        data$sjc <- data$sjp + data$sjf
                        nsj <- dim(subset(data, data$sjB>0))[1]
                        nsj2 <- dim(subset(data, data$sjB>0 & data$sjF>=0))[1]
                        number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Schistosoma$ $japonicum$ infections were observed in', nsj, 'subjects (', round(100*nsj/n,1), 'percent ). Complete data was available for', nsj2, 'subjects')
                        
                        
                    }
                    else{nubmer <- paste('No egg count data was provided.')}  
                }   
            }
        } 
    }
}

#' @title Analysis of Schistosomiasis (intensity)
#' @description Analysis of Schistosomiasis (intensity)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Shbas column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP
#' @param Shfol column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP
#' @param Smbas column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Smfol column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Sjbas column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Sjfol column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param ... not used yet
#' @export
#' @return TODO
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_schistosomiasis_intensity(x)
paradrug_schistosomiasis_intensity <- function(object, 
                                               Shbas = "BL_KK2_AL_EPG", Shfol = "FU_KK2_AL_EPG", 
                                               Smbas = "BL_KK2_TT_EPG", Smfol = "FU_KK2_TT_EPG", 
                                               Sjbas = "BL_KK2_HW_EPG", Sjfol = "FU_KK2_HW_EPG", 
                                               ...){
    data <- object$data
    input <- list(Shbas = Shbas, Shfol = Shfol, 
                  Smbas = Smbas, Smfol = Smfol, 
                  Sjbas = Sjbas, Sjfol = Sjfol)
    
    n <- nrow(data)
    
    
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
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {intense <- paste('No egg count data was provided.')}
    else {
        if(mean(data$sh)>-2 & mean(data$sm)>-2 & mean(data$smF)>-2 & mean(data$shF> - 2))
        {
            data$shB <-  data[,input$Shbas]  
            data$smB <-  data[,input$Smbas] 
            data$shF <-  data[,input$Shfol]  
            data$smF <-  data[,input$Smfol] 
            sh <- subset(data, data$shB >0 &  data$shF >=0)
            sm <- subset(data, data$smB >0 &  data$smF >=0)
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
            intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $S.$ $haematobium$ egg count equaled',Msh,'(',q25sh,';',q75sh,') eggs per 10 ml of urine. The mean 
$S.$ $mansoni$ egg count equaled',Msm,'(',q25sm,';',q75sm,') eggs per gram of stool. Low and highy-intensity $S.$ $haematobium$  
infections were observed in', NshL,'(',round(100*NshL/nsh,1),'percent ) and', NshH, '(',round(100*NshH/nsh,1),'percent ) subjects,  respectively. 
      For $S.$ $mansoni$, the number of low, moderate and high-intensity infections were', NsmL,'(',round(100*NsmL/nsm,1),'percent ),', NsmM,
                             '(',round(100*NsmM/nsm,1),'percent ) and', NsmH, '(',round(100*NsmH/nsm,1),'percent ), respectively.')
        }
        else{ 
            if(mean(data$sh)>-2 & mean(data$shF >-2)){
                data$shB <-  data[,input$Shbas] 
                data$shF <-  data[,input$Shfol] 
                sh <- subset(data, data$shB >0 &  data$shF >=0)
                NshH <- sum(ifelse(sh$shB>=50,1,0))
                NshL <- sum(ifelse(sh$shB>0 & sh$shB<50,1,0))
                q25sh <- round(quantile(sh$shB, probs=c(0.25)),1)
                q75sh <- round(quantile(sh$shB, probs=c(0.75)),1)
                Msh <- round(mean(sh$shB),1)
                nsh <- length(sh$shB)
                
                intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) $S.$ $haematobium$ egg count equaled',Msh,'(',q25sh,';',q75sh,') eggs per 10 ml of urine. 
Low and high-intensity $S.$ $haematobium$ infections were observed in', NshL,'(',round(100*NshL/nsh,1),'percent ) and', NshH, '(',round(100*NshH/nsh,1),'percent ) 
                subjects, respectively.')
            }
            else{
                if(mean(data$sm)>-2 & mean(data$smF)> -2){
                    data$smB <-  data[,input$Smbas] 
                    data$smF <-  data[,input$Smfol] 
                    sm <- subset(data, data$smB >0 &  data$smF >=0)
                    NsmH <- sum(ifelse(sm$smB>=400,1,0))
                    NsmM <- sum(ifelse(sm$smB>=100 & sm$smB<400,1,0))
                    NsmL <- sum(ifelse(sm$smB>0 & sm$smB<100,1,0))
                    q25sm <- round(quantile(sm$smB, probs=c(0.25)),1)
                    q75sm <- round(quantile(sm$smB, probs=c(0.75)),1)
                    Msm <- round(mean(sm$smB),1)
                    nsm <- length(sm$smB)
                    intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) $S.$ $mansoni$ egg count equaled',Msm,'(',q25sm,';',q75sm,') eggs per gram of stool. 
Low, moderate and high-intensity  $S.$ $mansoni$ infections were observed in',NsmL,'(',round(100*NsmL/nsm,1),'percent ),', NsmM,'(',round(100*NsmM/nsm,1),'percent ) 
                  and', NsmH, '(',round(100*NsmH/nsm,1),'percent ) subjects, respectively.')
                }
                else{
                    if(mean(data$sj)>-2 & mean(data$sjF) >- 2){
                        data$sjB <-  data[,input$Sjbas] 
                        data$sjF <-  data[,input$Sjfol] 
                        sj <- subset(data, data$sjB >0 &  data$sjF >=0)
                        NsjH <- sum(ifelse(sj$sjB>=400,1,0))
                        NsjM <- sum(ifelse(sj$sjB>=100 & sj$sjB<400,1,0))
                        NsjL <- sum(ifelse(sj$sjB>0 & sj$sjB<100,1,0))
                        q25sj <- round(quantile(sj$sjB, probs=c(0.25)),1)
                        q75sj <- round(quantile(sj$sjB, probs=c(0.75)),1)
                        Msj <- round(mean(sj$sjB),1)
                        nsj <- length(sj$sjB)
                        intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) $S.$ $japonicum$ egg count equaled',Msj,'(',q25sj,';',q75sj,') eggs per gram of stool. Low, moderate and 
high-intensity $S.$ $japonicum$ infections were observed in',NsjL,'(',round(100*NsjL/nsj,1),'percent ),', NsjM,'(',round(100*NsjM/nsj,1),'percent ) and', NsjH, 
                                         '(',round(100*NsjH/nsj,1),'percent ) subjects, respectively.')
                    }
                    else{intense <- paste('No egg count data was provided.')}  
                }   
            }
        } 
    }
}


#' @title Plot of eggcount of Schistosomiasis
#' @description Plot of eggcount of Schistosomiasis
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Shbas column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP
#' @param Shfol column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP
#' @param Smbas column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Smfol column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Sjbas column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Sjfol column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param ... not used yet
#' @export
#' @return TODO
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- plot_paradrug_schistosomiasis_eggcount(x)
plot_paradrug_schistosomiasis_eggcount  <- function(object, 
                                                    Shbas = "BL_KK2_AL_EPG", Shfol = "FU_KK2_AL_EPG", 
                                                    Smbas = "BL_KK2_TT_EPG", Smfol = "FU_KK2_TT_EPG", 
                                                    Sjbas = "BL_KK2_HW_EPG", Sjfol = "FU_KK2_HW_EPG", 
                                                    ...){
    data <- object$data
    input <- list(Shbas = Shbas, Shfol = Shfol, 
                  Smbas = Smbas, Smfol = Smfol, 
                  Sjbas = Sjbas, Sjfol = Sjfol)
    
    n <- nrow(data)
    
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
        sh <- subset(data, data$shB >0 &  data$shF >=0)
        sm <- subset(data, data$smB >0 &  data$smF >=0)
        par(mfrow=c(1,2))
        hist(sh$shB, col = "#EB4C4C", main=expression(italic(Schistosoma~haematobium)), freq=T,ylab = 'Number of subjects', xlab='Urine egg counts (eggs per 10 ml)')
        hist(sm$smB, col = "#EB4C4C", main=expression(italic(Schistosoma~mansoni)), freq=T, ylab='Number of subjects', xlab='Fecal egg counts (eggs per gram of stool)')}
        else
        {if(mean(data$sh)>-2 & mean(data$shf> - 2)){
            data$shB <-  data[,input$Shbas]  
            data$shF <-  data[,input$Shfol]  
            sh <- subset(data, data$shB >0 &  data$shF >=0)
            hist(sh$shB, col = "#EB4C4C", main=expression(italic(Schistosoma~haematobium)), freq=T, ylab='Number of subjects',xlab='Urine egg counts (eggs per 10 ml)')}
            else 
            {if(mean(data$sm)>-2 & mean(data$smf)>-2) {
                data$smB <-  data[,input$Smbas] 
                data$smF <-  data[,input$Smfol] 
                sm <- subset(data, data$smB >0 &  data$smF >=0)
                hist(sm$smB, col = "#EB4C4C", main=expression(italic(Schistosoma~mansoni)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')}
                else
                {if(mean(data$sj)>-2 & mean(data$sjf)>-2) {
                    data$sjB <-  data[,input$Sjbas]  
                    data$sjF <-  data[,input$Sjfol]  
                    sj <- subset(data, data$sjB >0 &  data$sjF >=0)
                    hist(sj$sjB, col = "#EB4C4C", main=expression(italic(Schistosoma~japonicum)), freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')}
                    else  {  }
                }
            }
        }
    }
}
