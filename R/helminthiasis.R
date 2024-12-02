#' @title Analysis of Helminthiasis (subjects)
#' @description Analysis of Helminthiasis (subjects)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param ... not used yet
#' @export
#' @return TODO
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_n(x)
paradrug_helminthiasis_n <- function(object, 
                                   Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                   Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                   Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                   ...){
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol)
    
    n <- nrow(data)
    
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
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {number <- paste('No egg count data was provided.')}
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
            mix <- dim(subset(data,data$mix>1))[1]
            com <- dim(subset(data, data$Rc==2 | data$Tc ==2 | data$Hc ==2))[1]
            nR <- dim(subset(data, data$RB>0))[1]
            nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
            nT <- dim(subset(data, data$TB>0))[1]
            nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
            nH <- dim(subset(data, data$HB>0))[1]
            nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
            
            number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ), $T.$ $trichiura$ infections in', nT, 'subjects (', round(100*nT/n,1),'percent ) and hookworms in', nH, '(',round (100*nH/n,1),'percent ) subjects. Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). Complete data were available for', com, 'subjects, including', nR2, 'cases of $A.$ $lumbricoides$,', nT2, 'cases of $T.$ $trichiura$, and',nH2, 'cases of hookworms.')
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
                mix <- dim(subset(data, data$mix>1))[1]
                com <- dim(subset(data, data$Rc==2 | data$Tc ==2))[1]
                nR <- dim(subset(data, data$RB>0))[1]
                nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                nT <- dim(subset(data, data$TB>0))[1]
                nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                
                number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ), $T.$ $trichiura$ infections in', nT, 'subjects (', round(100*nT/n,1),'percent ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of $A.$ $lumbricoides$, and ', nT2, 'cases of $T.$ $trichiura$.')
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
                    mix <- dim(subset(data, data$mix>1))[1]
                    com <- dim(subset(data, data$Rc==2 | data$Hc ==2))[1]
                    nR <- dim(subset(data, data$RB>0))[1]
                    nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                    nH <- dim(subset(data, data$HB>0))[1]
                    nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                    
                    number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'percent ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of $A.$ $lumbricoides$, and ', nH2, 'cases of hookworms.')
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
                        mix <- dim(subset(data,data$mix>1))[1]
                        com <- dim(subset(data, data$Tc ==2 | data$Hc ==2))[1]
                        nT <- dim(subset(data, data$TB>0))[1]
                        nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                        nH <- dim(subset(data, data$HB>0))[1]
                        nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                        
                        number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Trichuris$ $trichiura$ infections were observed in', nT, 'subjects (', round(100*nT/n,1), 'percent ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'percent ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nT2, 'cases of $T.$ $trichiura$, and ', nH2, 'cases of hookworms.')
                    }
                    else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            nR <- dim(subset(data, data$RB>0))[1]
                            nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                            number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ). In total,', nR2, 'infected subjects provided a sample at both baseline and follow-up.')
                        }
                        else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
                                data$TB <-  data[,input$Tbas]  
                                data$TF <-  data[,input$Tfol]  
                                nT <- dim(subset(data, data$TB>0))[1]
                                nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                                number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Trichuris$ $trichiura$ infections were observed in', nT, 'subjects (', round(100*nT/n,1), 'percent ). In total,', nT2, 'infected subjects provided a sample at both baseline and follow-up.')
                            }
                            
                            else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2){
                                    data$HB <-  data[,input$Hbas]  
                                    data$HF <-  data[,input$Hfol]  
                                    data$Hp <- ifelse(data$HB>0,1,0) 
                                    data$Hf <- ifelse(data$HF>=0,1,0) 
                                    data$Hc <- data$Hp + data$Hf
                                    nH <- dim(subset(data, data$HB>0))[1]
                                    nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                                    number <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. Hookworms infections were observed in', nH, 'subjects (', round(100*nH/n,1), 'percent ). In total,', nH2, 'infected subjects provided a sample at both baseline and follow-up.')
                                }
                                else{number <- paste('No egg count data was provided.')}
                            }
                        }
                    }  
                }   
            }
        } 
    }
}


#' @title Analysis of Helminthiasis (intensity)
#' @description Analysis of Helminthiasis (intensity)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param ... not used yet
#' @export
#' @return TODO
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_intensity(x)
paradrug_helminthiasis_intensity <- function(object, 
                                     Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                     Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                     Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                     ...){
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol)
    
    n <- nrow(data)
    
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
    
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {intense <- paste('No egg count data was provided.')}
    else {
        
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
        {
            data$RB <-  data[,input$Rbas]  
            data$TB <-  data[,input$Tbas]
            data$HB <-  data[,input$Hbas] 
            data$RF <-  data[,input$Rfol]  
            data$TF <-  data[,input$Tfol] 
            data$HF <-  data[,input$Hfol] 
            R <- subset(data, data$RB> 0 & data$RF >= 0)
            Tr <- subset(data, data$TB> 0 & data$TF >= 0)
            H <- subset(data, data$HB> 0 & data$HF >= 0)
            
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
            
            intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean 
$T.$ $trichiura$ egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') 
                  eggs per gram of stool. Low, moderate and high-intensity $A.$ $lumbricoides$ infections were observed in', NRL,'(',round(100*NRL/nR,1),'percent ),'
                             , NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects,  respectively.
            For $T.$ $trichiura$, these numbers were', NTL,'(',round(100*NTL/nT,1),'percent ),', NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ),
                  respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'percent ),', NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/n,1),
                             'percent ), respectively.')
        }
        
        
        else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2)
            {
                data$RB <-  data[,input$Rbas]  
                data$TB <-  data[,input$Tbas]
                data$RF <-  data[,input$Rfol]  
                data$TF <-  data[,input$Tfol] 
                R <- subset(data, data$RB>0 & data$RF >= 0)
                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                
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
                
                
                intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean $T.$ $trichiura$
egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity $A.$ $lumbricoides$ infections were observed 
                    in', NRL,'(',round(100*NRL/nR,1),'percent ),' , NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects,  respectively.
            For $T.$ $trichiura$, these numbers were', NTL,'(',round(100*NTL/nT,1),'percent ),', NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ),
                    respectively.')
            }
            
            else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2)
                {
                    data$RB <-  data[,input$Rbas]  
                    data$HB <-  data[,input$Hbas] 
                    data$RF <-  data[,input$Rfol]  
                    data$HF <-  data[,input$Hfol] 
                    R <- subset(data, data$RB>0 & data$RF >= 0)
                    H <- subset(data, data$HB>0 & data$HF >= 0)
                    
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
                    
                    intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. 
                      The mean hookworm egg counts equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. 
                      Low, moderate and high - intensity $A.$ $lumbricoides$ infections were observed in', NRL,'(',round(100*NRL/nR,1),'percent ),', 
                                     NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects, respectively. 
                      For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'percent ),', NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/n,1),'percent ),
                      respectively.')
                }
                
                
                else{
                    if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2)
                    {
                        data$TB <-  data[,input$Tbas]
                        data$HB <-  data[,input$Hbas] 
                        data$TF <-  data[,input$Tfol] 
                        data$HF <-  data[,input$Hfol] 
                        Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                        H <- subset(data, data$HB>0 & data$HF >= 0)
                        
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
                        
                        intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $T.$ $trichiura$ egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. 
                        The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensity $T.$ $trichiura$
                        infections were observed in', NTL,'(',round(100*NTL/nT,1),'percent ),', NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ) subjects, 
                        respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'percent ),', NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/nH,1),'percent ), respectively.')
                    }
                    else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
                        {
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            R <- subset(data, data$RB>0 & data$RF >= 0)
                            NRH <- sum(ifelse(R$RB>=50000,1,0))
                            NRM <- sum(ifelse(R$RB>=5000 & R$RB<50000,1,0))
                            NRL <- sum(ifelse(R$RB>0 & R$RB<5000,1,0))
                            q25R <- round(quantile(R$RB, probs=c(0.25)),1)
                            q75R <- round(quantile(R$RB, probs=c(0.75)),1) 
                            MR <- round(mean(R$RB),1)
                            nR <- length(R$RB)
                            intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NRL,'(',round(100*NRL/nR,1),'percent ),'
                                             , NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects, respectively.')
                        }
                        
                        else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                            {
                                data$TB <-  data[,input$Tbas]
                                data$TF <-  data[,input$Tfol] 
                                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                NTH <- sum(ifelse(Tr$TB>=5000,1,0))
                                NTM <- sum(ifelse(Tr$TB>=1000 & Tr$TB<5000,1,0))
                                NTL <- sum(ifelse(Tr$TB>=1 & Tr$TB<1000,1,0)) 
                                q25T <- round(quantile(Tr$TB, probs=c(0.25)),1)
                                q75T <- round(quantile(Tr$TB, probs=c(0.75)),1) 
                                MT <- round(mean(Tr$TB),1)
                                nT <- length(Tr$TB)
                                intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
$T.$ $trichiura$ egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NTL,'(',round(100*NTL/nT,1),'percent ),'
                                                 , NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ) subjects,  respectively.')
                            }
                            
                            else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                                {
                                    data$HB <-  data[,input$Hbas] 
                                    data$HF <-  data[,input$Hfol] 
                                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                                    NHH <- sum(ifelse(H$HB>=4000,1,0))
                                    NHM <- sum(ifelse(H$HB>=2000 & H$HB<4000,1,0))
                                    NHL <- sum(ifelse(H$HB>=1 & H$HB<2000,1,0)) 
                                    
                                    q25H <- round(quantile(H$HB, probs=c(0.25)),1)
                                    q75H <- round(quantile(H$HB, probs=c(0.75)),1)
                                    
                                    MH <- round(mean(H$HB),1)
                                    nH <- length(H$HB)
                                    
                                    intense <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensty infections were observed in', NHL,'(',round(100*NHL/nH,1),'percent ),'
                                                     , NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/nH,1),'percent ) subjects,  respectively.')
                                }
                                
                                else{intense <- paste('No egg count data was provided.')}
                            }
                        }
                    }  
                }   
            }
        } 
    }
}


#' @title Plot of eggcount of Analysis of Helminthiasis
#' @description Plot of eggcount of Helminthiasis
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param ... not used yet
#' @export
#' @return TODO
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- plot_paradrug_helminthiasis_eggcount(x)
plot_paradrug_helminthiasis_eggcount <- function(object, 
                                             Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                             Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                             Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                             ...){
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol)
    
    n <- nrow(data)
    
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
        R <- subset(data, data$RB> 0 & data$RF >= 0)
        Tr <- subset(data, data$TB> 0 & data$TF >= 0)
        H <- subset(data, data$HB> 0 & data$HF >= 0)
        
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
                R <- subset(data, data$RB> 0 & data$RF >= 0)
                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                
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
                    R <- subset(data, data$RB> 0 & data$RF >= 0)
                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                    
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
                        Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                        H <- subset(data, data$HB> 0 & data$HF >= 0)
                        
                        par(mfrow=c(1,2))
                        hist(Tr$TB, col = "#EB4C4C", main=expression(italic(Trichuris~trichiura)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
                        hist(H$HB, col = "#EB4C4C", main='Hookworm', freq=T, ylab='Number of subjects',xlab='Fecal egg counts (eggs per gram of stool)')
                    }
                    else{  
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2)
                        {
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            R <- subset(data, data$RB> 0 & data$RF >= 0)
                            
                            par(mfrow=c(1,1))
                            hist(R$RB, col = "#EB4C4C", main=expression(italic(Ascaris~lumbricoides)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
                        }
                        else{  
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                            {
                                data$TB <-  data[,input$Tbas]
                                data$TF <-  data[,input$Tfol] 
                                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                par(mfrow=c(1,1))
                                hist(Tr$TB, col = "#EB4C4C", main=expression(italic(Trichuris~trichiura)), ylab='Number of subjects',freq=T, xlab='Fecal egg counts (eggs per gram of stool)')
                            }
                            else{  
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                                {
                                    data$HB <-  data[,input$Hbas] 
                                    data$HF <-  data[,input$Hfol] 
                                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                                    
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