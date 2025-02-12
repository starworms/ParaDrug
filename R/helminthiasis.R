#' @title Analysis of Helminthiasis (subjects)
#' @description Analysis of Helminthiasis (subjects)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param type either 'latex' or 'markdown' to return a latex representation of the analysis or a markdown version. Defaults to 'latex'.
#' @param ... not used yet
#' @export
#' @return a character string with information on the number of subjects in the trial
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_n(x)
#' p <- paradrug_helminthiasis_n(x, type = "markdown")
paradrug_helminthiasis_n <- function(object, 
                                   Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                   Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                   Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                   type = c("latex", "markdown"),
                                   ...){
    type <- match.arg(type)
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
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {
        number <- paste('No egg count data was provided.')
        number_md <- paste('Please provide egg count data.')
    }else {
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
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
            
            number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ), $T.$ $trichiura$ infections in', nT, 'subjects (', round(100*nT/n,1),'percent ) and hookworms in', nH, '(',round (100*nH/n,1),'percent ) subjects. Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). Complete data were available for', com, 'subjects, including', nR2, 'cases of $A.$ $lumbricoides$,', nT2, 'cases of $T.$ $trichiura$, and',nH2, 'cases of hookworms.')
            number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ), <em>T. trichiura</em> infections in', nT, 'subjects (', round(100*nT/n,1),'% ) and hookworms in', nH, '(',round (100*nH/n,1),'% ) subjects. Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). Complete data were available for', com, 'subjects, including', nR2, 'cases of <em>A. lumbricoides</em>,', nT2, 'cases of <em>T. trichiura</em>, and',nH2, 'cases of hookworms.')
        }else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2){
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
                
                number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ), $T.$ $trichiura$ infections in', nT, 'subjects (', round(100*nT/n,1),'percent ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of $A.$ $lumbricoides$, and ', nT2, 'cases of $T.$ $trichiura$.')
                number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ), <em>T. trichiura</em> infections in', nT, 'subjects (', round(100*nT/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of <em>A. lumbricoides</em>, and ', nT2, 'cases of <em>T. trichiura</em>.')
            }else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2){
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
                    
                    number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'percent ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of $A.$ $lumbricoides$, and ', nH2, 'cases of hookworms.')
                    number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ), <em>T. trichiura</em> infections in', nT, 'subjects (', round(100*nT/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nR2, 'cases of <em>A. lumbricoides</em>, and ', nT2, 'cases of <em>T. trichiura</em>.')
                }else{
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
                        
                        number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Trichuris$ $trichiura$ infections were observed in', nT, 'subjects (', round(100*nT/n,1), 'percent ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'percent ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), 'percent ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nT2, 'cases of $T.$ $trichiura$, and ', nH2, 'cases of hookworms.')
                        number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Trichuris trichiura</em> infections were observed in', nT, 'subjects (', round(100*nT/n,1), '% ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nT2, 'cases of <em>T. trichiura</em>, and ', nH2, 'cases of hookworms.')
                    }else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            nR <- dim(subset(data, data$RB>0))[1]
                            nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                            number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Ascaris$ $lumbricoides$ infections were observed in', nR, 'subjects (', round(100*nR/n,1), 'percent ). In total,', nR2, 'infected subjects provided a sample at both baseline and follow-up.')
                            number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ). In total,', nR2, 'infected subjects provided a sample at both baseline and follow-up.')
                        }else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
                                data$TB <-  data[,input$Tbas]  
                                data$TF <-  data[,input$Tfol]  
                                nT <- dim(subset(data, data$TB>0))[1]
                                nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                                number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. $Trichuris$ $trichiura$ infections were observed in', nT, 'subjects (', round(100*nT/n,1), 'percent ). In total,', nT2, 'infected subjects provided a sample at both baseline and follow-up.')
                                number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Trichuris trichiura</em> infections were observed in', nT, 'subjects (', round(100*nT/n,1), '% ). In total,', nT2, 'infected subjects provided a sample at both baseline and follow-up.')
                            }else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2){
                                    data$HB <-  data[,input$Hbas]  
                                    data$HF <-  data[,input$Hfol]  
                                    data$Hp <- ifelse(data$HB>0,1,0) 
                                    data$Hf <- ifelse(data$HF>=0,1,0) 
                                    data$Hc <- data$Hp + data$Hf
                                    nH <- dim(subset(data, data$HB>0))[1]
                                    nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                                    number    <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. Hookworms infections were observed in', nH, 'subjects (', round(100*nH/n,1), 'percent ). In total,', nH2, 'infected subjects provided a sample at both baseline and follow-up.')
                                    number_md <- paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. Hookworms infections were observed in', nH, 'subjects (', round(100*nH/n,1), '% ). In total,', nH2, 'infected subjects provided a sample at both baseline and follow-up.')
                                }else{
                                    number    <- paste('No egg count data was provided.')
                                    number_md <- paste('Please match egg counting data.')
                                }
                            }
                        }
                    }  
                }   
            }
        } 
    }
    if(type == "markdown"){
        number <- number_md
    }
    number
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
#' @param type either 'latex' or 'markdown' to return a latex representation of the analysis or a markdown version. Defaults to 'latex'.
#' @param ... not used yet
#' @export
#' @return a character string with information on the intensity of the eggs in the trial
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_intensity(x)
#' p <- paradrug_helminthiasis_intensity(x, type = "markdown")
paradrug_helminthiasis_intensity <- function(object, 
                                     Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                     Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                     Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                     type = c("latex", "markdown"),
                                     ...){
    type <- match.arg(type)
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
    
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {
        intense    <- paste('No egg count data was provided.')
        intense_md <- paste('Please provide egg count data.')
    }else {
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
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
            
            intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean 
$T.$ $trichiura$ egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') 
                  eggs per gram of stool. Low, moderate and high-intensity $A.$ $lumbricoides$ infections were observed in', NRL,'(',round(100*NRL/nR,1),'percent ),'
                             , NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects,  respectively.
            For $T.$ $trichiura$, these numbers were', NTL,'(',round(100*NTL/nT,1),'percent ),', NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ),
                  respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'percent ),', NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/n,1),
                             'percent ), respectively.')
            intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean 
<em>T. trichiura</em> egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') 
                  eggs per gram of stool. Low, moderate and high-intensity <em>A. lumbricoides</em> infections were observed in', NRL,'(',round(100*NRL/nR,1),'% ),'
                                , NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects,  respectively.
            For <em>T. trichiura</em>, these numbers were', NTL,'(',round(100*NTL/nT,1),'% ),', NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ),
                  respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'% ),', NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/n,1),
                                '% ), respectively.')
        }else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2){
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
                
                
                intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean $T.$ $trichiura$
egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity $A.$ $lumbricoides$ infections were observed 
                    in', NRL,'(',round(100*NRL/nR,1),'percent ),' , NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects,  respectively.
            For $T.$ $trichiura$, these numbers were', NTL,'(',round(100*NTL/nT,1),'percent ),', NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ),
                    respectively.')
                intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. The mean <em>T. trichiura</em>
egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity <em>A. lumbricoides</em> infections were observed 
                    in', NRL,'(',round(100*NRL/nR,1),'% ),' , NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects,  respectively.
            For <em>T. trichiura</em>, these numbers were', NTL,'(',round(100*NTL/nT,1),'% ),', NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ),
                    respectively.')
            }else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2){
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
                    
                    intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. 
                      The mean hookworm egg counts equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. 
                      Low, moderate and high - intensity $A.$ $lumbricoides$ infections were observed in', NRL,'(',round(100*NRL/nR,1),'percent ),', 
                                     NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects, respectively. 
                      For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'percent ),', NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/n,1),'percent ),
                      respectively.')
                    intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. 
                      The mean hookworm egg counts equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. 
                      Low, moderate and high - intensity <em>A. lumbricoides</em> infections were observed in', NRL,'(',round(100*NRL/nR,1),'% ),', 
                                        NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects, respectively. 
                      For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'% ),', NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/n,1),'% ),
                      respectively.')
                }else{
                    if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
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
                        
                        intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) $T.$ $trichiura$ egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. 
                        The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensity $T.$ $trichiura$
                        infections were observed in', NTL,'(',round(100*NTL/nT,1),'percent ),', NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ) subjects, 
                        respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'percent ),', NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/nH,1),'percent ), respectively.')
                        intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figures below. 
The mean (25th quantile; 75th quantile) <em>T. trichiura</em> egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. 
                        The mean hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensity <em>T. trichiura</em>
                        infections were observed in', NTL,'(',round(100*NTL/nT,1),'% ),', NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ) subjects, 
                        respectively. For hookworms, these numbers were', NHL,'(',round(100*NHL/nH,1),'% ),', NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/nH,1),'% ), respectively.')
                    }else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
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
                            intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) $A.$ $lumbricoides$ egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NRL,'(',round(100*NRL/nR,1),'percent ),'
                                             , NRM,'(',round(100*NRM/nR,1),'percent ) and', NRH, '(',round(100*NRH/nR,1),'percent ) subjects, respectively.')
                            intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. 
The mean (25th quantile; 75th quantile) <em>A. lumbricoides</em> egg count equaled',MR,'(',q25R,';',q75R,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NRL,'(',round(100*NRL/nR,1),'% ),'
                                                , NRM,'(',round(100*NRM/nR,1),'% ) and', NRH, '(',round(100*NRH/nR,1),'% ) subjects, respectively.')
                        }else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
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
                                intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
$T.$ $trichiura$ egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NTL,'(',round(100*NTL/nT,1),'percent ),'
                                                 , NTM,'(',round(100*NTM/nT,1),'percent ) and', NTH, '(',round(100*NTH/nT,1),'percent ) subjects,  respectively.')
                                intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
<em>T. trichiura</em> egg count equaled',MT,'(',q25T,';',q75T,') eggs per gram of stool. Low, moderate and high-intensity infections were observed in', NTL,'(',round(100*NTL/nT,1),'% ),'
                                                    , NTM,'(',round(100*NTM/nT,1),'% ) and', NTH, '(',round(100*NTH/nT,1),'% ) subjects,  respectively.')
                            }else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2){
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
                                    
                                    intense    <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensty infections were observed in', NHL,'(',round(100*NHL/nH,1),'percent ),'
                                                     , NHM,'(',round(100*NHM/nH,1),'percent ) and', NHH, '(',round(100*NHH/nH,1),'percent ) subjects,  respectively.')
                                    intense_md <- paste('The distribution of the baseline egg counts across the subjects who completed the trial is illustrated in the figure below. The mean (25th quantile; 75th quantile) 
hookworm egg count equaled',MH,'(',q25H,';',q75H,') eggs per gram of stool. Low, moderate and high-intensty infections were observed in', NHL,'(',round(100*NHL/nH,1),'% ),'
                                                        , NHM,'(',round(100*NHM/nH,1),'% ) and', NHH, '(',round(100*NHH/nH,1),'% ) subjects,  respectively.')
                                }else{
                                    intense    <- paste('No egg count data was provided.')
                                    intense_md <- paste('Please match egg counting data.')
                                }
                            }
                        }
                    }  
                }   
            }
        } 
    }
    if(type == "markdown"){
        intense <- intense_md
    }
    intense
    
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
#' @return a plot with the egg counts in the trial as returned by \code{\link[graphics]{hist}}
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- plot_paradrug_helminthiasis_eggcount(x)
#' p <- plot_paradrug_helminthiasis_eggcount(x, 
#'   Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
#'   Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG",  
#'   Hbas = "Not recorded", Hfol = "Not recorded")
#' p <- plot_paradrug_helminthiasis_eggcount(x, 
#'   Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
#'   Tbas = "Not recorded", Tfol = "Not recorded",  
#'   Hbas = "Not recorded", Hfol = "Not recorded")
#' 
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


#' @title Analysis of Helminthiasis (follow)
#' @description Analysis of Helminthiasis (follow)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param followup column in name in object$data for Number of days between the baseline and the follow-up survey
#' @param type either 'latex' or 'markdown' to return a latex representation of the analysis or a markdown version. Defaults to 'latex'.
#' @param ... not used yet
#' @export
#' @return a character string indicating the analysis of the follow up period and basic statistics of the eggs
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_follow(x)
#' p <- paradrug_helminthiasis_follow(x, type = "markdown")
paradrug_helminthiasis_follow <- function(object, 
                                          Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                          Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                          Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                          followup = "Age",
                                          type = c("latex", "markdown"),
                                          ...){
    type <- match.arg(type)
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol,
                  followup = followup)
    
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
    data$FU <- ifelse(input$followup=='Not recorded',rep(-2,n), ifelse(data[,input$followup]>=0,1,0))
    
    if(mean(data$FU)==-2) {
        follow    <- paste('No egg count data was provided.')
        follow_md <- 'Please match follow-up period data'
    }else {
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2)){
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
            
            data2 <- subset(data, data$Rc==2 | data$Tc ==2 | data$Hc ==2)
            min <- round(quantile(data2$fol, probs=c(0)),1)
            max <- round(quantile(data2$fol, probs=c(1)),1)
            med <- round(quantile(data2$fol, probs=c(0.50)),1)
            nc <- length(data2$fol)
            ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
            follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
            follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
        }else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$FU>-2)){
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
                
                data2 <- subset(data, data$Rc==2 | data$Tc ==2)
                min <- round(quantile(data2$fol, probs=c(0)),1)
                max <- round(quantile(data2$fol, probs=c(1)),1)
                med <- round(quantile(data2$fol, probs=c(0.50)),1)
                nc <- length(data2$fol)
                ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                
            }else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2)){
                    
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
                    data2 <- subset(data, data$Rc==2 | data$Hc ==2)
                    min <- round(quantile(data2$fol, probs=c(0)),1)
                    max <- round(quantile(data2$fol, probs=c(1)),1)
                    med <- round(quantile(data2$fol, probs=c(0.50)),1)
                    nc <- length(data2$fol)
                    ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                    
                    follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                    follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                }else{
                    if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2)){
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
                        
                        data2 <- subset(data, data$Tc ==2 | data$Hc ==2)
                        min <- round(quantile(data2$fol, probs=c(0)),1)
                        max <- round(quantile(data2$fol, probs=c(1)),1)
                        med <- round(quantile(data2$fol, probs=c(0.50)),1)
                        nc <- length(data2$fol)
                        ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                        follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                        follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.') 
                    }else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2 & mean(data$FU>-2)){
                            data$fol <-  data[,input$followup] 
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            data$Rp <- ifelse(data$RB>0,1,0) 
                            data$Rf <- ifelse(data$RF>=0,1,0) 
                            data$Rc <- data$Rp + data$Rf
                            data2 <- subset(data, data$Rc==2)
                            min <- round(quantile(data2$fol, probs=c(0)),1)
                            max <- round(quantile(data2$fol, probs=c(1)),1)
                            med <- round(quantile(data2$fol, probs=c(0.50)),1)
                            nc <- length(data2$fol)
                            ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                            follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                            follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                        }else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2 & mean(data$FU>-2)){
                                data$fol <-  data[,input$followup] 
                                data$TB <-  data[,input$Tbas] 
                                data$TF <-  data[,input$Tfol]
                                data$Tp <- ifelse(data$TB>0,1,0)
                                data$Tf <- ifelse(data$TF>=0,1,0) 
                                data$Tc <- data$Tp + data$Tf
                                data2 <- subset(data,data$Tc ==2)
                                min <- round(quantile(data2$fol, probs=c(0)),1)
                                max <- round(quantile(data2$fol, probs=c(1)),1)
                                med <- round(quantile(data2$fol, probs=c(0.50)),1)
                                nc <- length(data2$fol)
                                ncont <- sum(ifelse(data2$fol>=14 & data2$fol<=21,1,0))
                                follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')  
                                follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.')
                            }else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2 & mean(data$FU>-2)){
                                    data$fol <-  data[,input$followup] 
                                    data$HB <-  data[,input$Hbas] 
                                    data$HF <-  data[,input$Hfol]
                                    data$Hp <- ifelse(data$HB>0,1,0)
                                    data$Hf <- ifelse(data$HF>=0,1,0) 
                                    data$Hc <- data$Hp + data$Hf
                                    
                                    data2 <- subset(data, data$Hc ==2)
                                    min <- round(quantile(data2$fol, probs=c(0)),1)
                                    max <- round(quantile(data2$fol, probs=c(1)),1)
                                    med <- round(quantile(data2$fol, probs=c(0.50)),1)
                                    nc <- length(data2$fol)
                                    ncont <- sum(ifelse(data2$fol>=7 & data2$fol<=21,1,0))
                                    follow    <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'percent ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.') 
                                    follow_md <- paste('The follow-up period ranged from',min,'to', max, 'days, with a median of',med,'days. A total of',ncont,'out of ',nc,'(',round(100*(ncont/nc),1),'% ) 
        complete cases were re-sampled between 14 and 21 days after drug administration.') 
                                }else{
                                    follow    <- paste('No egg count data was provided.')
                                    follow_md <- paste('Please match egg counting data.')
                                }
                            }
                        }
                    }  
                }   
            }
        } 
    }
    if(type == "markdown"){
        follow <- follow_md
    }
    follow
}



#' @title Analysis of Helminthiasis (egg reduction)
#' @description Analysis of Helminthiasis (egg reduction)
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param drug either "Albendazole (1x 400 mg)", "Mebendazole (1x 500 mg)" or "Other"
#' @param type either 'latex' or 'markdown' to return a latex representation of the analysis or a markdown version. Defaults to 'latex'.
#' @param ... not used yet
#' @export
#' @return a character string indicating the analysis of the egg count reduction in the trial
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_eggreduction(x, drug = "Albendazole (1x 400 mg)")
#' p <- paradrug_helminthiasis_eggreduction(x, drug = "Mebendazole (1x 500 mg)")
#' p <- paradrug_helminthiasis_eggreduction(x, drug = "Other")
#' p <- paradrug_helminthiasis_eggreduction(x, drug = "Albendazole (1x 400 mg)",
#'                                          type = "markdown")
#' p <- paradrug_helminthiasis_eggreduction(x, drug = "Mebendazole (1x 500 mg)",
#'                                          type = "markdown")
#' p <- paradrug_helminthiasis_eggreduction(x, drug = "Other",
#'                                          type = "markdown")
paradrug_helminthiasis_eggreduction <- function(object, 
                                                Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                                Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                                Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                                drug = c("Albendazole (1x 400 mg)", "Mebendazole (1x 500 mg)", "Other"),
                                                type = c("latex", "markdown"),
                                                ...){
    type <- match.arg(type)
    drug <- match.arg(drug)
    drug <- list("Albendazole (1x 400 mg)" = 1, "Mebendazole (1x 500 mg)" = 2, "Other" = 3)[[drug]]
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol,
                  STHdrug = drug)
    
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
    
    if(mean(data$inf)==0 | mean(data$inf2)==0 ) {
        err    <- paste('No egg count data was provided.')
        err_md <- paste('Please provide egg count data.')
    }else {
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
            data$RB <-  data[,input$Rbas]  
            data$TB <-  data[,input$Tbas]
            data$HB <-  data[,input$Hbas] 
            data$RF <-  data[,input$Rfol]  
            data$TF <-  data[,input$Tfol] 
            data$HF <-  data[,input$Hfol] 
            R <- subset(data, data$RB> 0 & data$RF >= 0)
            Tr <- subset(data, data$TB> 0 & data$TF >= 0)
            H <- subset(data, data$HB> 0 & data$HF >= 0)
            
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
                err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For $T.$ $trichiura$, the ERR equaled',round(100*ERRT,1),'percent (',round(100*LLT,1),';', round(100*ULT,1),').
For hookworms, the ERR equaled',round(100*ERRH,1),'percent (',round(100*LLH,1),';', round(100*ULH,1),').
The figures below classify the ERR estimates according to the WHO thresholds. 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', round(100*ULT,1),').
For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', round(100*ULH,1),').
The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
            }else{
                err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For $T.$ $trichiura$, the ERR equaled',round(100*ERRT,1),'percent (',round(100*LLT,1),';', 
                                round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'percent (',round(100*LLH,1),
                                ';', round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight lines represent the point 
                estimates.')
                err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', 
                                round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),
                                ';', round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
            }
        }else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2){
                data$RB <-  data[,input$Rbas]  
                data$TB <-  data[,input$Tbas]
                data$RF <-  data[,input$Rfol]  
                data$TF <-  data[,input$Tfol] 
                R <- subset(data, data$RB> 0 & data$RF >= 0)
                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                
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
                    err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For $T.$ $trichiura$, the ERR equaled',round(100*ERRT,1),'percent (',round(100*LLT,1),';', round(100*ULT,1),'). The figures below classify the ERR estimates according to the WHO thresholds.  
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                    err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', round(100*ULT,1),'). The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                }else {
                    err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For $T.$ $trichiura$, the ERR equaled',round(100*ERRT,1),'percent (',round(100*LLT,1),';', 
                                     round(100*ULT,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight lines represent the point 
                estimates.')
                    err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                  For <em>T. trichiura</em>, the ERR equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', 
                                    round(100*ULT,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
                }
            }else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2){
                    data$RB <-  data[,input$Rbas]  
                    data$HB <-  data[,input$Hbas] 
                    data$RF <-  data[,input$Rfol]  
                    data$HF <-  data[,input$Hfol] 
                    R <- subset(data, data$RB> 0 & data$RF >= 0)
                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                    
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
                        err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',
                                     round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      For hookworms, the ERR equaled',round(100*ERRH,1),'percent (',round(100*LLH,1),';', round(100*ULH,1),'). The figures below classify the ERR estimates according to the WHO thresholds. 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                        err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',
                                        round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', round(100*ULH,1),'). The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                    }else { 
                        err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      For hookworms, the ERR equaled',round(100*ERRH,1),'percent (',round(100*LLH,1),';', 
                                          round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight lines represent the point 
                estimates.')
                        err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', 
                                        round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
                    }
                }else{
                    if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
                        data$TB <-  data[,input$Tbas]
                        data$HB <-  data[,input$Hbas] 
                        data$TF <-  data[,input$Tfol] 
                        data$HF <-  data[,input$Hfol] 
                        Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                        H <- subset(data, data$HB> 0 & data$HF >= 0)
                        
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
                            err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $T.$ $trichiura$ equaled',round(100*ERRT,1),'percent 
(',round(100*LLT,1),';', round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'percent (',round(100*LLH,1),';', round(100*ULH,1),'). 
The figures below classify the ERR estimates according to the WHO thresholds. 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                            err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>T. trichiura</em> equaled',round(100*ERRT,1),'% 
(',round(100*LLT,1),';', round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'% (',round(100*LLH,1),';', round(100*ULH,1),'). 
The figures below classify the ERR estimates according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                        }else{
                            err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the 
                                drug $T.$ $trichiura$ equaled',round(100*ERRT,1),'percent (',round(100*LLT,1),';',
                                         round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'percent 
                                (',round(100*LLH,1),';', round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight lines represent the point 
                estimates.')
                            err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the 
                                drug <em>T. trichiura</em> equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';',
                                            round(100*ULT,1),'). For hookworms, the ERR equaled',round(100*ERRH,1),'% 
                                (',round(100*LLH,1),';', round(100*ULH,1),'). The figures below illustrate the uncertainty around the ERR point estimates. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight lines represent the point 
                estimates.')
                        }
                    }else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            R <- subset(data, data$RB> 0 & data$RF >= 0)
                            
                            ERRR <- (1- mean(R$RF)/mean(R$RB))
                            term1R <- (mean(R$RF)/mean(R$RB))**2; term2R <- ifelse(mean(R$RF)==0,0,var(R$RF)/mean(R$RF)**2); term3R <- var(R$RB)/mean(R$RB)**2
                            term4R <- ifelse(mean(R$RF)==0,0,-2*cor(R$RB,R$RF)*sqrt(var(R$RF))*sqrt(var(R$RB))/(mean(R$RB)*mean(R$RF)))
                            VARR <-  term1R*(term2R+term3R+term4R); varR <- VARR / length(R$RB)
                            aR <- ((1-ERRR)**2)/varR; bR <- varR/(1-ERRR)
                            ULR <- 1-qgamma(0.025,shape = aR, scale = bR)
                            LLR <- 1-qgamma(0.975,shape = aR, scale = bR)
                            
                            if(input$STHdrug == 1 | input$STHdrug == 2){
                                err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      The figure below classifies the ERR estimate according to the WHO thresholds. 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                                err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), ';',round(100*ULR,1), ').
                      The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                            }else {
                                err    <- paste('The egg reduction rate (95 percent confidence intervals) of the drug 
                                  against $A.$ $lumbricoides$ equaled',round(100*ERRR,1),'percent (',round(100*LLR,1), 
                                             ';',round(100*ULR,1), '). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight line represents the point 
                estimate.')
                                err_md <- paste('The egg reduction rate (95% confidence intervals) of the drug 
                                  against <em>A. lumbricoides</em> equaled',round(100*ERRR,1),'% (',round(100*LLR,1), 
                                                ';',round(100*ULR,1), '). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
                            } 
                        }else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
                                data$TB <-  data[,input$Tbas]
                                data$TF <-  data[,input$Tfol] 
                                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                
                                ERRT <- (1- mean(Tr$TF)/mean(Tr$TB))
                                term1T <- (mean(Tr$TF)/mean(Tr$TB))**2; term2T <- ifelse(mean(Tr$TF)==0,0,var(Tr$TF)/mean(Tr$TF)**2); term3T <- var(Tr$TB)/mean(Tr$TB)**2
                                term4T <- ifelse(mean(Tr$TF)==0,0,-2*cor(Tr$TB,Tr$TF)*sqrt(var(Tr$TF))*sqrt(var(Tr$TB))/(mean(Tr$TB)*mean(Tr$TF)))
                                VART <-  term1T*(term2T+term3T+term4T); varT <- VART / length(Tr$TB)
                                aT <- ((1-ERRT)**2)/varT; bT <- varT/(1-ERRT)
                                ULT <- 1-qgamma(0.025,shape = aT, scale = bT)
                                LLT <- 1-qgamma(0.975,shape = aT, scale = bT)
                                if(input$STHdrug == 1 | input$STHdrug == 2){
                                    err    <- paste('The egg reduction rate (ERRR; 95 percent confidence intervals) of the drug against $T.$ $trichiura$ equaled',round(100*ERRT,1),
                                                 'percent (',round(100*LLT,1),';', round(100*ULT,1),'). The figure below classifies the ERR estimate according to the WHO thresholds. 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                                    err_md <- paste('The egg reduction rate (ERRR; 95% confidence intervals) of the drug against <em>T. trichiura</em> equaled',round(100*ERRT,1),
                                                    '% (',round(100*LLT,1),';', round(100*ULT,1),'). The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                                } else { 
                                    err    <- paste('The egg reduction rate (95 percent confidence intervals) of the drug against 
                              $T.$ $trichiura$ equaled',round(100*ERRT,1),'percent (',round(100*LLT,1),';', 
                                                 round(100*ULT,1),'). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight line represents the point 
                estimate.')
                                    err_md <- paste('The egg reduction rate (95% confidence intervals) of the drug against 
                              <em>T. trichiura</em> equaled',round(100*ERRT,1),'% (',round(100*LLT,1),';', 
                                                    round(100*ULT,1),'). The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
                                }
                            }else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2){
                                    data$HB <-  data[,input$Hbas] 
                                    data$HF <-  data[,input$Hfol] 
                                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                                    
                                    ERRH <- (1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                                    term1H <- (mean(H$HF)/mean(H$HB))**2; term2H <- ifelse(mean(H$HF)==0,0,var(H$HF)/mean(H$HF)**2); term3H <- var(H$HB)/mean(H$HB)**2
                                    term4H <- ifelse(mean(H$HF)==0,0,-2*cor(H$HB,H$HF)*sqrt(var(H$HF))*sqrt(var(H$HB))/(mean(H$HB)*mean(H$HF)))
                                    VARH <-  term1H*(term2H+term3H+term4H); varH <- VARH / length(H$HB)
                                    aH <- ((1-ERRH)**2)/varH; bH <- varH/(1-ERRH)
                                    ULH <- 1-qgamma(0.025,shape = aH, scale = bH)
                                    LLH <- 1-qgamma(0.975,shape = aH, scale = bH)  
                                    if(input$STHdrug == 1 | input$STHdrug == 2){ 
                                        err    <- paste('The egg reduction rate (ERR; 95 percent confidence intervals) of the drug against hookworms equaled',round(100*ERRH,1),'percent
(',round(100*LLH,1),';', round(100*ULH,1),'). 
The figure below classifies the ERR estimate according to the WHO thresholds. 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                                        err_md <- paste('The egg reduction rate (ERR; 95% confidence intervals) of the drug against hookworms equaled',round(100*ERRH,1),'%
(',round(100*LLH,1),';', round(100*ULH,1),'). 
The figure below classifies the ERR estimate according to the WHO thresholds (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). 
Any ERR estimate in the green zone indicates that the efficacy of the drug is satisfactory, any value in the grey zone indicates that the efficacy is doubtful 
and any value in the red zone indicates that the efficacy is reduced. The black vertical line represents the ERR estimate of the drug administered in this trial.')
                                    }else{
                                        err    <- paste('The egg reduction rate (95 percent confidence intervals) of the drug against hookworms equaled',round(100*ERRH,1),
                                                     'percent (',round(100*LLH,1),';', round(100*ULH,1),').  The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95 percent confidence intervals, whereas the straight line represents the point 
                estimate.')
                                        err_md <- paste('The egg reduction rate (95% confidence intervals) of the drug against hookworms equaled',round(100*ERRH,1),
                                                        '% (',round(100*LLH,1),';', round(100*ULH,1),').  The figure below illustrates the uncertainty around the ERR point estimate. 
                The dashed lines represent the limits of the 95% confidence intervals, whereas the straight line represents the point 
                estimate.')
                                    }                            
                                }else{
                                    err    <- paste('No egg count data was provided.')
                                    err_md <- paste('Please match egg counting data.')
                                }
                            }
                        }
                    }  
                }   
            }
        } 
    }
    if(type == "markdown"){
        err <- err_md
    }
    err
}

#' @title Plot of eggcount reduction of Helminthiasis
#' @description Plot of eggcount reduction of Helminthiasis
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param drug either "Albendazole (1x 400 mg)", "Mebendazole (1x 500 mg)" or "Other"
#' @param ... not used yet
#' @export
#' @return a plot of the egg count reduction in the trial is generated
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- plot_paradrug_helminthiasis_eggcount_reduction(x, drug = "Albendazole (1x 400 mg)")
#' p <- plot_paradrug_helminthiasis_eggcount_reduction(x, drug = "Mebendazole (1x 500 mg)")
#' p <- plot_paradrug_helminthiasis_eggcount_reduction(x, drug = "Other")
#' p <- plot_paradrug_helminthiasis_eggcount_reduction(x, drug = "Albendazole (1x 400 mg)", 
#'   Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
#'   Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG",  
#'   Hbas = "Not recorded", Hfol = "Not recorded")
#' p <- plot_paradrug_helminthiasis_eggcount_reduction(x, drug = "Albendazole (1x 400 mg)", 
#'   Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
#'   Tbas = "Not recorded", Tfol = "Not recorded",  
#'   Hbas = "Not recorded", Hfol = "Not recorded")
plot_paradrug_helminthiasis_eggcount_reduction  <- function(object, 
                                                            Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                                            Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                                            Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                                            drug = c("Albendazole (1x 400 mg)", "Mebendazole (1x 500 mg)", "Other"),
                                                            ...){
    drug <- match.arg(drug)
    drug <- list("Albendazole (1x 400 mg)" = 1, "Mebendazole (1x 500 mg)" = 2, "Other" = 3)[[drug]]
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol,
                  STHdrug = drug)
    
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
                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                abline(v=100*ERRR, lwd=4) 
                
                T <- rgamma(10000,shape = aT, scale = bT)
                hist(100*(1-T), main=expression(italic(Trichuris~trichiura)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
                abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
                abline(v=100*ERRT, lwd=4) 
                
                H <- rgamma(10000,shape = aH, scale = bH)
                hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
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
            R <- subset(data, data$RB> 0 & data$RF >= 0)
            Tr <- subset(data, data$TB> 0 & data$TF >= 0)
            
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
                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                abline(v=100*ERRR, lwd=4)
                
                T <- rgamma(10000,shape = aT, scale = bT)
                hist(100*(1-T), main=expression(italic(Trichuris~trichiura)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
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
                R <- subset(data, data$RB> 0 & data$RF >= 0)
                H <- subset(data, data$HB> 0 & data$HF >= 0)
                
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
                        axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                        abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                        abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                        abline(v=100*ERRR, lwd=4)
                        
                        H <- rgamma(10000,shape = aH, scale = bH)
                        hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                        axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
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
                    Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                    
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
                            axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                            abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
                            abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
                            abline(v=100*ERRT, lwd=4)
                            
                            H <- rgamma(10000,shape = aH, scale = bH)
                            hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                            axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
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
                        R <- subset(data, data$RB> 0 & data$RF >= 0)
                        
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
                            axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                            abline(v=100*(1-qgamma(0.025,shape = aR, scale = bR)), lty=2,lwd=4)
                            abline(v=100*(1-qgamma(0.975,shape = aR, scale = bR)), lty=2,lwd=4)
                            abline(v=100*ERRR, lwd=4)
                        }
                        
                    } else{  
                        if(mean(data$Tb)>-2 & mean(data$Tf)>-2)
                        {
                            data$TB <-  data[,input$Tbas]
                            data$TF <-  data[,input$Tfol] 
                            Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                            
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
                                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                                abline(v=100*(1-qgamma(0.025,shape = aT, scale = bT)), lty=2,lwd=4)
                                abline(v=100*(1-qgamma(0.975,shape = aT, scale = bT)), lty=2,lwd=4)
                                abline(v=100*ERRT, lwd=4)     
                            }
                            
                        } else {  
                            if(mean(data$Hb)>-2 & mean(data$Hf)>-2)
                            {
                                data$HB <-  data[,input$Hbas] 
                                data$HF <-  data[,input$Hfol] 
                                H <- subset(data, data$HB> 0 & data$HF >= 0)
                                
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
                                        hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                                        axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
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



#' @title Conclusion of Helminthiasis
#' @description Conclusion of Helminthiasis
#' @param object an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}
#' @param Rbas column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Rfol column in name in object$data for Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tbas column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Tfol column in name in object$data for Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hbas column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param Hfol column in name in object$data for Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP
#' @param drug either "Albendazole (1x 400 mg)", "Mebendazole (1x 500 mg)" or "Other"
#' @param type either 'latex' or 'markdown' to return a latex representation of the analysis or a markdown version. Defaults to 'latex'.
#' @param ... not used yet
#' @export
#' @return a character string with the general conclusion of the trial
#' @export
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x <- read_paradrug_xls(path)
#' p <- paradrug_helminthiasis_conclusion(x, drug = "Albendazole (1x 400 mg)")
#' p <- paradrug_helminthiasis_conclusion(x, drug = "Mebendazole (1x 500 mg)")
#' p <- paradrug_helminthiasis_conclusion(x, drug = "Other")
#' p <- paradrug_helminthiasis_conclusion(x, drug = "Albendazole (1x 400 mg)", 
#'                                        type = "markdown")
#' p <- paradrug_helminthiasis_conclusion(x, drug = "Mebendazole (1x 500 mg)", 
#'                                        type = "markdown")
#' p <- paradrug_helminthiasis_conclusion(x, drug = "Other", 
#'                                        type = "markdown")
paradrug_helminthiasis_conclusion <- function(object, 
                                              Rbas = "BL_KK2_AL_EPG", Rfol = "FU_KK2_AL_EPG", 
                                              Tbas = "BL_KK2_TT_EPG", Tfol = "FU_KK2_TT_EPG", 
                                              Hbas = "BL_KK2_HW_EPG", Hfol = "FU_KK2_HW_EPG", 
                                              drug = c("Albendazole (1x 400 mg)", "Mebendazole (1x 500 mg)", "Other"),
                                              type = c("latex", "markdown"),
                                              ...){
    type <- match.arg(type)
    drug <- match.arg(drug)
    drug <- list("Albendazole (1x 400 mg)" = 1, "Mebendazole (1x 500 mg)" = 2, "Other" = 3)[[drug]]
    data <- object$data
    input <- list(Rbas = Rbas, Rfol = Rfol, 
                  Tbas = Tbas, Tfol = Tfol, 
                  Hbas = Hbas, Hfol = Hfol,
                  STHdrug = drug)
    
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
    
    if(mean(data$inf)==0 | mean(data$inf2)==0) {
        concl    <- paste('No egg count data was provided.')
        concl_md <- paste('Please provide egg count data.') 
    }else{
        if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
            data$RB <-  data[,input$Rbas]  
            data$TB <-  data[,input$Tbas]
            data$HB <-  data[,input$Hbas] 
            data$RF <-  data[,input$Rfol]  
            data$TF <-  data[,input$Tfol] 
            data$HF <-  data[,input$Hfol] 
            R <- subset(data, data$RB> 0 & data$RF >= 0)
            Tr <- subset(data, data$TB> 0 & data$TF >= 0)
            H <- subset(data, data$HB> 0 & data$HF >= 0)
            
            ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
            ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
            ERRH <- 100*(1- mean(H$HF)/mean(H$HB))
            
            if(input$STHdrug == 1) { 
                rstar<-ifelse(ERRR >=95,1,2)
                tstar<-ifelse(ERRT >=50,1,2)
                hstar<-ifelse(ERRH >=90,1,2)
                if(rstar == 1 & tstar == 2 & hstar == 2){
                    concl    <- paste('The efficacy of the drug administered is satisfactory for $A.$ $lumbricoides$, but is below the expected efficacy for 
both $T.$ $trichiura$ (50 precent) and hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                    concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em>, but is below the expected efficacy for 
both <em>T. trichiura</em> (50%) and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                }else{ 
                    if(rstar == 2 & tstar == 1 & hstar == 2){
                        concl    <- paste('The efficacy of the drug administered is satisfactory for $T.$ $trichiura$, but
is below the expected efficacy for both $A.$ $lumbricoides$ (95 precent) and hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em>, but
is below the expected efficacy for both <em>A. lumbricoides</em> (95%) and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                    }else{  
                        if(rstar == 2 & tstar == 2 & hstar == 1){
                            concl    <- paste('The efficacy of the drug administered is satisfactory for hookworms, but is below the expeceted efficacy for 
both $A.$ $lumbricoides$ (95 precent) and $T.$ $trichiura$ infections (50 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            concl_md <- paste('The efficacy of the drug administered is satisfactory for hookworms, but is below the expeceted efficacy for 
both <em>A. lumbricoides</em> (95%) and <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                        } else { 
                            if(rstar == 2 & tstar == 2 & hstar == 2){
                                concl    <- paste('The efficacy of the drug administered is below the expected efficacy for $A.$ $lumbricoides$ (95 precent), $T.$ $trichiura$ (50 precent) and hookworm
infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for <em>A. lumbricoides</em> (95%), <em>T. trichiura</em> (<50%) and hookworm
infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                            }else{ 
                                if(rstar == 1 & tstar == 1 & hstar == 2){
                                    concl    <- paste('The efficacy of the drug administered is satisfactory for both $A.$ $lumbricoides$ and $T.$ $trichiura$ infections, but is
below the expected efficcacy for hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and <em>T. trichiura</em> infections, but is
below the expected efficcacy for hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                }else{ 
                                    if(rstar == 1 & tstar == 2 & hstar == 1){
                                        concl    <- paste('The efficacy of the drug administered is satisfactory for both $A.$ $lumbricoides$ and hookworm infections, but is
below the expected efficacy for $T.$ $trichiura$ infections (50 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                        concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and hookworm infections, but is
below the expected efficacy for <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                    }else{ 
                                        if(rstar == 2 & tstar == 1 & hstar == 1){
                                            concl    <- paste('The efficacy of the drug administered is satisfactory for both $T.$ $trichiura$ and hookworm infections, but
is below the expected efficacy for $A.$ $lumbricoides$ infections (95 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                            concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>T. trichiura</em> and hookworm infections, but
is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                        }else{ 
                                            concl    <- paste('The efficacy of the drug administered is satisfactory for $A.$ $lumbricoides$, $T.$ $trichiura$ and hookworm infections.')
                                            concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em>, <em>T. trichiura</em> and hookworm infections.') 
                                        }
                                    }
                                }
                            }
                        }
                    } 
                }             
            }else{
                if(input$STHdrug == 2){
                    rstar<-ifelse(ERRR >=95,1,2)
                    tstar<-ifelse(ERRT >=50,1,2)
                    hstar<-ifelse(ERRH >=70,1,2)
                    if(rstar == 1 & tstar == 2 & hstar == 2){
                        concl    <- paste('The efficacy of the drug administered is satisfactory against $A.$ $lumbricoides$, but is below 
the expected efficacy for both $T.$ $trichiura$ (50 precent) and hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                        concl_md <- paste('The efficacy of the drug administered is satisfactory against <em>A. lumbricoides</em>, but is below 
the expected efficacy for both <em>T. trichiura</em> (50%) and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                    }else{ 
                        if(rstar == 2 & tstar == 1 & hstar == 2){
                            concl    <- paste('The efficacy of the drug administered is satisfactory against $T.$ $trichiura$, but is below
the expected efficacy for both $A.$ $lumbricoides$ (95 precent) and hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            concl_md <- paste('The efficacy of the drug administered is satisfactory against <em>T. trichiura</em>, but is below
the expected efficacy for both <em>A. lumbricoides</em> (95%) and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                        }else{  
                            if(rstar == 2 & tstar == 2 & hstar == 1){
                                concl    <- paste('The efficacy of the drug administered is satisfactory against hookorms, but is below the expeceted efficacy 
for both $A.$ $lumbricoides$ (95 precent) and $T.$ $trichiura$ infections (50 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                concl_md <- paste('The efficacy of the drug administered is satisfactory against hookorms, but is below the expeceted efficacy 
for both <em>A. lumbricoides</em> (95%) and <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                            }else{ 
                                if(rstar == 2 & tstar == 2 & hstar == 2){
                                    concl    <- paste('The efficacy of the drug administered is below the expected efficacy for $A.$ $lumbricoides$ (95 precent), 
$T.$ $trichiura$ (50 precent) and hookworm 
infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for <em>A. lumbricoides</em> (95%), 
<em>T. trichiura</em> (50%) and hookworm 
infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                }else{ 
                                    if(rstar == 1 & tstar == 1 & hstar == 2){
                                        concl    <- paste('The efficacy of the drug administered is satisfactory against both $A.$ $lumbricoides$ and $T.$ $trichiura$ infections, but 
is below the expected efficcacy for hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                        concl_md <- paste('The efficacy of the drug administered is satisfactory against both <em>A. lumbricoides</em> and <em>T. trichiura</em> infections, but 
is below the expected efficcacy for hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                    }else  { 
                                        if(rstar == 1 & tstar == 2 & hstar == 1){
                                            concl    <- paste('The efficacy of the drug administered is satisfactory against both $A.$ $lumbricoides$ and hookworm infections, but 
is below the expected efficacy for $T.$ $trichiura$ infections (50 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                            concl_md <- paste('The efficacy of the drug administered is satisfactory against both <em>A. lumbricoides</em> and hookworm infections, but 
is below the expected efficacy for <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                        }else{ 
                                            if(rstar == 2 & tstar == 1 & hstar == 1){
                                                concl    <- paste('The efficacy of the drug administered is satisfactory against both $T.$ $trichiura$ and hookworm infections, 
but is below the expected efficacy for $A.$ $lumbricoides$ infections (95 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                                concl_md <- paste('The efficacy of the drug administered is satisfactory against both <em>T. trichiura</em> and hookworm infections, 
but is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                            }else{ 
                                                concl    <- paste('The efficacy of the drug administered is satisfactory against $A.$ $lumbricoides$, $T.$ $trichiura$ and hookworm infections.')
                                                concl_md <- paste('The efficacy of the drug administered is satisfactory against <em>A. lumbricoides</em>, <em>T. trichiura</em> and hookworm infections.') 
                                            }
                                        }
                                    }
                                }
                            }
                        } 
                    }  
                    ## new drug 
                }else{
                    concl    <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')
                    concl_md <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.') 
                }
            }
            # end
        }else{ 
            if(mean(data$Rb)>-2 & mean(data$Tb)>-2 & mean(data$Rf)>-2 & mean(data$Tf)>-2){
                data$RB <-  data[,input$Rbas]  
                data$TB <-  data[,input$Tbas]
                data$RF <-  data[,input$Rfol]  
                data$TF <-  data[,input$Tfol] 
                R <- subset(data, data$RB> 0 & data$RF >= 0)
                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                
                ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
                ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
                
                if(input$STHdrug == 1 | input$STHdrug ==2){ 
                    rstar <-ifelse(ERRR >= 95,1,2)
                    tstar <- ifelse(ERRT >=50,1,2)
                    
                    if(rstar == 1 & tstar == 1){
                        concl    <- paste('The efficacy of the drug administered is satisfactory for both $A.$ $lumbricoides$ and $T.$ $trichiura$ infections.')
                        concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and <em>T. trichiura</em> infections.') 
                    }else{
                        if(rstar == 1 & tstar == 2){
                            concl    <- paste('The efficacy of the drug administered is satisfactory for $A.$ $lumbricoides$ infections, 
but is below the expected efficacy for $T.$ $trichiura$ infections (50 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em> infections, 
but is below the expected efficacy for <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                        }else{
                            if(rstar == 2 & tstar == 1) {
                                concl    <- paste('The efficacy of the drug administered is satisfactory against $T.$ $trichiura$ infections, but 
is below the expected efficacy for $A.$ $lumbricoides$ infections (95 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                concl_md <- paste('The efficacy of the drug administered is satisfactory against <em>T. trichiura</em> infections, but 
is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                            }else{
                                if(rstar == 2 & tstar == 2) {
                                    concl    <- paste('The efficacy of the drug administered is below the expected efficacy for both $A.$ $lumbricoides$ (95 precent) 
and $T.$ $trichiura$ infections (50 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for both <em>A. lumbricoides</em> (95%) 
and <em>T. trichiura</em> infections (50%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                }
                            }
                        }
                    }
                }else{
                    concl    <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')
                    concl_md <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.') 
                }
            }else{
                if(mean(data$Rb)>-2 & mean(data$Hb)>-2 & mean(data$Rf)>-2 & mean(data$Hf)>-2){
                    data$RB <-  data[,input$Rbas]  
                    data$HB <-  data[,input$Hbas] 
                    data$RF <-  data[,input$Rfol]  
                    data$HF <-  data[,input$Hfol] 
                    R <- subset(data, data$RB> 0 & data$RF >= 0)
                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                    
                    ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
                    ERRH <- 100*(1- mean(H$HF)/mean(H$HB))
                    
                    if(input$STHdrug == 1){ 
                        rstar<-ifelse(ERRR >=95,1,2)
                        hstar<-ifelse(ERRH >=90,1,2)
                        if(rstar == 1 & hstar == 2){
                            concl    <- paste('The efficacy of the drug administered is satisfactory for $A.$ $lumbricoides$ infections, but
is below the expected efficacy hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                            concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em> infections, but
is below the expected efficacy hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                        }else{ 
                            if(rstar == 2 & hstar == 1){
                                concl    <- paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for $A.$ $lumbricoides$ infection (95 precent). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                                concl_md <- paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for <em>A. lumbricoides</em> infection (95%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.') 
                            }else{  
                                if(rstar == 2 & hstar == 2){
                                    concl    <- paste('The efficacy of the drug administered is below the expected efficacy for both $A.$ $lumbricoides$ (95 precent) 
and hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for both <em>A. lumbricoides</em> (95%) 
and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                }else{ 
                                    if(rstar == 1 & hstar == 1){
                                        concl    <- paste('The efficacy of the drug administered is satisfactory for both $A.$ $lumbricoides$ and hookworm infections.')
                                        concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and hookworm infections.') 
                                    }
                                }
                            }
                        }
                    }else{
                        if(input$STHdrug == 2){
                            rstar<-ifelse(ERRR >=95,1,2)
                            hstar<-ifelse(ERRH >=70,1,2)
                            if(rstar == 1 & hstar == 2){
                                concl <- paste('The efficacy of the drug administered is satisfactory for $A.$ $lumbricoides$ infections, 
but is below the expected efficacy hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>A. lumbricoides</em> infections, 
but is below the expected efficacy hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                            }else{ 
                                if(rstar == 2 & hstar == 1){
                                    concl    <- paste('The efficacy of the drug administered is satisfactory against hookworm infections, but is below the expected efficacy for $A.$ $lumbricoides$ infection (95 precent). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is satisfactory against hookworm infections, but is below the expected efficacy for <em>A. lumbricoides</em> infection (95%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.') 
                                }else{  
                                    if(rstar == 2 & hstar == 2){
                                        concl    <- paste('The efficacy of the drug administered is below the expected efficacy for both $A.$ $lumbricoides$ (95 precent) 
and hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                        concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for both <em>A. lumbricoides</em> (95%) 
and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                    }else{ 
                                        if(rstar == 1 & hstar == 1){
                                            concl    <- paste('The efficacy of the drug administered is satisfactory for both $A.$ $lumbricoides$ and hookworm infections.')
                                            concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>A. lumbricoides</em> and hookworm infections.') 
                                        }
                                    }
                                }
                            }
                            ## new drug 
                        }else{
                            concl    <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')
                            concl_md <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.') 
                        }
                    } 
                }else{
                    if(mean(data$Tb)>-2 & mean(data$Hb)>-2 & mean(data$Tf)>-2 & mean(data$Hf)>-2){
                        data$TB <-  data[,input$Tbas]
                        data$HB <-  data[,input$Hbas] 
                        data$TF <-  data[,input$Tfol] 
                        data$HF <-  data[,input$Hfol] 
                        Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                        H <- subset(data, data$HB> 0 & data$HF >= 0)
                        
                        ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
                        ERRH <- 100*(1- mean(H$HF)/mean(H$HB))
                        
                        if(input$STHdrug == 1) { 
                            tstar<-ifelse(ERRT >=50,1,2)
                            hstar<-ifelse(ERRH >=90,1,2)
                            if(tstar == 1 & hstar == 2){
                                concl    <- paste('The efficacy of the drug administered is satisfactory for $T.$ $trichiura$ infections, 
but is below the expected efficacy hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em> infections, 
but is below the expected efficacy hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                            }else{ 
                                if(tstar == 2 & hstar == 1){
                                    concl    <- paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for $T.$ $trichiura$ infection (50 precent). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for <em>T. trichiura</em> infection (50%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.') 
                                }else {  
                                    if(tstar == 2 & hstar == 2){
                                        concl    <- paste('The efficacy of the drug administered is below the expected efficacy for both whipwworm (50 precent) 
and hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                        concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for both whipwworm (50%) 
and hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                    }else{ 
                                        if(tstar == 1 & hstar == 1){
                                            concl    <- paste('The efficacy of the drug administered is satisfactory for both $T.$ $trichiura$ and hookworm infections.')
                                            concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>T. trichiura</em> and hookworm infections.') 
                                        }
                                    }
                                }
                            }
                        }else{
                            if(input$STHdrug == 2){
                                tstar<-ifelse(ERRT >=50,1,2)
                                hstar<-ifelse(ERRH >=70,1,2)
                                if(tstar == 1 & hstar == 2){
                                    concl    <- paste('The efficacy of the drug administered is satisfactory for $T.$ $trichiura$ infections, 
but is below the expected efficacy hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em> infections, 
but is below the expected efficacy hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                }else{ 
                                    if(tstar == 2 & hstar == 1){
                                        concl    <- paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for $T.$ $trichiura$ infection (50 precent). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.')
                                        concl_md <- paste('The efficacy of the drug administered is satisfactory for hookworm infections, but is below the expected efficacy for <em>T. trichiura</em> infection (50%). Please contact World Health Organization (wormcontrol@who.int or Dr. A. Montresor (montresora@who.int)) and its collaborating centre for the monitoring of anthelmintic drug efficacy for soil-transmitted helminthiasis (Dr. B. Levecke: bruno.levecke@ugent.be) to discuss further actions.') 
                                    }else{  
                                        if(tstar == 2 & hstar == 2){
                                            concl    <- paste('The efficacy of the drug administered is below the expected efficacy for both $T.$ $trichiura$ (50 precent) 
and hookworm infections (70 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                            concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for both <em>T. trichiura</em> (50%) 
and hookworm infections (70%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                        }else{ 
                                            if(tstar == 1 & hstar == 1){
                                                concl    <- paste('The efficacy of the drug administered is satisfactory for both $T.$ $trichiura$ and hookworm infections.')
                                                concl_md <- paste('The efficacy of the drug administered is satisfactory for both <em>T. trichiura</em> and hookworm infections.') 
                                            }
                                        }
                                    }
                                }
                                ## new drug 
                            }else{
                                concl    <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.')
                                concl_md <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against soil-transmitted helminthiasis.') 
                            }
                        } 
                    }else{
                        if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
                            data$RB <-  data[,input$Rbas]  
                            data$RF <-  data[,input$Rfol]  
                            R <- subset(data, data$RB> 0 & data$RF >= 0)
                            
                            ERRR <- 100*(1- mean(R$RF)/mean(R$RB))
                            
                            if(input$STHdrug == 1 | input$STHdrug == 2){
                                rstar <- ifelse(ERRR>=95,1,2)
                                if(rstar == 1) {
                                    concl    <- paste('The efficacy of the drug administered is satisfactory.')  
                                    concl_md <- paste('The efficacy of the drug administered is satisfactory.') 
                                }else{
                                    concl    <- paste('The efficacy of the drug administered is below the expected efficacy for $A.$ $lumbricoides$ infections (95 precent). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                    concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for <em>A. lumbricoides</em> infections (95%). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                }
                            }else{
                                concl    <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                        Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against $A.$ $lumbricoides$ infections.')
                                concl_md <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                        Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against <em>A. lumbricoides</em> infections.') 
                            }
                        }else{
                            if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
                                data$TB <-  data[,input$Tbas]
                                data$TF <-  data[,input$Tfol] 
                                Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                
                                ERRT <- 100*(1- mean(Tr$TF)/mean(Tr$TB))
                                
                                if(input$STHdrug == 1 | input$STHdrug == 2){ 
                                    tstar <- ifelse(ERRR>=50,1,2)
                                    if(tstar == 1){
                                        concl    <- paste('The efficacy of the drug administered is satisfactory for $T.$ $trichiura$ infections.')  
                                        concl_md <- paste('The efficacy of the drug administered is satisfactory for <em>T. trichiura</em> infections.')   
                                    }else{
                                        concl    <- paste('The efficacy of the drug administered is below the expected efficacy for $T.$ $trichiura$ infections (50 precent). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                        concl_md <- paste('The efficacy of the drug administered is below the expected efficacy for <em>T. trichiura</em> infections (50%). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                    }
                                }else{
                                    concl    <- paste('Currently, the expected efficacy is only determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against $T.$ $trichiura$ infections.')
                                    concl_md <- paste('Currently, the expected efficacy is only determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequently, no conclusions can be drawn on the efficacy for this drug or drug regimen against <em>T. trichiura</em> infections.') 
                                }
                            }else{
                                if(mean(data$Hb)>-2 & mean(data$Hf)>-2){
                                    data$HB <-  data[,input$Hbas] 
                                    data$HF <-  data[,input$Hfol] 
                                    H <- subset(data, data$HB> 0 & data$HF >= 0)
                                    
                                    ERRH <- 100*(1- mean(H$HF[H$HB>0 & H$HF>=0])/mean(H$HB[H$HB>0 & H$HF>=0]))
                                    
                                    if(input$STHdrug == 1){ 
                                        hstar<-ifelse(ERRH >=90,1,2)
                                        if(hstar == 2){
                                            concl    <- paste('The efficacy of the drug administered is below the expected efficacy hookworm infections (90 precent). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                            concl_md <- paste('The efficacy of the drug administered is below the expected efficacy hookworm infections (90%). Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                        }else{ 
                                            if(hstar == 1){
                                                concl    <- paste('The efficacy of the drug administered is satisfactory against hookworm infections.')
                                                concl_md <- paste('The efficacy of the drug administered is satisfactory against hookworm infections.') 
                                            } 
                                        }
                                    }else{
                                        if(input$STHdrug == 2){
                                            hstar<-ifelse(ERRH >=70,1,2)
                                            if(hstar == 2){
                                                concl    <- paste('The efficacy of the drug administered is below the expected efficacy hookworm infections (70 precent). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.')
                                                concl_md <- paste('The efficacy of the drug administered is below the expected efficacy hookworm infections (70%). 
Please inform the local authorities (e.g., Ministry of Health) about these findings. In addition, 
it is recommended to contact World Health Organization
                          (wormcontrol@who.int or Antonio Montresor (montresora@who.int)) and its collaborating centre 
(Bruno Levecke: bruno.levecke@ugent.be) to exclude any possible confounding factors that may explain this poor drug efficacy and to discuss further actions.') 
                                            }else{ 
                                                if(hstar == 1){
                                                    concl    <- paste('The efficacy of the drug administered is satisfactory against hookworm infections.')
                                                    concl_md <- paste('The efficacy of the drug administered is satisfactory against hookworm infections.') 
                                                }    
                                            }
                                            ## new drug 
                                        }else {
                                            concl    <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequenlty, no conclusions can be drawn on the efficacy for this drug or drug regimen against hookworm infections.')
                                            concl_md <- paste('Currently, the expected efficacy has only been determined for a single oral dose of albendazole (400 mg) and mebendazole (500 mg). 
                                      Consequenlty, no conclusions can be drawn on the efficacy for this drug or drug regimen against hookworm infections.') 
                                        }
                                    } 
                                }
                                else{
                                    concl    <- paste('No egg count data was provided.')
                                    concl_md <- concl 
                                }
                            }
                        }
                    }  
                }   
            }
        } 
    }
    if(type == "markdown"){
        concl <- concl_md
    }
    concl
}