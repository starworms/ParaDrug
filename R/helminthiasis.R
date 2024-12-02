#' @title Analysis of Helminthiasis
#' @description Analysis of Helminthiasis
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