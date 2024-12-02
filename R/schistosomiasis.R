
#' @title Analysis of Schistosomiasis
#' @description Analysis of Schistosomiasis
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


