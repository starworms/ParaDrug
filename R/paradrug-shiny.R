
#' @title Paradrug Shiny app component
#' @description Main UI part of the Paradrug app
#' @param id character string with the id of the module
#' @param ... not used
#' @return 
#' \code{paradrugUI} returns the the ui elements of the Paradrug app.\cr
#' \code{paradrugServer} returns the the server components of the Paradrug app.
#' @export
#' @name paradrugShiny
#' @aliases paradrugUI paradrugServer
#' @examples 
#' if(interactive()){
#' library(ParaDrug)
#' library(shiny)
#' 
#' ui <- paradrugUI()
#' server <- paradrugServer
#' shinyApp(ui, server)
#' }
paradrugUI <- function(id, ...){
    addResourcePath(prefix = "images", directoryPath = system.file(package = 'ParaDrug', "apps", "paradrug-1.0", "www"))
    fluidPage(
        #tags$head(includeScript("GoogleAnalytics.js")), 
        # Application title
        theme = bslib::bs_theme(version = "4"),
        titlePanel(h2("ParaDrug 1.0 - Test Phase")),
        sidebarLayout(
            sidebarPanel(
                h4('Upload data', style="color:#EB4C4C"),
                fileInput('file1', 'Choose Excel file to upload',accept = c(".xls",".XLS", ".xlsx")),
                h4('General Information',style="color:#EB4C4C"),
                selectInput("NTD", label = "Disease:", choices = list("Schistosomiasis" = 1, "Soil-transmitted helminthiasis" = 2), selected = 1), 
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Sdrug", label = "Anthelmintic drug:", choices = list("Praziquantel (1x 40 mg/kg)" = 1, "Other" = 2), selected = 1)),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("STHdrug", label = "Anthelmintic drug:", choices = list("Albendazole (1x 400 mg)" = 1, "Mebendazole (1x 500 mg)" = 2, "Other" = 3), selected = 1)),
                br(),
                h4('Baseline information',style="color:#EB4C4C"),
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Shbas", label = p(em("S. haematobium"), "eggs per 10 ml of urine:"), choices= "")),
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Smbas", label = p(em("S. mansoni"), "eggs per gram of stool:"), choices="")),
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Sjbas", label = p(em("S. japonicum"), "eggs per gram of stool:"), choices="")),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("Rbas", label = p(em("Ascaris lumbricoides"), "eggs per gram of stool:"), choices="")),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("Tbas", label = p(em("Trichuris trichiura"), "eggs per gram of stool:"), choices="")),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("Hbas", label = "Hookworm eggs per gram of stool:", "")),
                h6(textOutput('smix')),
                br(),
                h4('Follow-up information',style="color:#EB4C4C"),
                selectInput("followup", "Number of days between the baseline and the follow-up survey: ",""), 
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Shfol", label = p(em("S. haematobium"), "eggs per 10 ml of urine:"), "")),
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Smfol", label = p(em("S. mansoni"), "eggs per gram of stool:"), "")),
                conditionalPanel(condition = "input.NTD == '1'", selectInput("Sjfol", label = p(em("S. japonicum"), "eggs per gram of stool:"), "")),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("Rfol", label = p(em("Ascaris lumbricoides"), "eggs per gram of stool:"),choices= "")),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("Tfol", label = p(em("Trichuris trichiura"), "eggs per gram of stool:"), choices="")),
                conditionalPanel(condition = "input.NTD == '2'", selectInput("Hfol", label = "Hookworm eggs per gram of stool:",choices= ""))
                #  br(),
                #  actionButton("do", "Analyze data",class = "butt"),
                #  tags$head(tags$style(".butt{background-color:#EB4C4C;} .butt{color: white;}"))
            ),
            mainPanel(tabsetPanel(
                tabPanel(
                    h4(strong("Introduction"),tags$style(type = "text/css", "a{color:#EB4C4C;}")),h4('Background', style="color:#EB4C4C"),
                    p('The most prevalent neglected tropical diseases (NTDs) include schistosomiasis (',em('Schistosoma haematobium, S. mansoni'), 'and' ,em('S. japonicum'),') and soil-transmitted helmintiasis (',em('Ascaris lumbricoides'),',',em('Trichuris trichiura'), ', and the hookworms',em('Ancylostoma duodenale'),'and', em('Necator americanus'), '). 
    The main strategy for controlling the morbidity caused by these diseases are preventive chemotherapy (PC) programs, in which anthelmintic drugs (praziquantel for schistosomiasis, and albendazole or mebendazole for soil-transmitted helminthiasis) are periodically administered to children (', a('WHO et al., 2011', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/44671/1/9789241548267_eng.pdf'),'). Fueled by the', a('London Declaration on NTDs', style="color:#EB4C4C",href='http://unitingtocombatntds.org/sites/default/files/document/london_declaration_on_ntds.pdf'), ',  the coverage of children in PC programs has substantially increased 
          (', a('WHO et al., 2016', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/251908/1/WER9149_50.pdf?ua=1'),'), and an upscale is underway with the ultimate goal to include at least 75% of the children in all endemic countries by 2020 (', a('WHO et al., 2012', style="color:#EB4C4C",href='http://www.who.int/neglected_diseases/NTD_RoadMap_2012_Fullversion.pdf'),'). 
          However, a major concern is that this high drug pressure will cause anthelmintic drug resistance, and hence will reduce the impact of the PC programs. Therefore, 
thoroughly designed monitoring systems are needed, allowing to detect changes in anthelmintic drug efficacy that may arise through the evolution of drug resistance in these worms. '),
                    p('Currently, the reduction in egg counts following drug administration, the egg reduction rate (ERR), is the recommended method for
          monitoring the efficacy of anthelmintic drugs against both schistosomes and soil-transmitted helminths. However, the efficacy data available have 
been obtained through a variety of
widely differing study protocols, which impedes drawing readily conclusions on the emergence of 
          anthelmintic resistance (', a('Vercruysse et al., 2011', style="color:#EB4C4C",href='http://www.sciencedirect.com/science/article/pii/S2211320711000042?via%3Dihub'),
                      '). As a response to this lack of standardization, World Health Organization (WHO) has developed guidelines on how to assess drug efficacy of anthelmintic drugs used in PC programs against both schistosomiasis and soil-transmitted helminthiasis (', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'). This WHO document provides guidance on when and how to assess the efficacy of anthelmintic drugs, including detailed recommendations on the indicators of efficacy, the sample size, the follow-up period, the laboratory methods, the statistical analysis and the final interpretation of the data collected.'),
                    
                    br(),h4('Goal of ParaDrug 1.0', style="color:#EB4C4C"),
                    p('ParaDrug 1.0 aims to further standardize reporting and interpreting the anthelmintic drug efficacy data obtained during PC programs. 
          It supports program managers in analysing, interpreting and summarizing drug efficacy data as recommended by the WHO, and this without the need of 
          any prior knowledge on statistical softwares.'), br(),h4('Work with ParaDrug 1.0',style="color:#EB4C4C"),
                    p('     ',strong("Step 1"), ': verify whether your data meet the requirements in the', em('My data'),'tab'),
                    p('     ',strong("Step 2"), ': upload your data in the top left corner of this this tool'),
                    p('     ',strong("Step 3"), ': indicate which disease has been targetted and which anthelmintic drug has been admininistered in the general information section of the side panel'),
                    p('     ',strong("Step 4"), ': match both baseline and follow-up information in the side panel with the corresponding headers of your data set'),
                    p('     ',strong("Step 5"), ': press',em('calculate baseline statstics'),'in the corresponding tab to obtain the baseline statistics'),
                    p('     ',strong("Step 6"), ': press',em('calculate drug efficacy'),'in the corresponding tab to obtain the drug efficacy'),
                    p('     ',strong("Step 7"), ': customize and download your report in the',em('report'),'tab'),
                    p('     ',strong("Step 8 (Optional)"), ': consult the',em('Parameters'),'tab to gain more insight into the calculation of the different parameters'),
                    p('     ',strong("Note"),': summarizing the results and creating the report will take about 20 seconds.'),
                    br(),
                    h4('Data use and ownership',style="color:#EB4C4C"), 
                    p('ParaDrug 1.0 is solely designed as a data analysis and reporting tool. It will analyze the data uploaded and generate a corresponding report, but it will under no circumstances store the data in any form. Users
will retain full ownership on their data.'),
                    br(),
                    h4('Contact us',style="color:#EB4C4C"), 
                    p('info@starworms.org'),
                    br(),
                    #        p(strong('Address')),
                    #        p('Laboratory of Parasitology'),
                    #        p('Salisburylaan 133'),
                    #        p('9820 Merelbeke'),
                    #        p('Belgium'),
                    #        br(),
                    h4('Reference ParaDrug 1.0',style="color:#EB4C4C"), 
                    # p(strong('E-mail')),
                    p('Levecke B (2017). ParaDrug 1.0. Available at: http://www.starworms.org'),
                    br(),
                    h4('Further reading',style="color:#EB4C4C"), 
                    br(),
                    p(a(strong('Levecke B, et al. (2014).'),style="color:black",href='http://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0003204'),'Assessment of anthelmintic efficacy of mebendazole in
school children in six countries where soil-transmitted helminths are endemic. PLoS Negl. Trop. Dis. 8: e3204.'),
                    br(),
                    p(a(strong('Levecke B, et al. (2015).'), style="color:black",href='http://www.sciencedirect.com/science/article/pii/S0065308X15000081?via%3Dihub'),'Mathematical inference on helminth egg counts in stool and its applications in mass drug administration programmes to control soil-transmitted helminthiasis in public health. Adv. Parasitol. 87: 197-247.'),
                    br(),
                    p(a(strong('Vercruysse J, et al. (2011).'),style="color:black",href='http://www.sciencedirect.com/science/article/pii/S2211320711000042?via%3Dihub'), 'Is anthelmintic resistance a concern for the control of human soil-transmitted helminths? Int. J. Parasitol. Drugs and Drug Resistance 1: 14-27.'),
                    br(),
                    p(a(strong('Vercruysse J, et al. (2011).'),style="color:black",href='http://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0000948'),'Assessment of the anthelmintic efficacy of albendazole
in school children in seven countries where soil-transmitted helminths are endemic. PloS Negl. Trop. Dis. 5: e948.'),
                    br(),
                    p(a(strong('World Health Organization (2013).'),style="color:black", href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),'Assessing the efficacy of anthelminthic drugs against schistosomiasis and soil-transmitted helminthiasis. World Health Organization, Geneva, Switzerland.'),
                    br(),
                    h4('Acknowledgements',style="color:#EB4C4C"), 
                    p('ParaDrug 1.0 is developed by Bruno Levecke. The development of this tool was financed by the Bill & Melinda Gates Foundation and the Research Foundation - Flanders (FWO).'),
                    br(),
                    fluidRow(
                        column(4,a(href='http://www.starworms.org/',img(src="images/combo2.jpg", height = 182, width = 300))),
                        column(4,a(href='https://www.ugent.be/en',img(src="images/Ugent.jpg", height = 180, width = 200))),
                        column(4,a(href='http://www.fwo.be/en',img(src="images/FWO.jpg", height = 90, width = 230)))
                        
                    )),
                
                tabPanel(h4(strong("My data"),tags$style(type = "text/css", "a{color:#EB4C4C;}")),
                         h4('Required data',style="color:#EB4C4C"), 
                         p(strong('Egg counts')),
                         p('The tool analyzes egg count data collected during trials designed to assess the efficacy of anthelmintic drugs by means of egg reduction rate 
(= the reduction in egg excretion after drug administration). Hence the file should contain at least egg counts before and after drug administration.'),
                         p('The data should be expressed in number of eggs per gram of stool in case of',
                           em('Schistosoma mansoni,'), em('S. japonicum'),'and the soil-transmitted helminths, and in number of eggs per 10 ml of urine in case of', em('S. haematobium.')),
                         br(),
                         p(strong('Follow-up period')),
                         p('The follow-up period is the number of days between the drug administration and the examination of a follow-up stool/urine sample. Although this information is not required to calculate the drug efficacy, it is important to readily interpret the drug efficacy results. The recommended follow-up period to assess the efficacy of an anthelmintic drug against schistosomiasis and soil-transmitted helminthiasis is between 14 and 21 days. Any values outside this interval may undermine the interpretation of the results.'),
                         br(),
                         
                         h4('Required format',style="color:#EB4C4C"), 
                         p(strong('File extension')),
                         p('ParaDrug 1.0 can analyse data entered in either xls or xlsx files.'),
                         br(),
                         p(strong('Identification of the worksheet')),
                         p('There is no need to provide a specific name to the worksheet containing the data. ParaDrug 1.0 will always select the first worksheet in your xls(x) file. 
        Hence, in case you have multiple worksheets in your file, you will need to place the worksheet contaning the raw data first.'),
                         br(),
                         p(strong('Headers')), 
                         p('Once uploaded, ParaDrug 1.0 will identify the first row of each column as a header, and will read the remaining rows as data entries. 
        It is therefore essential that each parameter/column is given a unique name.'),
                         br(),
                         p(strong('Format cells')), 
                         p('Each of the cells representing the required data should be formatted as numbers. When strings are inserted, ParaDrug 1.0 will interpret the data as missing. 
      Furthermore, to avoid discripancies between a comma (e.g., 5,00) and a dot (e.g., 5.00) decimal, it is recommended to enter data as integer numbers (e.g., 5). It is also important that the data set is trimmed, excluding unnessary empty rows.'),
                         br(), 
                         h4('Missing data',style="color:#EB4C4C"), 
                         p('Missing data are to be expected in anthelmintic drug efficacy trials, as subjects may not present themselves at follow-up or because they were 
excluded from the trial based on the study specific exclusion criteria. Since these subjects will need to be excluded from the final analysis, it is important that missing data are identified correctly. There are various ways to report missing data. We recommend using an 
impossible value, such as -1. First, empty cells in the data set can also be due to human error, if someone forgets to enter data. Second, inserting a string such as NA 
          may not be compatible with the required cell format.'),
                         br(), 
                         h4('Contact us',style="color:#EB4C4C"), 
                         p('Please do not hesitate to contact us at', strong('info@starworms.org'), 'when you encounter any other obstacle when uploading your data.'),
                         br()),
                tabPanel(h4(strong("Baseline statistics"),tags$style(type = "text/css", "a{color:#EB4C4C;}")),
                         br(),
                         actionButton("basedata", "Calculate baseline statistics",class = "butt"),
                         tags$head(tags$style(".butt{background-color:#EB4C4C;} .butt{color: white;}")),
                         br(),
                         br(),
                         htmlOutput('countbase'),
                         h4('Number of subjects',style="color:#EB4C4C"),htmlOutput('number'),br(),h4('Intensity of infections',style="color:#EB4C4C"), htmlOutput('int'),plotOutput('distegg'),             
                         br(),h4('Follow-up period',style="color:#EB4C4C"),htmlOutput('followup')),
                tabPanel(h4(strong("Drug efficacy"),tags$style(type = "text/css", "a{color:#EB4C4C;}")),
                         br(),
                         actionButton("eggreducrate", "Calculate drug efficacy",class = "butt"),
                         tags$head(tags$style(".butt{background-color:#EB4C4C;} .butt{color: white;}")),
                         br(),
                         br(),
                         h4('Egg reduction rate',style="color:#EB4C4C"),htmlOutput('err'),plotOutput('disterr'),h4('Conclusions',style="color:#EB4C4C"),htmlOutput('concl')),
                tabPanel(h4(strong("Report"),tags$style(type = "text/css", "a{color:#EB4C4C;}")),
                         h4('Customize report',style="color:#EB4C4C"),
                         p('To further customize please complete the gaps below.'),
                         br(),
                         textInput("Name", label = p(strong("The name of your institution")), 
                                   value = ""), 
                         textInput("Country", label = p(strong("Country in which the trial was conducted")), 
                                   value = ""), 
                         textInput("Region", label = p(strong("District/province in which the trial was conducted")), 
                                   value = ""),
                         br(),
                         h4('Download report',style="color:#EB4C4C"),
                         downloadButton("report", class = "butt"),
                         tags$head(tags$style(".butt{background-color:#EB4C4C;} .butt{color: white;}"))
                         
                ),
                tabPanel(h4(strong("Parameters"),tags$style(type = "text/css", "a{color:#EB4C4C;}")),
                         tags$head(tags$style(".butt{background-color:#EB4C4C;} .butt{color: white;}")),
                         h4('Number of subjects',style="color:#EB4C4C"),
                         p(strong('Number of subjects enrolled')),
                         p('The number of subjects enrolled corresponds with the number of rows (excluding the header) in the data set. 
           It is therefore important that the data set is appropriately trimmed, excluding any unnessary empty rows.'),
                         br(),
                         p(strong('Number of cases per worm species')),
                         p('The number of cases for a particular worm species corresponds with the number of enrolled subjects who are excreting eggs of this worm species 
at baseline. In addition, the proportion of cases (%) are reported for each of the worm species. To this end, the number of cases is divided over the 
           number of subjects enrolled and is multiplied by 100.'),
                         br(),
                         p(strong('Number of mixed infections')),
                         p('The number of mixed infections corresponds with the number of enrolled subjects who were excreting eggs of at least two worm species at baseline. In addition, the proportion of mixed infections (%) is reported. To this end, the number of mixed infections is divided over the 
           number of subjects enrolled and multiplied by 100.'),
                         br(),
                         p(strong('Number of complete cases')),
                         p('The number of complete cases corresponds with the number of subjects who were excreting eggs of any worm species at baseline, and for whom a follow-up sample is examined.
          In addition, the number of complete cases for each worm species are reported seperately. These correspond to the number of subjects who were excreting eggs of this worm species at baseline, and for whom a follow-up sample is examined.'),
                         br(),
                         h4('Intensity of infections',style="color:#EB4C4C"),
                         p(strong('Mean egg count at baseline per worm species')),
                         p('The mean egg count per worm species corresponds with the arithmetic mean egg count (in eggs per gram of stool / eggs per 10 ml of urine) at baseline across all the complete
          cases for that particular worm species.'),
                         br(),
                         p(strong('Spread of egg count at baseline per worm species')),
                         p('The spread of the egg counts is presented in both text and graphic format for each worm species seperately. First, both the 25th and the 75th percentile of the baseline egg counts across complete cases are determined. The 25th quantile divides the individual egg counts so that 25% of the values lie below (or 75% of the values lie above). Similarly, the 75th quantile divides the individual egg counts so that 75% of the values 
           lie below (or 25% of the values lie above). Second, a histogram is provided to illustrate the distribution of the egg counts across
           the complete cases per worm species. The figure below shows the distribution of the baseline hookworm egg counts for all complete cases for this worm species in a previously conducted drug efficacy trial.'),
                         fluidRow(
                             column(4,img(src="images/Rplot01.jpg", height = 273, width = 300))),
                         
                         
                         br(),
                         br(),
                         p(strong('Number of low, moderate and high intensity infections per worm species')),
                         p('The number of low, moderate and high intensity infections is determined for each worm species seperately. WHO thresholds defining the three classes of infection intensity for the different worm species are summarized in the table below (', a('WHO et al., 1998', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/63821/1/WHO_CTD_SIP_98.1.pdf'),'). In addition, the proportions of low, moderate and high intensity infections (%) are reported.
          To this end, the number of low, moderate and high infections is divided over the number of complete cases for that particular worm species and multiplied by 100.'),
                         fluidRow(
                             column(4,a(href='http://apps.who.int/iris/bitstream/10665/63821/1/WHO_CTD_SIP_98.1.pdf',img(src="images/Slide2.jpg", height = 203, width = 400)))),
                         
                         br(),
                         br(),
                         h4('Follow-up period',style="color:#EB4C4C"),
                         p('The median (50th quantile), the shortest and the longest follow-up period is determined for all complete cases, regardless the worm species. In addition, the number and proportion (in %) of complete cases
              for which a follow-up sample was collected between 14 and 21 days is calculated. The proportion equals the number of complete cases for which a follow-up sample was collected between this time interval divided by the number of complete cases and multiplied by 100.'),
                         br(),
                         br(),
                         h4('Egg reduction rate',style="color:#EB4C4C"),
                         p(strong('Formula')),
                         p('To date, a wide range of formulae has been used to calculate ERR, each different in terms of the statistical unit (individual vs. group) 
           and the way the mean egg count is calculated (arithmetic vs. geometric). The group-based formula is now recommended by WHO to summarize egg reduction data 
(', a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),').'),
                         fluidRow(
                             column(4,a(img(src="images/ERRform.jpg", height = 53, width = 300)))),
                         br(),
                         
                         p('Compared to the other formulae, 
           it represents a robust indicator (vs. individual-based formula;', a('Vercruysse et al., 2011',href='http://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0000948'),') that provides accurate estimates of 
           drug efficacy 
          (vs. group-based formula using geometric mean;',a('Dobson et al., 2009',
                                                            href='https://ac.els-cdn.com/S030440170800705X/1-s2.0-S030440170800705X-main.pdf?_tid=fd2a4f3c-a86a-11e7-ad50-00000aab0f01&acdnat=1507056492_10ae60eab74fa8a1d9ddef8b986abb02'),').'),
                         br(),
                         p(strong('95% confidence intervals')),
                         p('The 95% confidence intervals (95%CI) are calculated using the mathematical framework described by ', a('Levecke et al., 2015',href='http://www.sciencedirect.com/science/article/pii/S0065308X15000081?via%3Dihub'),'. In short, this methodology derives the variance of the ERR applying the 
        Taylor method (delta method;', a('Casella and Berger, 2001',href='https://fsalamri.files.wordpress.com/2015/02/casella_berger_statistical_inference1.pdf'),'), and assumes that 100%-ERR follows a Gamma distribution. The variance of the ERR applying the Taylor method equals'), 
                         fluidRow(
                             column(4,a(img(src="images/Variance.jpg", height = 196.3, width = 800)))),
                         br(),  
                         p('The lower and upper limit of the 95% CI equal 1 - 97.5th quantile and 1 - 2.5th quantile of the Gamma distribution with a shape parameter', 
                           HTML("&gamma;"),'and a scale parameter', HTML("&theta;"),', respectively. Based on the ERR, its variance and a sample size N, the two parameters of the 
          Gamma distribution of 1 - ERR can be written as'),
                         fluidRow(
                             column(4,a(img(src="images/gamma.jpg", height = 109, width = 150)))),
                         br(),
                         p(strong('Interpretation of the observed ERR')),
                         p('Based on the observed ERR, the efficacy of the anthelmintic drug can be classified into different 
          levels by comparing the observed ERR with the WHO reference value for each worm species (', 
                           a('WHO et al., 2013', style="color:#EB4C4C",href='http://apps.who.int/iris/bitstream/10665/79019/1/9789241564557_eng.pdf'),').
          Generally, the efficacy of the anthelmintic drug is (i) satisfactory when the ERR is superior or equal to the reference value, 
          doubtful if the observed ERR is inferior to the reference value by less than 10 percent points, and (iii) reduced if the observed ERR is 
          inferior to the reference value by at least 10 percent points. The table below summarizes the thresholds and the corresponding levels of drug efficacy for 
          the different worm species and the different anthelmintic drugs used in preventive chemotherapy programs.'),
                         fluidRow(
                             column(4,a(href='http://apps.who.int/iris/bitstream/10665/63821/1/WHO_CTD_SIP_98.1.pdf',img(src="images/Slide3.jpg", height = 180, width = 400))))
                         
                         
                         
                         
                         
                )
                
            )
            
            )))
}

#' @name paradrugShiny
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#' @export
paradrugServer <- function(input, output, session){
    requireNamespace("knitr")
    requireNamespace("shiny")
    paradrug_data <- eventReactive(input$file1, {
        data <- list(n = 0, data = data.frame(), fields = character())
        if(!is.null(input$file1$datapath)){
            msg <- try(data <- read_paradrug_xls(input$file1$datapath))
            if(inherits(msg, "try-error")){
                showNotification(as.character(msg), type = "error")
            }
        }
        data   
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    #library(evaluate)
    #Step1: number of subjects in trial
    number <- eventReactive(input$basedata, {
        
        isolate ({  
            inFile <- input$file1
            if (is.null(inFile))
            {paste0(('Please upload data.'))
            } else {if (input$NTD=='1')
            {PARADRUG <- paradrug_data()
            paradrug_schistosomiasis_n(PARADRUG, 
                            Shbas = input$Shbas, Shfol = input$Shfol, 
                            Smbas = input$Smbas, Smfol = input$Smfol, 
                            Sjbas = input$Sjbas, Sjfol = input$Sjfol, 
                            type = "markdown")
            }
                else {
                    # Soil-transmitted helminthiasis
                    if (input$NTD=='2')
                    {
                        PARADRUG <- paradrug_data()
                        data     <- PARADRUG$data
                        n        <- PARADRUG$n
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
                                mix <- dim(subset(data,data$mix>1))[1]
                                com <- dim(subset(data, data$Rc==2 | data$Tc ==2 | data$Hc ==2))[1]
                                nR <- dim(subset(data, data$RB>0))[1]
                                nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                                nT <- dim(subset(data, data$TB>0))[1]
                                nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                                nH <- dim(subset(data, data$HB>0))[1]
                                nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                                
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
                                    mix <- dim(subset(data, data$mix>1))[1]
                                    com <- dim(subset(data, data$Rc==2 | data$Tc ==2))[1]
                                    nR <- dim(subset(data, data$RB>0))[1]
                                    nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                                    nT <- dim(subset(data, data$TB>0))[1]
                                    nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                                    
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
                                        mix <- dim(subset(data, data$mix>1))[1]
                                        com <- dim(subset(data, data$Rc==2 | data$Hc ==2))[1]
                                        nR <- dim(subset(data, data$RB>0))[1]
                                        nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                                        nH <- dim(subset(data, data$HB>0))[1]
                                        nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                                        
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
                                            mix <- dim(subset(data,data$mix>1))[1]
                                            com <- dim(subset(data, data$Tc ==2 | data$Hc ==2))[1]
                                            nT <- dim(subset(data, data$TB>0))[1]
                                            nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                                            nH <- dim(subset(data, data$HB>0))[1]
                                            nH2 <- dim(subset(data, data$HB>0 & data$HF>=0))[1]
                                            
                                            paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Trichuris trichiura</em> infections were observed in', nT, 'subjects (', round(100*nT/n,1), '% ), hookworm infections in', nH, 'subjects (', round(100*nH/n,1),'% ). Mixed STH infections 
                       were observed in', mix, 'subjects (', round(100*mix/n,1), '% ). In total,', com, 'infected subjects provided a sample at both baseline and follow-up, including', nT2, 'cases of <em>T. trichiura</em>, and ', nH2, 'cases of hookworms.')
                                        }
                                        else{
                                            if(mean(data$Rb)>-2 & mean(data$Rf)>-2){
                                                data$RB <-  data[,input$Rbas]  
                                                data$RF <-  data[,input$Rfol]  
                                                nR <- dim(subset(data, data$RB>0))[1]
                                                nR2 <- dim(subset(data, data$RB>0 & data$RF>=0))[1]
                                                paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Ascaris lumbricoides</em> infections were observed in', nR, 'subjects (', round(100*nR/n,1), '% ). In total,', nR2, 'infected subjects provided a sample at both baseline and follow-up.')
                                            }
                                            else{
                                                if(mean(data$Tb)>-2 & mean(data$Tf)>-2){
                                                    data$TB <-  data[,input$Tbas]  
                                                    data$TF <-  data[,input$Tfol]  
                                                    nT <- dim(subset(data, data$TB>0))[1]
                                                    nT2 <- dim(subset(data, data$TB>0 & data$TF>=0))[1]
                                                    paste('In total,', n, 'subjects were enrolled in this drug efficacy trial. <em>Trichuris trichiura</em> infections were observed in', nT, 'subjects (', round(100*nT/n,1), '% ). In total,', nT2, 'infected subjects provided a sample at both baseline and follow-up.')
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
            {PARADRUG <- paradrug_data()
            data     <- PARADRUG$data
            n        <- PARADRUG$n
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
                        sh <- subset(data, data$shB >0 &  data$shF >=0)
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
                            sm <- subset(data, data$smB >0 &  data$smF >=0)
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
                                sj <- subset(data, data$sjB >0 &  data$sjF >=0)
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
                        PARADRUG <- paradrug_data()
                        data     <- PARADRUG$data
                        n        <- PARADRUG$n
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
                                                R <- subset(data, data$RB>0 & data$RF >= 0)
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
                                                    Tr <- subset(data, data$TB> 0 & data$TF >= 0)
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
                                                        H <- subset(data, data$HB> 0 & data$HF >= 0)
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
            {PARADRUG <- paradrug_data()
            data     <- PARADRUG$data
            n        <- PARADRUG$n
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
                else  {if (input$NTD=='2'){
                    PARADRUG <- paradrug_data()
                    data     <- PARADRUG$data
                    n        <- PARADRUG$n
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
            {PARADRUG <- paradrug_data()
            data     <- PARADRUG$data
            n        <- PARADRUG$n
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
                        PARADRUG <- paradrug_data()
                        data     <- PARADRUG$data
                        n        <- PARADRUG$n
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
                                
                                data2 <- subset(data, data$Rc==2 | data$Tc ==2 | data$Hc ==2)
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
                                    
                                    data2 <- subset(data, data$Rc==2 | data$Tc ==2)
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
                                        data2 <- subset(data, data$Rc==2 | data$Hc ==2)
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
                                            
                                            data2 <- subset(data, data$Tc ==2 | data$Hc ==2)
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
                                                data2 <- subset(data, data$Rc==2)
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
                                                    data2 <- subset(data,data$Tc ==2)
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
                                                        
                                                        data2 <- subset(data, data$Hc ==2)
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
            {PARADRUG <- paradrug_data()
            data     <- PARADRUG$data
            n        <- PARADRUG$n
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
                    sh <- subset(data, data$shB >0 &  data$shF >=0)
                    sm <- subset(data, data$smB >0 &  data$smF >=0)
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
                        sh <- subset(data, data$shB >0 &  data$shF >=0)
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
                            sm <- subset(data, data$smB >0 &  data$smF >=0)
                            
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
                                sj <- subset(data, data$sjB >0 &  data$sjF >=0)
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
                        PARADRUG <- paradrug_data()
                        data     <- PARADRUG$data
                        n        <- PARADRUG$n
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
                                                R <- subset(data, data$RB> 0 & data$RF >= 0)
                                                
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
                                                    Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                                    
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
                                                        H <- subset(data, data$HB> 0 & data$HF >= 0)
                                                        
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
            {PARADRUG <- paradrug_data()
            data     <- PARADRUG$data
            n        <- PARADRUG$n
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
                    sh <- subset(data, data$shB >0 &  data$shF >=0)
                    sm <- subset(data, data$smB >0 &  data$smF >=0)
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
                        sh <- subset(data, data$shB >0 &  data$shF >=0)
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
                            sm <- subset(data, data$smB >0 &  data$smF >=0)
                            
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
                                sj <- subset(data, data$sjB >0 &  data$sjF >=0)
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
                        PARADRUG <- paradrug_data()
                        data     <- PARADRUG$data
                        n        <- PARADRUG$n
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
                                    R <- subset(data, data$RB> 0 & data$RF >= 0)
                                    Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                    
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
                                        R <- subset(data, data$RB> 0 & data$RF >= 0)
                                        H <- subset(data, data$HB> 0 & data$HF >= 0)
                                        
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
                                            Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                            H <- subset(data, data$HB> 0 & data$HF >= 0)
                                            
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
                                                R <- subset(data, data$RB> 0 & data$RF >= 0)
                                                
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
                                                    Tr <- subset(data, data$TB> 0 & data$TF >= 0)
                                                    
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
                                                        H <- subset(data, data$HB> 0 & data$HF >= 0)
                                                        
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
                                                            if(input$STHdrug == 2){
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
            {PARADRUG <- paradrug_data()
            data     <- PARADRUG$data
            n        <- PARADRUG$n
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
                    sh <- subset(data, data$shB > 0 &data$shF >=0)
                    sm <- subset(data, data$smB > 0 & data$smF >=0)
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
                        axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                        abline(v=100*(1-qgamma(0.025,shape = aSH, scale = bSH)),lty= 2, lwd=4)
                        abline(v=100*(1-qgamma(0.975,shape = aSH, scale = bSH)),lty=2, lwd=4)
                        abline(v=100*ERRSH, lwd=4)
                        
                        SM <- rgamma(10000,shape = aSM, scale = bSM)
                        hist(100*(1-SM), main=expression(italic(Schistosoma~mansoni)),ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (%)')
                        axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                        abline(v=100*(1-qgamma(0.025,shape = aSM, scale = bSM)), lty=2,lwd=4)
                        abline(v=100*(1-qgamma(0.975,shape = aSM, scale = bSM)), lty=2,lwd=4)
                        abline(v=100*ERRSM, lwd=4)
                    }
                    
                } else {
                    if(mean(data$sh)>-2 & mean(data$shf)> - 2){
                        data$shB <-  data[,input$Shbas]  
                        data$shF <-  data[,input$Shfol]  
                        
                        sh <- subset(data, data$shB >0 &  data$shF >=0)
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
                            axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                            abline(v=100*(1-qgamma(0.025,shape = aSH, scale = bSH)),lty= 2, lwd=4)
                            abline(v=100*(1-qgamma(0.975,shape = aSH, scale = bSH)),lty=2, lwd=4)
                            abline(v=100*ERRSH, lwd=4)
                        }
                        
                    } else {
                        if(mean(data$sm)>-2 & mean(data$smf)> - 2){
                            data$smB <-  data[,input$Smbas] 
                            data$smF <-  data[,input$Smfol] 
                            sm <- subset(data, data$smB >0 &  data$smF >=0)
                            
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
                                axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                                abline(v=100*(1-qgamma(0.025,shape = aSM, scale = bSM)), lty=2,lwd=4)
                                abline(v=100*(1-qgamma(0.975,shape = aSM, scale = bSM)), lty=2,lwd=4)
                                abline(v=100*ERRSM, lwd=4)
                            }
                            
                        } else {
                            if(mean(data$sj)>-2 & mean(data$sjf)> - 2){
                                data$sjB <-  data[,input$Sjbas] 
                                data$sjF <-  data[,input$Sjfol] 
                                sj <- subset(data, data$sjB >0 &  data$sjF >=0)
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
                                        axis(side=2, at=seq(0,10000,1000),labels = seq(0,100,10))
                                        abline(v=100*(1-qgamma(0.025,shape = aSJ, scale = bSJ)), lty=2,lwd=4)
                                        abline(v=100*(1-qgamma(0.975,shape = aSJ, scale = bSJ)), lty=2,lwd=4)
                                        abline(v=100*ERRSJ, lwd=4)      }
                            }
                        }
                    }
                }
            }
            } else  {if (input$NTD=='2'){
                PARADRUG <- paradrug_data()
                data     <- PARADRUG$data
                n        <- PARADRUG$n
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
                                                    hist(100*(1-H), main='Hookworm',ylab='Frequency (%)',yaxt='n', col='grey',xlab='Egg reduction rate (% )')
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
        PARADRUG <- paradrug_data()
        data     <- PARADRUG$data
        n        <- PARADRUG$n
        fields   <- PARADRUG$fields
        updateSelectInput(session,'baseline',choices=c('Not recorded', fields))
        updateSelectInput(session,'incl',choices=c('Not recorded', fields))
        updateSelectInput(session,'age',choices=c('Not recorded', fields))
        updateSelectInput(session,'sex',choices=c('Not recorded', fields))
        updateSelectInput(session,'Shbas',choices=c('Not recorded', fields))
        updateSelectInput(session,'Smbas',choices=c('Not recorded', fields))
        updateSelectInput(session,'Sjbas',choices=c('Not recorded', fields))
        updateSelectInput(session,'Rbas',choices=c('Not recorded', fields))
        updateSelectInput(session,'Tbas',choices=c('Not recorded', fields))
        updateSelectInput(session,'Hbas',choices=c('Not recorded', fields))
        updateSelectInput(session,'followup',choices=c('Not recorded', fields))
        updateSelectInput(session,'Shfol',choices=c('Not recorded', fields))
        updateSelectInput(session,'Smfol',choices=c('Not recorded', fields))
        updateSelectInput(session,'Sjfol',choices=c('Not recorded', fields))
        updateSelectInput(session,'Rfol',choices=c('Not recorded', fields))
        updateSelectInput(session,'Tfol',choices=c('Not recorded', fields))
        updateSelectInput(session,'Hfol',choices=c('Not recorded', fields))
    })
    output$report = downloadHandler(
        filename = 'report.pdf',
        content = function(file) {
            #out = knit2pdf('input2.Rnw', clean = TRUE, texi2dvi="pdflatex")
            #list.files(system.file(package = "ParaDrug", "apps", "paradrug-1.0"))
            file.copy(from = system.file(package = "ParaDrug", "apps", "paradrug-1.0", "logo.pdf"),
                      to = file.path(getwd(), "logo.pdf"), overwrite = TRUE)
            file.copy(from = system.file(package = "ParaDrug", "apps", "paradrug-1.0", "Rplot02.jpg"),
                      to = file.path(getwd(), "Rplot02.jpg"), overwrite = TRUE)
            on.exit({
                file.remove(file.path(getwd(), "logo.pdf"))
                file.remove(file.path(getwd(), "Rplot02.jpg"))
            })
            out = knit2pdf(system.file(package = "ParaDrug", "apps", "paradrug-1.0", "input2.Rnw"), clean = TRUE)
            file.rename(out, file) # move pdf to file for downloading
        },
        contentType = 'application/pdf'
    )
}

