
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
                        paradrug_helminthiasis_n(PARADRUG, 
                                                 Rbas = input$Rbas, Rfol = input$Rfol, 
                                                 Tbas = input$Tbas, Tfol = input$Tfol, 
                                                 Hbas = input$Hbas, Hfol = input$Hfol,
                                                 type = "markdown")
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
            paradrug_schistosomiasis_intensity(PARADRUG, 
                                       Shbas = input$Shbas, Shfol = input$Shfol, 
                                       Smbas = input$Smbas, Smfol = input$Smfol, 
                                       Sjbas = input$Sjbas, Sjfol = input$Sjbas,
                                       type = "markdown")
            }
                else {
                    # Number of cases of soil-transmitted helminthiasis
                    if (input$NTD=='2')
                    {
                        PARADRUG <- paradrug_data()
                        paradrug_helminthiasis_intensity(PARADRUG, 
                                                         Rbas = input$Rbas, Rfol = input$Rfol, 
                                                         Tbas = input$Tbas, Tfol = input$Tfol, 
                                                         Hbas = input$Hbas, Hfol = input$Hfol, 
                                                 type = "markdown")
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
            plot_paradrug_schistosomiasis_eggcount(PARADRUG, 
                                                   Shbas = input$Shbas, Shfol = input$Shfol, 
                                                   Smbas = input$Smbas, Smfol = input$Smfol, 
                                                   Sjbas = input$Sjbas, Sjfol = input$Sjfol)

            }
                else  {if (input$NTD=='2'){
                    PARADRUG <- paradrug_data()
                    plot_paradrug_helminthiasis_eggcount(PARADRUG, 
                                                         Rbas = input$Rbas, Rfol = input$Rfol, 
                                                         Tbas = input$Tbas, Tfol = input$Tfol, 
                                                         Hbas = input$Hbas, Hfol = input$Hfol)

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
            paradrug_schistosomiasis_follow(PARADRUG, 
                                            Shbas = input$Shbas, Shfol = input$Shfol, 
                                            Smbas = input$Smbas, Smfol = input$Smfol, 
                                            Sjbas = input$Sjbas, Sjfol = input$Sjfol,
                                            followup = input$followup,
                                            type = "markdown")
            }
                else {
                    # Number of cases of soil-transmitted helminthiasis
                    if (input$NTD=='2')
                    {
                        PARADRUG <- paradrug_data()
                        paradrug_helminthiasis_follow(PARADRUG, 
                                                      Rbas = input$Rbas, Rfol = input$Rfol, 
                                                      Tbas = input$Tbas, Tfol = input$Tfol, 
                                                      Hbas = input$Hbas, Hfol = input$Hfol, 
                                                      followup = input$followup,
                                                      type = "markdown")
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
            paradrug_schistosomiasis_eggreduction(PARADRUG, 
                                                  Shbas = input$Shbas, Shfol = input$Shfol, 
                                                  Smbas = input$Smbas, Smfol = input$Smfol, 
                                                  Sjbas = input$Sjbas, Sjfol = input$Sjfol,
                                                  drug = ifelse(input$Sdrug %in% 1, "Praziquantel (1x 40 mg/kg)", "Other"),
                                                  type = "markdown")
            }
                else {
                    # Soil-transmitted helminthiasis
                    if (input$NTD=='2')
                    {
                        PARADRUG <- paradrug_data()
                        paradrug_helminthiasis_eggreduction(PARADRUG, 
                                                            Rbas = input$Rbas, Rfol = input$Rfol, 
                                                            Tbas = input$Tbas, Tfol = input$Tfol, 
                                                            Hbas = input$Hbas, Hfol = input$Hfol, 
                                                            drug = ifelse(input$STHdrug %in% 1, "Albendazole (1x 400 mg)", 
                                                                          ifelse(input$STHdrug %in% 2, "Mebendazole (1x 500 mg)", "Other")),
                                                            type = "markdown")
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
            paradrug_schistosomiasis_conclusion(PARADRUG, 
                                                Shbas = input$Shbas, Shfol = input$Shfol, 
                                                Smbas = input$Smbas, Smfol = input$Smfol, 
                                                Sjbas = input$Sjbas, Sjfol = input$Sjfol, 
                                                drug = ifelse(input$Sdrug %in% 1, "Praziquantel (1x 40 mg/kg)", "Other"),
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
            plot_paradrug_schistosomiasis_eggcount_reduction(PARADRUG, 
                                                             drug = ifelse(input$Sdrug %in% 1, "Praziquantel (1x 40 mg/kg)", "Other"))
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
            out = knitr::knit2pdf(system.file(package = "ParaDrug", "apps", "paradrug-1.0", "input2.Rnw"), clean = TRUE)
            file.rename(out, file) # move pdf to file for downloading
        },
        contentType = 'application/pdf'
    )
}

