library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(shinycssloaders)
library(readr)
library(htmlwidgets)
library(shinyWidgets)
library(stringr)
library(shinyBS)
library(shinyjs)
library(png)
library(shinydashboard)
library(dqshiny)
library(RColorBrewer)


load("./www/data/ui.RData")
load("./www/data/new/0.plot/plt1.RData")
load("./www/data/new/0.plot/plt2.RData")
load("./www/data/new/0.plot/plt3.RData")
ui <- tagList(
shiny::tags$style("html,body{background-color: white ;}
.container{
width: 100%;
hight: 100%;
margin: 0 auto;
padding: 0;}"),
shiny::tags$style("img{ max-width: 100%;
		    height: auto !important;
		    display: block; 
		    margin: 0px auto; }"),
shiny::tags$style("box{ max-width: 100%;
		    height: auto !important;
		    display: block; 
		    margin: 0px auto; }"),
## CSS-Code ###############
inlineCSS("
            #anovadata .table td {
             text-align: center;
            }
            
            #pathdata .table td {
            text-align: center;
            }
            #ccleanovadata .table td {
            text-align: center;
            }
            "
),
##java
shinyjs::useShinyjs(),
extendShinyjs("./www/app-shinyjs.js", functions = c("updateScrollspy")),
includeCSS("./www/app.css"),
includeScript("https://cdnjs.cloudflare.com/ajax/libs/jquery-scrollTo/1.4.3/jquery.scrollTo.min.js"),
#js function to reset a button, variableName is the button name whose value we want to reset
shiny::tags$script(HTML('Shiny.addCustomMessageHandler("resetInputValue", function(variableName){
                Shiny.onInputChange(variableName,null);
                });
                ')),
shiny::tags$div(class="container", #
                navbarPage(position = c("fixed-top"),
                           title = "AS-CMC",
                           windowTitle = HTML("AS-CMC</title> <link rel='icon' type='image/gif/png' href='2.ico'>"),
                           theme=shinytheme('flatly'),id ="inTabset",
                           header = div(useShinydashboard()),
                           # Home ==========================================================
                           shiny::tabPanel("Home", icon = icon("home"),
                                           fluidPage(
                                             br(),br(),br(),br(),
                                             shiny::tags$h1(strong("A pan-cancer databases of Alternative Splicing for Cancer Molecular Classification"), align= "center" ),
                                             br(),br(),
                                             fluidRow(width =12,
                                                      column(width= 5, offset=1,
                                                             shinydashboard::box(width =12, title ="About AS-CMC",
                                                                                 status ="warning", solidHeader = T,
                                                                                 p(style="font-family:  Arial; font-size: 20px","AS-CMC has a user-friendly interface, which allows researchers to explore alternative splicing (AS) events in The Cancer Genomics Atlas (TCGA) molecular subtypes. Our web service consists of two parts, viz. “Single-cancer AS” and “Pan-cancer AS.” In the “Single-cancer AS,” users can select cancer type first and get the list of AS events with the statistical analysis results. In the “Pan-cancer AS,” users can obtain pan-cancer views for a selected AS event."))),
                                                      column(width= 4,includeHTML("./www/data/new/12.help/html/home_events.html"))),
                                             fluidRow(column(width = 10, offset = 1,
                                                             div(style = "height:1650px",
                                                                 br(),br(),
                                                                 includeHTML("./www/data/new/12.help/html/0.home.html")
                                                             )),
                                             ),
                                             fluidRow(column(width=10, offset = 1,
                                                             box(title = " TCGA (24 Cancers) ", width = 16, 
                                                                 column(width=4, style = "height: 80vh;",home.plt1),
                                                                 column(width=4, style = "height: 80vh;",home.plt2),
                                                                 column(width=4, style = "height: 80vh;",home.plt3),
                                                                 column(12,
                                                                        div(style="height:100px",
                                                                            includeHTML("./www/data/new/12.help/html/0.home2.html"))
                                                                        )))))
                                           ),#
                           # single-cancer AS ==========================================================<i class="fa-solid fa-cube"></i>
                           shiny::tabPanel(title = "Single-cancer AS",icon = icon('fas fa-th-list'),value = "cancer",
                                           fluidPage(
                                             br(),br(),br(),br(),
                                             shiny::tags$div(HTML(' <i class="fas fa-arrow-circle-right"></i> &nbsp;<font size = "5" >
                                                                      Approaching for identifying molecular subtype-specific AS events in cancers.</font>')),
                                             br(),
                                             # search ============
                                             box(width = 16,solidHeader = T, status = "primary", collapsible = T, 
                                                 title = shiny::tags$div(HTML('<i class="fas fa-search"></i> &nbsp; <font size = "4" > Selection </font>')),
                                                 br(),
                                                 fluidRow(
                                                   column(3, shiny::selectInput("cancertype", label = "Cancer type (*Requried)", choices = tcga.abb$name),
                                                   tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                                                                                       #cancertype+ div>.selectize-input{width: 350px !important; }"))))),
                                                 shiny::tags$div(HTML('<i class="fas fa-search-plus"></i> &nbsp; <font size = "4" > Priority : Filtering ASEs or genes </font>')),
                                                 br(),
                                                 fluidRow(
                                                   column(3, shiny::selectInput("surtype", label = "Associated survival", choices = c("All", "LogRank: p < 0.05")),
                                                          tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                                                          #surtype+ div>.selectize-input{width: 350px !important; }")))),
                                                   column(3, shiny::selectInput("exptype", label = "Correlation of PSI to gene expression", 
                                                                                choices = c("All", "Yes in correlation, |r| >= 0.5", "No in correlation, |r| < 0.5")),
                                                          tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                                                          #exptype+ div>.selectize-input{width: 350px !important; }")))))),
                                             # search summary ====================
                                             shiny::tags$table(style = "width :100%",
                                                        tags$tr(tags$td(style = "width :5%",align= "right",
                                                                        actionButton("reset", "Priority reset", width = 150,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                                tags$td(shiny::tags$div(HTML("&nbsp;&nbsp"))),
                                                                tags$td(htmlOutput("select.pri")))),
                                             br(),
                                             fluidRow(column(12, hr(style = "border-top: 5px double #bbb"))),
                                             fluidRow(sidebarLayout(# Summary ====================
                                                                    sidebarPanel(width = 2, height = 800,
                                                                                 h4(shiny::tags$div(HTML('<i class="fas fa-square"></i> &nbsp; <font size="3"> Molecular sutypes</font>'))),
                                                                                 htmlOutput("tcga.txt"),br(),
                                                                                 h4(shiny::tags$div(HTML('<i class="fas fa-square"></i> &nbsp; <font size="3"> Filtered ASEs distribution </font>'))),
                                                                                 plotlyOutput("tcga.as.type", height=300)),
                                                                    # main table============
                                                                    mainPanel(width = 10,
                                                                              tags$style(".nav-tabs {background: white;}
                                                                              .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {background-color: #fff;
                                                                              border-color: #fff;}
                                                                              .nav-tabs-custom .nav-tabs li.active {border-top-color: #314a6d;}"),
                                                                              tags$head(tags$style("#anovadata .table {table-layout: fixed;}")),
                                                                              shinycssloaders::withSpinner(DT::dataTableOutput("anovadata")),
                                                                              uiOutput("modal"),
                                                                              tags$head(tags$style("#modal
                                                                              .modal-lg {width: 60%;}")))))
                                             )),
                           # pan-cancer AS ==========================================================<i class="fa-solid fa-cubes"></i>
                           shiny::tabPanel(title = "Pan-cancer AS", icon = icon("fa-solid fa-cubes"),value = "gene",
                                           fluidPage(br(),br(),br(),br(),
                                           shiny::tags$div(HTML(' <i class="fas fa-arrow-circle-right"></i> &nbsp;<font size = "5" >
                                                                      Exploring molecular subtype-specific AS events across cancers.</font>')),
                                           br(),
                                           # search ============
                                           box(width = 16, solidHeader = T, status = "primary", collapsible = T, 
                                               title = shiny::tags$div(HTML('<i class="fas fa-search"></i> &nbsp; <font size = "4" >  Priority : Filtering ASEs or genes </font>')),
                                               br(),
                                               fluidRow(
                                                 column(3, shiny::selectInput("astype", label = "AS event types", choices = c("All", "ES","RI","AA", "AD","ME"), selected = "ES"),
                                                        tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                                                          #astype+ div>.selectize-input{width: 350px !important; }")))),
                                                 column(3, shiny::selectInput("p_max", label = "Anova p.value", choices = c("All", "p < 1e-06")),
                                                        tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                                                          #p_max+ div>.selectize-input{width: 350px !important; }")))),
                                                 column(3, shiny::selectInput("pathtype", label = "Biological pathways", choices = c("All",names(path.whole.name))),
                                                        tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                                                                             #pathtype+ div>.selectize-input{width: 350px !important; }"))))
                                                 )
                                               ),
                                           # search summary ====================
                                           shiny::tags$table(style = "width :100%",
                                                             tags$tr(tags$td(style = "width :5%",align= "right",
                                                                             actionButton("pan.reset", "Priority reset", width = 150,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                                     tags$td(shiny::tags$div(HTML("&nbsp"))),
                                                                     tags$td(htmlOutput("pan.select.pri")))),
                                           br(),
                                           fluidRow(column(12,hr(style = "border-top: 5px double #bbb"))),
                                           fluidRow(sidebarLayout(# Summary ====================
                                                                  sidebarPanel(width = 3, height = 800,
                                                                               h4(shiny::tags$div(HTML('<i class="fas fa-square"></i> &nbsp; <font size="3"> Selected genes from pathway</font>'))),
                                                                               htmlOutput("pathgene")),
                                                                  # main table============
                                                                  mainPanel(width = 8, height = 1000,
                                                                            shinycssloaders::withSpinner(DT::dataTableOutput("panAA")),
                                                                            uiOutput("aamodal"),
                                                                            tags$head(tags$style("#aamodal
                                                                            .modal-lg {width: 60%;}")))))
                                           )),
                           # Downalod ==========================================================
                           shiny::tabPanel("Download",icon = icon("download"),
                                           br(),br(),br(),br(),br(),br(),
                                           fluidRow(column(12, offset = 1, tags$head(tags$style(".btn { width: 30%;}")),
                                                           downloadButton("table1", "Table1_Molecular_classification_for_cacner_type.xlsx"
                                                                             ,style ="color: #fff; background-color: #16a68a; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; text-align: left;")),
                                                    column(12,offset = 1, downloadButton("table2", "Table2_Number_of_samples_for_cancer_type_and_subtypes.xlsx"
                                                                             ,style ="color: #fff; background-color: #16a68a; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; text-align: left;")),
                                                    column(12,offset = 1, downloadButton("table3", "Table3_Number_of_subtype-specific_AS_events.xlsx"
                                                                             ,style ="color: #fff; background-color: #16a68a; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; text-align: left;")),
                                                    column(12,offset = 1, downloadButton("table4", "Table4_Pan-cancer_analysis_of_subtype-specific_AS.xlsx"
                                                                             ,style ="color: #fff; background-color: #16a68a; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; text-align: left;")))),
                           # Help ==========================================================
                           shiny::tabPanel("Help",  icon = icon("book"),
                                           div(id="Help-content",
                                               fluidPage(br(),br(),
                                                         column(
                                                           3,#div(style = "height:1000px;"),
                                                           br(),br(),br(),br(),
                                                           div( #
                                                             id = "Help-scrollspy",
                                                             class = "potential-scrollspy",
                                                             tags$ul(
                                                               class = "nav nav-pills nav-stacked",
                                                               tags$li(tags$a(href = "#section1-1", tags$div(HTML("<b>1. &nbspIntroduction</b>")))),
                                                               tags$li(tags$a(href = "#section1-2", tags$div(HTML("<b>2. &nbspAlternative splicing events</b>")))),
                                                               tags$li(tags$a(href = "#section1-3", tags$div(HTML("<b>3. &nbspTCGA molecular subtypes</b>")))),
                                                               tags$li(tags$a(href = "#section1-4", tags$div(HTML("<b>4. &nbspSignificant subtype-specific AS events</b>")))),
                                                               tags$li(tags$a(href = "#section1-5", tags$div(HTML("<b>5. &nbspPhenotype association</b>")))),
                                                               tags$li(tags$a(href = "#section1-6", tags$div(HTML("<b>6. &nbspSingle-cancer AS</b>")))),
                                                               tags$li(tags$a(href = "#section1-7", tags$div(HTML("&nbsp&nbsp 6-1. &nbspAS-level")))),
                                                               tags$li(tags$a(href = "#section1-8", tags$div(HTML("&nbsp&nbsp 6-2. &nbspPatient-level")))),
                                                               tags$li(tags$a(href = "#section1-9", tags$div(HTML("&nbsp&nbsp 6-3. &nbspTissue-level")))),
                                                               tags$li(tags$a(href = "#section1-10", tags$div(HTML("&nbsp&nbsp 6-4. &nbspGene-level")))),
                                                               tags$li(tags$a(href = "#section1-11", tags$div(HTML("<b>7. &nbspPan-cancer AS</b>")))),
                                                               tags$li(tags$a(href = "#section1-12", tags$div(HTML("&nbsp&nbsp 7-1. &nbspAS-level")))),
                                                               tags$li(tags$a(href = "#section1-13", tags$div(HTML("&nbsp&nbsp 7-2. &nbspPatientl-level")))),
                                                               tags$li(tags$a(href = "#section1-14", tags$div(HTML("&nbsp&nbsp 7-3. &nbspGene-level")))),
                                                               tags$li(tags$a(href = "#section1-15", tags$div(HTML("<b>8. &nbspOthers</b>")))),
                                                               tags$li(tags$a(href = "#section1-16", tags$div(HTML("&nbsp&nbsp 8-1. &nbspGenes related biological pathways"))))
                                                             )
                                                           )
                                                         ),
                                                         column(9, 
                                                           div(id = "section1-1",class = "scrollspy-section",  style = "height:250px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/1.introduction.html")
                                                           ),
                                                           div(id = "section1-2",
                                                               class = "scrollspy-section", style = "height:1600px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/2.TCGAspliceseq.html")
                                                           ),
                                                           div(id = "section1-3",
                                                               class = "scrollspy-section", style = "height:1100px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/3.TCGAmolecular.html")
                                                           ),
                                                           div(id = "section1-4",
                                                               class = "scrollspy-section", style = "height:1350px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/4.Anova.html")
                                                           ),
                                                           div(id = "section1-5",
                                                               class = "scrollspy-section", style = "height:1300px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/5.phenotype.html")
                                                           ),
                                                           div(id = "section1-6",
                                                               class = "scrollspy-section", style = "height:1300px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/6.Menu.single.html")
                                                           ),
                                                           div(id = "section1-7",
                                                               class = "scrollspy-section", style = "height:1400px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/6-1.AS-level.html")
                                                           ),
                                                           div(id = "section1-8",
                                                               class = "scrollspy-section",  style = "height:800px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/6-2.patient-level.html")
                                                           ),
                                                           div(id = "section1-9",
                                                               class = "scrollspy-section", style = "height:1200px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/6-3.tissue-level.html")
                                                           ),
                                                           div(id = "section1-10",
                                                               class = "scrollspy-section", style = "height:1000px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/6-4.gene-level.html")
                                                           ),
                                                           div(id = "section1-11",
                                                               class = "scrollspy-section", style = "height:1350px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/7.Menu.pan.html")
                                                           ),
                                                           div(id = "section1-12",
                                                               class = "scrollspy-section", style = "height:800px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/7-1.AS-level.html")
                                                           ),
                                                           div(id = "section1-13",
                                                               class = "scrollspy-section", style = "height:1000px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/7-2.patient-level.html")
                                                           ),
                                                           div(id = "section1-14",
                                                               class = "scrollspy-section", style = "height:450px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/7-3.gene-level.html")
                                                            ),
                                                           div(id = "section1-16",
                                                               class = "scrollspy-section", style = "height:800px",
                                                               br(),br(),
                                                               includeHTML("./www/data/new/12.help/html/8-2.Pathwaygene.html")
                                                           ))))
                                           ),
                           # Help ======================================
                           shiny::tabPanel("Contacts", icon = icon("far fa-envelope"),
                                           fluidRow(br(),br(),br(),br(),
                                                     column(width = 10,offset = 1,
                                                            includeHTML("./www/data/new/12.help/html/contact.html")))
                                           )
                           )))

