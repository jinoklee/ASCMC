
load( "./www/data/server.RData")
stdb <- read.delim("./www/data/new/1.tcga/subtypedb.txt")


server <- function(input, output, session){
  # function
  shinyInput = function(FUN, len, id, ...){
    inputs = as.character(len)
    for (i in seq_len(len))
    {inputs[i] = as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  #### single-cancer ===============================================
  tcgadf <- reactive({
    df <- tcga.sig.df
    canty <- gsub("\\ \\(.*","",input$cancertype)
    df <- df[df$Cancer%in%canty,]
    df<- left_join(df, exp)
    df <- left_join(df,sur)
    cibersort_max <- subset(cibersort, select =c(Sid, Cancer,cibersort))
    hypoxia_max <- subset(hypoxia, select =c(Sid, Cancer, hypoxia))
    emt_max <- subset(emt, select =c(Sid, Cancer, emt))
    df <- left_join(df, cibersort_max)
    df <- left_join(df, hypoxia_max)
    df <- left_join(df, emt_max)
    
    if( input$exptype == "Yes in correlation, |r| >= 0.5"){
      df <- filter(df , abs(`PSI & Exp.`) >=  0.5 )
      }
    
    if(input$exptype == "No in correlation, |r| < 0.5"){
        df <- filter(df , abs(`PSI & Exp.`) <  0.5 )
    }
    
    if(input$surtype != "All"){
      df <- filter(df, Survival < 0.05 )
    }
    

    if(nrow(df) > 0){
      df$Anova.p <- formatC(df$Anova.p, format= "e", digit= 2)
      df$`PSI & Exp.` <- formatC(df$`PSI & Exp.`,digit= 1 )
      df$Survival <- formatC(df$Survival, format= "e", digit= 2)
      df$cibersort <-formatC(df$cibersort,digit= 1) 
      df$hypoxia <- formatC(df$hypoxia,digit= 1)
      df$emt <- formatC(df$emt,digit= 1)
      colnames(df)[8:ncol(df)] <- c("ANONA (p.value)", "Correlation PSI & Exp (r.value)", "Survival (p.value)","Immunity (r.value)","Hypoxia (r.value)","EMT (r.value)")
      Plot = shinyInput(actionLink, nrow(df), 'button_', label = "Click", onclick = 'Shiny.onInputChange(\"info_button\",  this.id)' )
      df <- df[,c(1:8,10,9,11:ncol(df))]
      df <- cbind(df[,1,drop=F],Plot,df[,2:ncol(df)])
    }
    df
   })%>%bindCache(input$cancertype, input$exptype, input$surtype)
  

  observeEvent(input$reset, {
    updateSelectInput(inputId = "exptype", choices = c("All", "Yes in correlation, |r| >= 0.5", "No in correlation, |r| < 0.5"), selected  = "All")
    updateSelectInput(inputId = "surtype", choices = c("All", "LogRank: p < 0.05"), selected = "All")
  })

  
  output$select.pri <- reactive({
    canty <- gsub("\\ \\(.*","",input$cancertype)
    canty <- gsub(" ","", canty)
    
    exptype <- input$exptype
    surtype <- input$surtype
    
    if(input$surtype !="All"){
      surtype <- paste0('<span style=\"color:white; font-weight:bold; padding:7px;border-radius: 1em ;background-color:#A9A9A9 \">',"&" ,' </span>',shiny::HTML("&nbsp;&nbsp"),
                        '<span style=\"color:#00873E; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ',"Survival is ",' </span>',shiny::HTML("&nbsp;&nbsp"),
                        '<span style=\"color:#00873E; font-weight:bold; padding:7px;border-radius: 1em ;border:3px solid  skyblue\"> ', input$surtype ,' </span>',shiny::HTML("&nbsp;&nbsp"))
    }else{surtype <- ""}
    
    if(input$exptype != "All"){
      exptype <- paste0('<span style=\"color:white; font-weight:bold; padding:7px;border-radius: 1em ;background-color:#A9A9A9 \">',"&" ,' </span>',shiny::HTML("&nbsp"),
                        '<span style=\"color:#194775; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ', 
                        "Correlation of PSI to Gene Expression is ",' </span>',shiny::HTML("&nbsp;&nbsp"),
                        '<span style=\"color:#194775; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ', input$exptype,' </span>',shiny::HTML("&nbsp"))
      }else{exptype <- ""}
      paste0('<span style=\"color:red; font-weight:bold; padding:7px;border-radius: 1em ;border:3px solid skyblue\">',"Cancer Type is ", ' </span>',shiny::HTML("&nbsp"),
             '<span style=\"color:red; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ', canty,' </span>',shiny::HTML("&nbsp"),
             surtype,exptype, shiny::HTML("&nbsp;&nbsp"))
  })

  output$tcga.txt <- renderText({
    canty <- gsub("\\ \\(.*","",input$cancertype)
    canty <- gsub(" ","", canty)
    dtt <- stdb[stdb$cancer.type==canty,]
    paste0("Significant AS in ", '<span style=\"color:red\">  ',canty ,' </span>',
           "  across ",'<span style=\"color:blue; font-weight:bold\">  ', dtt$n,' </span>',
           " molecular subtype: ", "<br>",
           '<span style=\"color:blue\"> ',"",dtt$value,' </span>', "")
  })
  
  output$tcga.as.type <- renderPlotly({
      df <- tcgadf()
      df <-  data.frame(as.matrix(df %>%group_by(Splice.Type)%>%summarise(n = length(Splice.Type))))
      colnames(df) <- c("Splice.Type","n")
      plot_ly(df, labels = ~Splice.Type, values = ~n, type = 'pie',
              textposition = "inside",
              textinfo = "label+percent",
              showlegend = FALSE,
              marker = list(colors = brewer.pal(5,"Pastel1"))) %>%
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)")
    })
  
  
  output$anovadata  <- DT::renderDataTable({
    DT::datatable({
      tcgadf()[,-1]}
      ,extensions=c('Scroller')
      ,selection = 'single'
      ,rownames = FALSE
      ,escape = FALSE
      ,options = list(
                    autoWidth=TRUE,
                    columnDefs = list(list(className = 'dt-center', targets = '_all'),
                                      list(width="15%", targets=4)
                                      ),
                    scroller = TRUE,
                    scrollX = FALSE,
                    scrollY = 400,
                    preDrawCallback = JS("function() {
                             Shiny.unbindAll(this.api().table().node()); }"),
                    drawCallback = JS("function() {
                          Shiny.bindAll(this.api().table().node()); } ")
                    
                    ))
  }, server = FALSE)


  
  observeEvent(input$info_button, {
    s <- as.numeric(strsplit(input$info_button, "_")[[1]][2])
    canty <- tcgadf()$Cancer[s]
    sid <- tcgadf()$Sid[s]
    survival <- paste0("TCGA/survival/1_sig/",canty,"/surv.", sid,".png")
    expression <- paste0("TCGA/boxplot/1_sig/",canty,"/",sid,".png")
    locus <- paste0("track/",tcgadf()$GeneName[s], sep="_",tcgadf()$Splice.Type[s], sep="_", tcgadf()$Exon[s],".png")
    ciber.immne <- paste0("TCGA/immune/cibersort/",canty,"/",sid,".png")
    purity <- paste0("TCGA/immune/leuk_purity/",canty,"/",sid,".png")
    emt <- paste0("TCGA/EMT/",canty,"/",sid,".png")
    csbj <- paste0("TCGA/csbj22/",canty,"/",sid,".png")
    scomparison <- paste0("TCGA/compare-anova-png/",sid, ".png")
    shcomparison <- paste0("TCGA/compare-hz-png/surv.",sid, ".png")
    gcomparison <- paste0("TCGA/compare-gene-png/",sid, ".png")
    
    load(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/immune/cibersort/",tcgadf()$Cancer[s],"/",tcgadf()$Sid[s],".RData")) 
    
    
    if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/hypoxia/",canty,"/",sid,".png"))){
      hypoxia<-paste0("/data11/jinoklee/ASCMC/app/www/TCGA/hypoxia/",canty,"/",sid,".png")
    }
    
    expcordf <- read.delim(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/coregul-gene/",canty,"/",sid,".tbl"))
    expcordf <- expcordf[,c(1,5,6,7,11,12)]
    expcordf$r <- formatC(expcordf$r,digit= 2)
    expcordf$r.1 <- formatC(expcordf$r.1,digit= 2)
    
    
    if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/annotation/",sid,".RData"))){
      load(paste0("/data11/jinoklee/ASCMC/app/www/annotation/",sid, ".RData"))
    }
    
    if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/expcor_pan/heatmap/",sid,".RData"))){
      load(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/expcor_pan/heatmap/",sid,".RData"))
    }
    
    
    output$scorgenehtml<- downloadHandler(
      filename = function(){
        paste0(sid, ".html")
      },
      content = function(file) {
        file.copy(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/coregul-gene-html/",canty,"/",sid,".html"), file)
      }
    )
    
    output$scoreghtml <- downloadHandler(
      filename = function(){
        paste0(sid, ".html")
      },
      content = function(file) {
        file.copy(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/expcor_pan/html/",sid,".html"), file)
      }
    )
    

      output$modal <- renderUI({
        tagList(
          bsModal(paste('modal', s, sep = ''), paste( tcgadf()$Sid[s]), "info_button", size = "large",
                  shinydashboard::tabBox(width=12,
                                         tabPanel(title= "1. Subtype-specific AS",
                                                  shiny::tags$div(img(src = "TCGA/boxplot/guide.exp.png", width = "700px"),style="text-align: center;"),
                                                  br(),
                                                  br(),
                                                  shiny::tags$div(img(src = expression, width = "600px"),
                                                                  style="text-align: center;")),
                                         tabPanel(title = "2. Survival plot",
                                                  shiny::tags$div(img(src = "TCGA/survival/guide.survival.png", width = "400px"),
                                                                  style="text-align: center;"),
                                                  br(),
                                                  br(),
                                                  shiny::tags$div(img(src = survival, width = "500px"),
                                                                  style="text-align: center;")
                                         ),
                                         tabPanel(title = "3. Associated immunity (Cibersort)",
                                                  br(),
                                                  br(),
                                                  column(width = 6, offset = 3, 
                                                         if(exists("imm_heatmap")){
                                                           imm_heatmap
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         }),
                                                  column(width = 12,
                                                         br(),
                                                         h4(" < Top 9 Immune cell filteration > ", style="text-align: center;"),
                                                         shiny::tags$div(img(src = ciber.immne, width = "600px"),
                                                                         style="text-align: center;"),
                                                         br()
                                                  )
                                         ),
                                         tabPanel(title = "4. Associated purity fraction",
                                                  br(),
                                                  br(),
                                                  shiny::tags$div(img(src = purity, width = "800px"),
                                                                  style="text-align: center;")),
                                         tabPanel(title = "5. Associated hypoxia",
                                                  br(),
                                                  br(),
                                                  column(width = 12,
                                                         if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/hypoxia/",canty,"/",sid,".png"))){
                                                           shiny::tags$div(
                                                             img(src = hypoxia, width = "800px"),
                                                             style="text-align: center;") 
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         }
                                                         )
                                                  ),
                                         tabPanel(title = "6. Associated EMT",
                                                  br(),
                                                  br(),
                                                  column(width = 12,
                                                         if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/EMT/",canty,"/",sid,".png"))){
                                                           shiny::tags$div(
                                                             img(src = emt, width = "800px"),
                                                             style="text-align: center;") 
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         }
                                                  )
                                         ),
                                         tabPanel(title = "7. Associated splicesome pathway activity",
                                                  br(),
                                                  br(),
                                                  column(width = 12,
                                                         if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/csbj22/",canty,"/",sid,".png"))){
                                                           shiny::tags$div(
                                                             img(src = csbj, width = "800px"),
                                                             style="text-align: center;") 
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         }
                                                  )
                                         ),
                                         tabPanel(title= "8. Corregulated genes",
                                                  br(),
                                                  br(),
                                                  column(9),column(3,
                                                                   if(exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/coregul-gene-ggplotpng/",canty,"/",sid,".png"))){
                                                                     downloadLink("scorgenehtml", "Download HTML file")
                                                                   }),
                                                  column(width = 10,offset = 1,
                                                         shiny::tags$div(img(src =paste0("TCGA/coregul-gene-ggplotpng/",canty,"/",sid,".png"), width = "600px"),
                                                                         style="text-align: center;")),
                                                  column(12,br(),br(),br()),
                                                  column(12,
                                                         column(width = 12, align="center",style = "overflow-x: scroll;",
                                                                DT::datatable(expcordf,height = 500, width = 700,
                                                                              container = htmltools::withTags(table(
                                                                                class= 'display',
                                                                                thead(
                                                                                  tr(
                                                                                    th(rowspan = 2, "No"),
                                                                                    th(colspan = 3, "Negative correlation",style = "border-right: solid 1px;"),
                                                                                    th(colspan = 3, "Positive correlation")),
                                                                                  tr(th("Gene Name"),th("r.value"), th("p.value", style = "border-right: solid 1px;"),
                                                                                     th("Gene Name"),th("r.value"), th("p.value"))
                                                                                )
                                                                              )),
                                                                              options = list(dom = 't',autoWidth=T
                                                                                             ,columnDefs = list(#list(width='5%',targets=c(0)),
                                                                                               list(className = 'dt-center', targets = '_all')))
                                                                )%>%formatStyle(c(3), 'border-right'= "solid 1px")
                                                         ))
                                         ),
                                          tabPanel(title = "9. Annotation",
                                                  br(),
                                                  br(),
                                                  h4(" < Locus > ", style="text-align: center;"),
                                                  column(width = 12,
                                                         shiny::tags$div(img(src = locus,  width = "800px"),
                                                                         style="text-align: center;")),
                                                  br(),
                                                  br(),
                                                  column(width = 12, div(style = "height:80px;")),
                                                  br(),
                                                  br(),
                                                  h4(" < Annotation (Aspedia) > ", style="text-align: center;"),
                                                  column(width = 12, style = "overflow-x: scroll;", align="center",
                                                         if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/annotation/",tcgadf()$Sid[s], ".RData"))){
                                                           DT::datatable(df,
                                                                         extensions = 'RowGroup',
                                                                         options = list(rowGroup = list(dataSrc = 2),
                                                                                        pageLength = nrow(df),
                                                                                        lengthMenu = c(6, nrow(df)),
                                                                                        autoWidth=T,
                                                                                        columnDefs = list(list(targets = 2, visible = FALSE))),
                                                                         rownames = F
                                                           )
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         }
                                                         
                                                         )),
                                         tabPanel(title = "10.(Pan-cancer) Subtype-specific AS",
                                                  br(),
                                                  h4(" < Comparison of subtype-specific regulation > ", style="text-align: center;"),
                                                  shiny::tags$div(img(src = scomparison, width = "700px"),
                                                                  style="text-align: center;")
                                                  ),
                                         tabPanel(title = "11.(Pan-cancer) Survival",
                                                  shiny::tags$div(img(src = "images/guide.p.sur.png", width = "500px"),style="text-align: center;"),
                                                  br(),
                                                  h4(" < Comparison of clincial significance > ", style="text-align: center;"),
                                                  br(),
                                                  shiny::tags$div(img(src = shcomparison,  width = "500px"),
                                                                  style="text-align: center;")
                                         ),
                                         tabPanel(title = "12.(Pan-cancer) Gene expression",
                                                  br(),
                                                  h4(" < Comparison of gene expression correlation with self gene > ", style="text-align: center;"),
                                                  column(width = 12,align="center",
                                                           shiny::tags$div(img(src = gcomparison,  width = "450px"),
                                                                           style="text-align: center;")
                                                  ),
                                                  fluidRow(column(12, hr(style = "border-top: 5px double #bbb"))),
                                                  br(),
                                                  h4(" < Comparison of gene expression correlation with other genes > ", style="text-align: center;"),
                                                  column(9),column(3,
                                                                   if(exists("expcor_pan_heatmap")){
                                                                     downloadLink("scoreghtml", "Download HTML file")
                                                                   }),
                                                  column(width = 12,align="center",
                                                         if(exists("expcor_pan_heatmap")){
                                                           expcor_pan_heatmap
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         }
                                                  )
                                         ),
                                         )))})

     # =============
    
    toggleModal(session,paste('modal', s, sep = ''), toggle = "Assessment")
    #Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "info_button")
  })
  

  #### pan-cancer ===============================================
  
  observeEvent(input$pan.reset, {
    updateSelectInput(inputId = "p_max", choices = c("All", "p < 1e-06"), selected = "All")
    updateSelectInput(inputId = "astype",  choices = c("All", "ES","RI","AA", "AD","ME"), selected = "ES")
    updateSelectInput(inputId = "pathtype",  choices = c("All",names(path.whole.name)))
  })
  


  pandf <- reactive({
    df <- read.delim("/data11/jinoklee/ASCMC/app/www/TCGA/pan-cancer.tbl")

    if(input$p_max == " p < 1e-06"){
      df <- filter(df , df$min.p < 1e-06)
    }

    if(input$astype != "All" ){
      df <- filter(df , Splice.Type == input$astype)
    }

    if( input$pathtype != "All"){
      selgene <- path.whole.name[names(path.whole.name)%in%input$pathtype][[1]]
      df <- df[df$Gene.Name%in%selgene,]
    }
    if(nrow(df) > 0){
      Plot = shinyInput(actionLink, nrow(df), 'button_', label = "Click", onclick = 'Shiny.onInputChange(\"searcomp_button\",  this.id)' )
      cbind(Plot,df[,1:ncol(df)-1])
      
    }

  })
  
  output$pathgene <- renderText({
    
    if(input$pathtype !="All"){
      sel.g <- as.character(unique(pandf()$Gene.Name))
      g <- path.whole.name[names(path.whole.name)%in%input$pathtype][[1]]
      
      paste("Select", '<span style=\"color:red\"> ',length(sel.g), "genes (", paste(sel.g, collapse = " , "),")",'</span>',"out of", "<br>",length(g)," genes " ,"(" ,paste(g, collapse = " , "),")" )
      
    }else{
      "There is no select gene."
    }
  })
  
  output$pan.select.pri <- reactive({
    
    if(input$p_max !="All"){
      maxtype <- paste0('<span style=\"color:white; font-weight:bold; padding:7px;border-radius: 1em ;background-color:#A9A9A9 \">',"&" ,' </span>',shiny::HTML("&nbsp"),
                        '<span style=\"color:#00873E; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ',"Anova p.value ",' </span>',shiny::HTML("&nbsp;&nbsp"),
                        '<span style=\"color:#00873E; font-weight:bold; padding:7px;border-radius: 1em ;border:3px solid  skyblue\"> ', input$p_max ,' </span>',shiny::HTML("&nbsp;&nbsp"))
    }else{maxtype <- ""}
    
    if(input$pathtype != "All"){
      pathtype <- paste0('<span style=\"color:white; font-weight:bold; padding:7px;border-radius: 1em ;background-color:#A9A9A9 \">',"&" ,' </span>',shiny::HTML("&nbsp"),
                         '<span style=\"color:#194775; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ', 
                         "Biological pathway is",' </span>',shiny::HTML("&nbsp;&nbsp"),
                         '<span style=\"color:#194775; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">  ', input$pathtype,' </span>',shiny::HTML("&nbsp"))
    }else{pathtype <- ""}
    
    
    paste0( '<span style=\"color:red; font-weight:bold; padding:7px;border-radius: 1em ;border:3px solid skyblue\">',"ASE is ",' </span>',shiny::HTML("&nbsp"),
            '<span style=\"color:red; font-weight:bold; padding:7px; border-radius: 1em; border:3px solid  skyblue\">', input$astype,' </span>',shiny::HTML("&nbsp"),
            maxtype, pathtype,shiny::HTML("&nbsp;&nbsp"))
  })
  
  output$panAA<- DT::renderDataTable({
    shiny::validate(need(nrow(pandf()) > 0, "There is no significant AS events in selected pathway. Plesse select other pathway or All genes"))
    DT::datatable({
      pandf()[,-2]
    },extensions=c('Scroller')
    ,selection = 'single'
    ,rownames = FALSE
    ,escape = FALSE
    ,options = list(
    scroller = TRUE,
    scrollX = FALSE,
    scrollY = 400,
    autoWidth = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = '_all')),
    preDrawCallback = JS("function() {
                             Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() {
                          Shiny.bindAll(this.api().table().node());} ")
    ))
  }, server = FALSE)
  
 
  observeEvent(input$searcomp_button,{
    s <- as.numeric(strsplit(input$searcomp_button, "_")[[1]][2])
    sid <- pandf()$sid[s]
    name <- pandf()$Gene.Name[s]#data.frame(do.call('rbind',strsplit(as.character(sid),split = "_")))[[1]]
    event <- pandf()$Splice.Type[s]#data.frame(do.call('rbind',strsplit(as.character(sid),split = "_")))[[2]]
    exon <- pandf()$Exon[s]#data.frame(do.call('rbind',strsplit(as.character(sid),split = "_")))[[3]]
    locus <- paste0("track/",name, sep="_", event, sep="_", exon,".png")

    comparison <- paste0("TCGA/compare-anova-png/",sid, ".png")
    hcomparison <- paste0("TCGA/compare-hz-png/surv.",sid, ".png")
    gcomparison <- paste0("TCGA/compare-gene-png/",sid, ".png")
    
    if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/expcor_pan/heatmap/",sid,".RData"))){
      load(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/expcor_pan/heatmap/",sid,".RData"))
    }

    if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/annotation/",sid,".RData"))){
      load(paste0("/data11/jinoklee/ASCMC/app/www/annotation/",sid, ".RData"))
    }

    if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/compare_gene/",sid,".txt"))){
      load(file = "/data11/jinoklee/ASCMC/app/www/TCGA/compare_gene/clrs.RData")
      compare.gene.df <- read.delim(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/compare_gene/",sid,".txt"))
    }

    
    
    genedf<- fdf[fdf$GeneName%in%name,]
    genedf<- data.frame(t(genedf))
    colnames(genedf) <- genedf[1,]
    genedf<- genedf[-1,,drop = F]

    output$pcoreghtml <- downloadHandler(
      filename = function(){
        paste0(sid, ".html")
      },
      content = function(file) {
        file.copy(paste0("/data11/jinoklee/ASCMC/app/www/TCGA/expcor_pan/html/",sid,".html"), file)
      }
    )
    

      output$aamodal <- renderUI({
        tagList(
          bsModal(paste('aamodal',s, sep = ''), paste(sid) ,"searcomp_button", size = "large",
                  shinydashboard::tabBox(width=12,
                                         tabPanel(title = "1. subtype-specific AS",
                                                  br(),
                                                  br(),
                                                  h4(" < Comparison of subtype-specific regulation > ", style="text-align: center;"),
                                                  shiny::tags$div(img(src = comparison, width = "700px"),
                                                                  style="text-align: center;")),
                                         tabPanel(title = "2.Association with patient survival",
                                                  h4(" < Comparison of clincial significance > ", style="text-align: center;"),
                                                  br(),
                                                  shiny::tags$div(img(src = "images/guide.p.sur.png", width = "500px"),style="text-align: center;"),
                                                  shiny::tags$div(img(src = hcomparison,  width = "500px"),
                                                                  style="text-align: center;")
                                                  ),
                                         tabPanel(title = "3. Correlation with gene expression",
                                                  h4(" < Comparison of gene expression correlation with self gene > ", style="text-align: center;"),
                                                  column(width = 12,align="center",
                                                         shiny::tags$div(img(src = gcomparison,  width = "450px"),
                                                                         style="text-align: center;")),
                                                  fluidRow(column(12, hr(style = "border-top: 5px double #bbb"))),
                                                  br(),
                                                  h4(" < Comparison of gene expression correlation with other genes > ", style="text-align: center;"),
                                                  column(9),column(3,
                                                                   if(exists("expcor_pan_heatmap")){
                                                                     downloadLink("pcoreghtml", "Download HTML file")
                                                                   }),
                                                  column(width = 12,align="center",
                                                         if(exists("expcor_pan_heatmap")){
                                                           expcor_pan_heatmap
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         })),
                                         tabPanel(title = "4. Annotation",
                                                  h4(" < Locus > ", style="text-align: center;"),
                                                  column(width = 12,
                                                         shiny::tags$div(img(src = locus,  width = "800px"),
                                                                         style="text-align: center;")),
                                                  br(),
                                                  br(),
                                                  column(width = 12 , div(style = "height:80px;")),
                                                  br(),
                                                  br(),
                                                  h4(" < Annotation (Aspedia) > ", style="text-align: center;"),
                                                  column(width = 12,  style = "overflow-x: scroll;", align="center",
                                                         if(file.exists(paste0("/data11/jinoklee/ASCMC/app/www/annotation/",sid, ".RData"))){
                                                           shiny::div(width = "200px",
                                                                      DT::datatable(df,
                                                                         extensions = 'RowGroup',
                                                                         options = list(rowGroup = list(dataSrc = 2),
                                                                                        pageLength = nrow(df),
                                                                                        lengthMenu = c(6, nrow(df)),
                                                                                        columnDefs = list(list(targets = 2, visible = FALSE))),
                                                                         rownames = F))
                                                         }else{
                                                           h6("There is no data.",style="text-align: center;")
                                                         })),
                                         tabPanel(title = "5. Gene Information"
                                                  ,
                                                  br(),
                                                  br(),
                                                  column(width = 12,  style = "overflow-x: scroll;", align="center",
                                                         DT::datatable({
                                                           genedf
                                                         },
                                                         extensions=c('FixedColumns','Scroller'),
                                                         selection = 'single',
                                                         options=list(
                                                           dom = 't',
                                                           scrollY = 600,
                                                           scrollX = FALSE,
                                                           autoWidth = FALSE),
                                                         escape = F)
                                                  )
                                         )
                  )))})


    toggleModal(session,paste('aamodal', s, sep = ''), toggle = "Assessment")
    #Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "searcomp_button")
    
  })

    #### Download===========================================
  
  output$table1 <- downloadHandler(
    filename = function(){
      paste0( 'Table1_Molecular_classification_for_cacner_type.xlsx',"")
    },
    content = function(file){
      myfile <- srcpath <- '/data11/jinoklee/ASCMC/app/www/data/new/13.download/Table1_Molecular_classification_for_cacner_type.xlsx'
      file.copy(myfile, file)
    }
  )
  
  
  output$table2 <- downloadHandler(
    filename = function(){
      paste0( 'Table2_Number_of_samples_for_cancer_type_and_subtypes.xlsx',"")
    },
    content = function(file){
      myfile <- srcpath <- '/data11/jinoklee/ASCMC/app/www/data/new/13.download/Table2_Number_of_samples_for_cancer_type_and_subtypes.xlsx'
      file.copy(myfile, file)
    }
  )
  
  output$table3 <- downloadHandler(
    filename = function(){
      paste0( 'Table3_Number_of_subtype-specific_AS_events.xlsx',"")
    },
    content = function(file){
      myfile <- srcpath <- '/data11/jinoklee/ASCMC/app/www/data/new/13.download/Table3_Number_of_subtype-specific_AS_events.xlsx'
      file.copy(myfile, file)
    }
  )
  
  output$table4 <- downloadHandler(
    filename = function(){
      paste0( 'Table4_Pan-cancer_analysis_of_subtype-specific_AS.xlsx',"")
    },
    content = function(file){
      myfile <- srcpath <- '/data11/jinoklee/ASCMC/app/www/data/new/13.download/Table4_Pan-cancer_analysis_of_subtype-specific_AS.xlsx'
      file.copy(myfile, file)
    }
  )
  
  
  
  output$help <- renderImage({
    width  <- session$clientData$output_help_width
    height <- session$clientData$output_help_height
    
    list(src = "./www/help.png",
         width = 1000,
         height = 2600
         )}, deleteFile = FALSE)

  
  ### Help
  
  observeEvent(input$inTabset, {
    js$updateScrollspy(input$inTabset)
  })
  
}
