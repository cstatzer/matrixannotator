# source functions
source("./config.R")
source("./helper.R")

####################################################
####################### UI ######################### 
#################################################### 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Inputs", icon = icon("cog"), tabName = "Inputs_tab",
             badgeLabel = "required", badgeColor = "red"),
    hidden(menuItem("Data analysis", icon = icon("tasks"), tabName = "data_tab")),
#    menuItem("Orthology", icon = icon("paw"), tabName = "orthology"),    
    hidden(menuItem("Download / Export", icon = icon("print"), tabName = "Export_tab")),
    menuItem("Contact", icon = icon("envelope"), tabName = "Contact_tab"),
    menuItem(" Help & Information", icon = icon("info-circle"), tabName = "how_to_tab")
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    
    
    ############################################## Inputs_tab ######################################
    tabItem(tabName = "Inputs_tab",
            h1(tags$em("C. elegans Matrisome Annotator")),
            tags$br(),
            h2(tags$strong("Input: upload your dataset")),
            tags$br(),

                        
            fluidRow(
              box(title = div("Browse the", tags$em("C. elegans"), "matrisome"),width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                  dataTableOutput("df")
              )
            ),
            
            fluidRow(
              # upload box
              box(title = "Upload your gene list",width = 6,status = "danger",
                  fileInput("csv_upload", "Choose CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv",
                              ".xlsx")),
                  
                  textAreaInput("copy_paste", "Copy/Paste gene list input", ""),
                  br(), tags$hr(),
                  verbatimTextOutput("separated_input")
              ),
                
                box(title = "Select input options",width = 6,status = "danger",
                    textInput("personal_reference", "Personal reference: name your input data set", value = paste(Sys.Date(), "input"), width = NULL, placeholder = "Optional: enter a reference for file labelling"),
                
                    radioButtons("user_gene_id", "Please select the uploaded gene ID type",
                                     choiceNames = list(
                                       div("Ensembl / Wormbase ID, e.g. WBGene00000149",tags$em("(preferred)")),
                                       "Entrez Gene ID, e.g. 180783",
                                       div("Gene name / symbol, e.g.", tags$em("apl-1")),
                                       "Uniprot ID, e.g. Q10651"
                                     ),
                                     choiceValues = c("WORMBASE","ENTREZID","SYMBOL","UNIPROT"),
                                     selected = "WORMBASE")
                            )
              ),
            fluidRow(
              column(width = 2,offset = 5,
                     actionButton("upload_finished", "Apply import settings", style="color: #fff; background-color: #f45042; border-color: #ff1500")
            )
            ),
            

            
            fluidRow(
              dataTableOutput("translated_genes")
            ),
            fluidRow(
              tags$br(),
              tags$br(),
              tags$br(),
              dataTableOutput("unmapped_genes")
            ),
            fluidRow(
              textOutput("unmapped_genes_explanation")
            )
            
            
            ),
    
############################################## Data Analysis ######################################
    tabItem(tabName = "data_tab",
            h1(tags$strong("Results")),
            fluidRow(
              infoBoxOutput("info_box_genes_in_input"),
              infoBoxOutput("info_box_genes_mapped"),
              infoBoxOutput("info_box_genes_in_matrisome")

              
            ),
            
        tabsetPanel(
              tabPanel("Occurrence tables", 
                       tags$h3("Occurrence in matrisome"),
                       fluidRow(
                           formattableOutput("tab_formatted")
                       ),
                       tags$h3("Summary by matrisome divisions"),
                       fluidRow(
                         formattableOutput("tab_formatted_summary")
                       ),
                       tags$br(),
                       tags$br(),
                       tags$h3("Browse identified genes across matrisome divisions"),
                       fluidRow(
                         box(title = "Core matrisome",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                           dataTableOutput("user_divisions_table_1")
                         )
                       ),
                       fluidRow(
                         box(title = "Matrisome-associated",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                             dataTableOutput("user_divisions_table_2")
                         )
                       ),
                       fluidRow(
                         box(title = "Nematode-specific core matrisome",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                             dataTableOutput("user_divisions_table_3")
                         )
                       ),
                       fluidRow(
                         box(title = "Nematode-specific matrisome-associated",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                             dataTableOutput("user_divisions_table_4")
                         )
                       ),
                       fluidRow(
                         box(title = "All",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                             dataTableOutput("user_divisions_table_5_combined")
                         )
                       )
              ),
              tabPanel("Venn diagram",
                       # d3vennR::d3vennROutput("Venn_Division")
                       d3vennROutput("Venn_Division")
              ),
              tabPanel("Combined fractions",
                       fluidRow(
                         plotOutput("donut_plot_full",height = "700px",width = "900px")
                       ),
                       fluidRow(
                         plotOutput("donut_plot_user",height = "700px",width = "900px")
                       )
                       
              )
              # ###networkd3 issue:
              # ,
              # tabPanel("Matrisome layers",
              #          h2("Matrisome layers keeping categories separated"),
              #          fluidRow(
              #            sankeyNetworkOutput("sankey_out")
              #            ),
              #          h2("Matrisome layers with merged categories accross matrisome divisions"),
              #          fluidRow(
              #            sankeyNetworkOutput("sankey_out_summarized")
              #          )
              #          ),
              # tabPanel("Orthology network",
              #          fluidRow(
              #            forceNetworkOutput("network")
              #          )
              # )
        )
    ),

############################################## Orthology ######################################
# tabItem(tabName = "orthology",
#         h2("Explore orthologies between species in the matrisome")
#           
# ),
            
    
############################################## Download & Export ######################################
    tabItem(tabName = "Export_tab",
            h1(tags$strong("Export")),
            h3(div("Tables as",tags$em("Excel"), "and", tags$em("CSV"))),
            fluidRow(
              box(title = "Excel tables",width = 6,height = 370,solidHeader = TRUE,status = "danger",
                  "To work with the Excel download option your current MS Office version must support the", tags$em("'.xlsx'"), "file type.", tags$br(),
                  "The Excel file contains four tabs for each of the four matrisome divisions that were found in the gene list you uploaded plus one tab where all results found in your gene list are combined (Combined Matrisome). If for a particular division no overlap was identified this worksheet is empty apart from the header.",
                  #"To increase readability the", tags$em("Division"), "and the", tags$em("Category"), "columns are dynamically colored using the same colors as throughout the", tags$em("C. elegans") ," Matrisome Annotator website.",
                  tags$br(),tags$hr(),
                  tags$strong("Excel download options"),
                  checkboxInput(inputId = "excel_add_full_matrisome_tab",label = div("Add published matrisome", tags$em("(a local copy of the matrisome is added)")),value = TRUE),
                  checkboxInput(inputId = "excel_add_mapping_info",label = div("Add gene mapping information", tags$em("(the gene list you submitted is added)")),value = TRUE),
                  downloadButton("excel_sheet","Generate Excel file")
                  ),
              box(title = "CSV tables",width = 6,height = 370, solidHeader = TRUE,status = "danger",
                  "Download the output dataset as", tags$em("comma separated values"), "which can be easily read by multiple programs. Please select which table you would like to download.",
                  tags$br(),
                  tags$hr(),
                   radioButtons("csv_download_selection", "CSV download options",
                               choiceNames = list(
                                 div("Core matrisome",tags$em("- conserved")),
                                 div("Matrisome-associated",tags$em("- conserved")),
                                 div("Core matrisome",tags$em("- nematode specific")),
                                 div("Matrisome-associated",tags$em("- nematode specific")),
                                 div("Combined divisions",tags$em("- all of the above")),
                                 div("Published matrisome",tags$em("- complete matrisome"))
                                 ),
                               choiceValues = c("user_div_core_matrisome",
                                                "user_div_matrisome_associated",
                                                "user_div_ns_core_matrisome",
                                                "user_div_ns_matrisome_associated",
                                                "user_df",
                                                "df"),
                               selected = "user_df"),
                  downloadButton("csv_sheet","Generate csv file")
                  )
            ),
            
            h3(div("Report and images as",tags$em("PDF"), "and", tags$em("PNG"))),
            fluidRow(
              box(width = 6,height = 230,title = "PDF report",status = "primary",solidHeader = TRUE,
                  "The report contains the gene list you submitted (mapped and unmapped fraction) as well as the two tables displaying which parts of the", 
                  tags$em("C. elegans"), "matrisome were identified in your submitted list",
                  tags$br(),tags$hr(),
                  "Generate PDF report",
                  downloadButton("report","Report")
                  ),
              box(width = 6,height = 230,title = "PNG Images",status = "primary",solidHeader = TRUE,
                  "Table displaying matrisome components identified in your submitted gene list: ",
                  tags$br(),
                  downloadButton("download_tab_formatted","Divisions & Categories"),
                  downloadButton("download_tab_formatted_summary","Divisons (Categories pooled)"),
                  tags$br(),tags$hr(),
                  "Combined fractions of the submitted list and the entire matrisome:",tags$br(),
                  downloadButton("download_donut_plot_user",label = textOutput("personal_reference_2",inline = TRUE)     ),
                  downloadButton("download_donut_plot_full","Complete matrisome")
                  )
              ),
            
            h3(div("R Objects as",tags$em("RDS"),"file")),
            fluidRow(
              box(width = 12,title = "RDS file",status = "success",solidHeader = TRUE,
                  "If you work in R you can seamlessly continue the analysis by downloading the RDS file and loading it into your R session as described below:",
                  tags$br(),
                  tags$code("your_R_object <- readRDS('downloaded_RDS_file.rds')"),
                  tags$br(),tags$hr(),
                  "R data frame object of the fraction of the matrisome mapped to your specified input list and the complete",tags$em("C. elegans"),"matrisome",
                  tags$br(),
                  downloadButton("r_userdf_download",textOutput("personal_reference_1",inline = TRUE)),
                  downloadButton("r_df_download","Entire matrisome")
                  )
              )
  ),
  
  ############################################## Contact ######################################
  tabItem(tabName = "Contact_tab",
          h1(tags$strong("Contact")),
          fluidRow(
            column(width = 6,
                   div(img(src = "logo.png",height = 200, width = 430),style="text-align: center;")
            )
          ),
          h3("Original publication"),
          tags$br(),
          fluidRow(
            column(width = 9,
              div(img(src = "paper_header.png",height = 200, width = 760),style="text-align: center;")
              )
            ),
          tags$br(),
          tags$br(),
          tags$br(),
            
          # # Research group details
          # h3("Involved research groups and contact details"),
          # tags$br(),
          # fluidRow(
          #   column(
          #     width = 3,div(img(src = "groups_other.png",height = 200, width = 200),style="text-align: center;")
          #   ),
          #   column(width = 3,
          #          box(title = "Adress",width = 12,height = 200,status = "danger",
          #              "******",tags$br(),
          #              "******",tags$br(),
          #              "******",tags$br(),
          #              "******",tags$br(),
          #              tags$strong("******")
          #          )
          #   ),
          #   column(width = 3,
          #          box(title = "Contact",width = 12,height = 200, status = "danger",
          #              "******"
          #          )
          #   )
          # ),
          # tags$br(),
          # tags$br(),
          # 
          # fluidRow(
          #   column(
          #     width = 3,div(img(src = "groups_other.png",height = 200, width = 200),style="text-align: center;")
          #   ),
          #   column(width = 3,
          #          box(title = "Adress",width = 12,height = 200,status = "primary",
          #              "******",tags$br(),
          #              "******",tags$br(),
          #              "******",tags$br(),
          #              "******",tags$br(),
          #              tags$strong("******")
          #          )
          #   ),
          #   column(width = 3,
          #          box(title = "Contact",width = 12,height = 200, status = "primary",
          #              "******"
          #          )
          #   )
          # ),
          # tags$br(),
          # tags$br(),
          
          fluidRow(
            column(
              width = 3,div(img(src = "ewaldlogo_square.png",height = 200, width = 200),style="text-align: center;")
            ),
            column(width = 3,
                   box(title = "Adress",width = 12,height = 200,status = "success",
                       "ETH Zurich",tags$br(),
                       "Extracellular Matrix Regeneration Laboratory",tags$br(),
                       "Schorenstrasse 16",tags$br(),
                       "8607 Schwerzenbach",tags$br(),
                       tags$strong("Switzerland")
                   )
            ),
            column(width = 3,
                   box(title = "Contact us online",width = 12,height = 200, status = "success",
                       tags$a(href="https://ewaldlab.com","Group website")
                       # br(), tags$hr(),
                       # # twitter button
                       # tags$a(href="https://ewaldlab.com", "Tweet", class="twitter-share-button"),
                       # includeScript("http://platform.twitter.com/widgets.js")
                   )
            )
          ),
          tags$br(),
          tags$br(),
          fluidRow(
            column(width = 3,
                   div(img(src = "cyril.jpg",height = 200, width = 200),style="text-align: center;")
            ),
            column(width = 3,
                   box(title = "Adress",width = 12,height = 200,status = "danger",
                       "ETH Zurich",tags$br(),
                       "Extracellular Matrix Regeneration Laboratory",tags$br(),
                       "Schorenstrasse 16",tags$br(),
                       "8607 Schwerzenbach",tags$br(),
                       tags$strong("Switzerland")
                   )
            ),
            column(width = 3,
                   box(title = "Contact me directly",width = 12,height = 200,status = "danger",
                       tags$em(
                         tags$span("Contact email: "),
                         tags$a("Cyril Statzer", href = "mailto:cyrilstatzer@gmx.ch"),
                         tags$br(), tags$br()
                         )
                       )
                   )
            )
          ),

    ############################################## How to tab - help & information ######################################
    tabItem(tabName = "how_to_tab",
            h1("User tutorial for the", tags$em("C. elegans"), "Matrisome Annotator"),
            tags$br(),
            
            
            h3(tags$strong("Navigate the website")),
            tags$ol(
              tags$li('On the input page, you can search or manually browse through ',tags$em("C. elegans"),' matrisome by clicking on [+] of the “Browse ',tags$em("C. elegans"),' matrisome” field. '), 
              tags$br(),
              tags$li('Upload a CSV file or copy/paste your gene list in the “Upload your gene list” field. In order to be recognized, the CSV file should contain only one column and no header. Independent of how the gene list is submitted, a window will appear that displays how the gene list is parsed. Check that your data has been imported correctly. Under “Select input options”, choose the gene ID type used in your dataset (see ',tags$strong("Note 1"),' ). You have the option to name your analysis.'),
              tags$br(),
              tags$li('Click on the “Apply import settings” button. A table will appear displaying your input genes with their corresponding Gene name, Gene description, Wormbase ID, and Entrez gene ID. This table is searchable. The total number of mapped genes is displayed at the bottom left of the table. The genes which could not be mapped to Wormbase IDs are listed in the table below (see ',tags$strong("Note 2"),').'), 
              tags$br(),
              tags$li('Click on the “Data analysis” tab on the left-side bar. This tab appears when at least one submitted gene was successfully mapped'),
              tags$br(),
              tags$li('At the top of the “Data analysis” page you will see the number of identifiers submitted, how many were recognized, and how many were identified as being part of the',tags$em("C. elegans") ,'matrisome. Three tabs are provided to explore the matrisome signature of your gene list:',
                      tags$br(),
                      '1) The occurrence page lists the affiliations of each gene to their respective matrisome divisions and categories. Each gene can be further examined at the bottom of the page by clicking on [+] for the table of interest. The "In matrisome (%)" column, indicates the distribution of the matrisome genes of your dataset across the different matrisome categories.',
                      tags$br(),
                      '2) The Venn diagram is to scale and interactive and by hovering with the mouse over each area you can determine how many genes belong to each matrisome division.', 
                      tags$br(),
                      '3) The complete fraction page summarizes the distributions of both the published matrisome (719 genes) and the user-supplied list in a circular graph. The core section is split into the four matrisome divisions and each division is subdivided in its periphery into its corresponding categories.
                      '),
              tags$br(),
              tags$li('The Download / Export tab provides the option to download the data tables (Excel, CSV), illustrations (PDF and PNG) as well as R RDS files. We recommend to download the Excel file (both checkboxes checked) to have the entire matrisome analysis as well as your uploaded gene list (mapped and unmapped) in one file.')
            ),
            tags$br(),
            
            h3(tags$strong("Notes")),
            tags$ol(
              tags$li('In general, please use the most robust identifier, which are in decreasing order: Ensembl ID / WormBase ID (e.g., WBGene00000149), Entrez ID (e.g. 180783) and gene name / symbol (e.g., ',tags$em("apl-1"),'). '),
              tags$br(),
              tags$li('Problems with genes that fail to map: this could be due to multiple reasons: (A) copy / pasting errors, (B) selecting the wrong gene identifier, but also (C) through potential gene annotation changes. Failing to map genes is usually the case with retired gene/sequences names (e.g. ',tags$em("pqn-4"),' and T25B9.10). For instance, ',tags$em("C. elegans"),' official gene names and sequence names are regularly updated and thus are often not mapped anymore. If genes failed to map, go to ',tags$a(href="https://wormbase.org", "www.wormbase.org"),' and obtain the corresponding Ensembl ID / WormBase ID (e.g., WBGene00012016)')
              ),
            tags$br(),
            
            h3(tags$strong("Example")),
            tags$ul(
              tags$li('Please, see Supplementary Table 5: Analysis of microarray transcriptomic data (of this publication)'),
              tags$br(),
              tags$li('We used the differentially upregulated genes from the Supplementary Table 3 published in Ewald et al., 2015 ( ',tags$a(href="https://www.nature.com/articles/nature14021", "doi: 10.1038/nature1402"),', PMID: 25517099): link to '   , tags$a(href="https://media.nature.com/original/nature-assets/nature/journal/v519/n7541/extref/nature14021-s2.xlsx", "Excel sheet") ),
              tags$br(),
              tags$li('We copied the 429 genes found in column “Public Name” of the “SKN-1-dependent upregulated genes under reduced IIS (ranked by SAM score)” into the ',tags$em("C. elegans"),' Matrisome Annotator. 24 out of the 429 genes were not mapped due to retired genes or sequence names. For this reason, WormBase gene ID are preferred for gene input. We used WormBase to convert these 24 gene names to WormBase gene IDs manually and used this updated list of 427 genes as the input list for the ',tags$em("C. elegans"),' Matrisome Annotator. ')
            )
            
            
    )






))




# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Matrisome browser"),
  sidebar,
  body
)






### SERVER


server <- function(input, output) {
  
  
  ############################################## Inputs_tab ######################################
  
  # always copy paste gene input
  observeEvent(input$copy_paste, {
    output$separated_input <- renderText({ remove_delimiters(input$copy_paste) }) #display what was pasted by the user
  })
  

  output$personal_reference_1 <- renderText({ input$personal_reference }) #display what was pasted by the user
  output$personal_reference_2 <- renderText({ input$personal_reference }) #display what was pasted by the user
  output$personal_reference_3 <- renderText({ input$personal_reference }) #display what was pasted by the user
  output$personal_reference_4 <- renderText({ input$personal_reference }) #display what was pasted by the user
  
  
  output$df <- renderDataTable({ 
    values$df <- readRDS("./data/df.rds") #only read here for display purposes
    values$df[,c(1:4,6,7)]
    }) # to display to user
  
  
  # check and analyse the upload of both csv and copy paste
  observeEvent(input$upload_finished, {
    
    # test how the user uploaded the genes
    print("in upload_finished event")
    if (!is.null(input$csv_upload)){    
      submitted_genes <- upload(input$csv_upload)
      submitted_genes <- unname(unlist(submitted_genes))
      output$separated_input <- renderText({ remove_delimiters(submitted_genes) })
    } 
    if (!all(input$copy_paste %in% "")) submitted_genes <- input$copy_paste # copy paste is prioritized here

    ### remove delimiters
    values$submitted_genes <- remove_delimiters(submitted_genes)
    
    # Using clusterProfiler (offline)
    convtable <- tryCatch(
      {out <- bitr(values$submitted_genes, fromType=input$user_gene_id, toType=c("SYMBOL","GENENAME","WORMBASE","ENTREZID"), OrgDb="org.Ce.eg.db")
      setnames(out, old = c("SYMBOL","GENENAME","WORMBASE","ENTREZID"), new = c("wikigene_name","description","ensembl_gene_id","entrezgene")  )}, # preserves the original ordering of the table, first column remains first
      error = function(e)
      {
        msg <- "Invalid gene name for selected id:"
        out <- data_frame(error = msg,submitted_genes = values$submitted_genes)
        return(out)
      }
    )
    values$convtable <- convtable
    # enable additional menu items if at least one gene could be mapped.
    if (colnames(values$convtable)[1] != "error") {
      shinyjs::show(selector = "ul li:eq(1)", anim = TRUE)
      delay(ms = 270,expr =  shinyjs::show(selector = "ul li:eq(2)", anim = TRUE))
    }
    ### biomaRt (online)
    ### ensembl <- readRDS(file = "ensembl.rds")
    ### output_genes <- c("ensembl_gene_id", "entrezgene", "wikigene_name")
    ### print(values$submitted_genes) # submitted genes
    ### print(ensembl)
    ### values$convtable <- getBM(ensembl, attributes = output_genes,
    ####                    filters = input$user_gene_id, values = as.character(values$submitted_genes))
    print(values$convtable)
    values$unmapped <- values$submitted_genes[!values$submitted_genes %in% values$convtable[,1]]
    # saveRDS(values$convtable, file = "values_convtable.rds")
    # readRDS(file = "values_convtable.rds")
    output$translated_genes <- renderDataTable({ 
      out <- values$convtable
      setnames(out, old = c("wikigene_name","description","ensembl_gene_id","entrezgene"), new = c("Gene name","Gene description","Wormbase ID","Entrez gene ID")  ) # preserves the original ordering of the table, first column remains first
      out
      }) # to display to user
    output$unmapped_genes <- renderDataTable({
      # values$unmapped <- values$submitted_genes[!values$submitted_genes %in% values$convtable[,1]]
      out <- data.frame("Entered genes that could not be mapped" = values$unmapped, Message = rep(paste0("Invalid name for the selected gene ID type: ",input$user_gene_id),length(values$unmapped)), check.names = FALSE )
      datatable(out,
                options = list(
                  dom = 't',
                  pageLength = -1
                ))
      }) # to display to user
    output$unmapped_genes_explanation <- renderText({
      if(length(values$unmapped) == 0){
        NULL
      }else{
        paste("You encountered", length(values$unmapped) , "gene(s) that could not be mapped. This could be due to multiple reasons like copy / pasting errors, selecting the wrong gene identifier but also through potential gene annotation changes. In general, please use the most robust identifier which are in decreasing order: Ensembl ID, Entrez ID and gene name.")
      } 
    })
    
    
    # #import the specified matrisome
    # # if(input$species_of_origin == "Celegans") {
    #   df <- import_matrisome(matrisome_path = "./data/20181001_matrisome.csv",unique_column = "WormBaseID")
    #   values$df <- separate_orthologies(df)
    #   print("matrisome loaded")
    #   ## saveRDS(object = df,file = "./data/df.rds")
    #   ## df <- readRDS("./data/df.rds")
    #   print(nrow(values$df))
    # # }
    
    # matrisome overview table
    values$df <- readRDS("./data/df.rds")
    print(paste("nrow(values$df",nrow(values$df)))
    values$user_df <- values$df[values$df$WormBaseID %in% values$convtable$ensembl_gene_id,] #load specified matrisome
    main_grp <- "division"
    outer_grp <- "category"
    occ_full_mat <- build_occurence_table(values$df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
    occ_user_mat <- build_occurence_table(values$user_df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
    tab <- merge(x = occ_full_mat[,c(main_grp,outer_grp,"id","n","frac")],y = occ_user_mat[,c("id","n","frac")],by = "id",all = TRUE)
    tab$fold_enriched <- round(tab$frac.y/tab$frac.x,digits = 1)
    tab <- tab[c(main_grp,outer_grp,"n.x","n.y","frac.y","fold_enriched")]
    tab$frac.y <- round(tab$frac.y *100,digits = 0  )
    tab[is.na(tab)] <- ""
    colnames(tab) <- c(main_grp,outer_grp,"genesMatrisome","Genesinput","Percentage","Enrichment")
    values$tab <- tab[,c(main_grp,outer_grp,"genesMatrisome","Genesinput","Percentage")]
    
    # matrisome summary table
    main_grp <- "division"
    outer_grp <- NULL
    occ_full_mat <- build_occurence_table(values$df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
    occ_user_mat <- build_occurence_table(values$user_df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
    tab_summary <- merge(x = occ_full_mat[,c(main_grp,outer_grp,"id","n","frac")],y = occ_user_mat[,c("id","n","frac")],by = "id",all = TRUE)
    tab_summary$fold_enriched <- round(tab_summary$frac.y/tab_summary$frac.x,digits = 1)
    tab_summary <- tab_summary[c(main_grp,outer_grp,"n.x","n.y","frac.y","fold_enriched")]
    tab_summary$frac.y <- round(tab_summary$frac.y *100,digits = 0  )
    tab_summary[is.na(tab_summary)] <- ""
    tab_summary <- add_column(tab_summary, category = "all categories", .after = "division")
    colnames(tab_summary) <- c(main_grp,"category","genesMatrisome","Genesinput","Percentage","Enrichment")
    values$tab_summary <- tab_summary[, c(main_grp,"category","genesMatrisome","Genesinput","Percentage")]
    
    
    # generate the division sub-tables
    values$user_div_core_matrisome <- values$user_df[values$user_df$division %in% "Core matrisome",c(1:4,6)]
    values$user_div_matrisome_associated <- values$user_df[values$user_df$division %in% "Matrisome-associated",c(1:4,6)]
    values$user_div_ns_core_matrisome <- values$user_df[values$user_df$division %in% "Nematode-specific core matrisome",c(1:4,6)]
    values$user_div_ns_matrisome_associated <- values$user_df[values$user_df$division %in% "Nematode-specific matrisome-associated",c(1:4,6)]
    
    
    
    # saveRDS(object = values$user_df,file = "values_user_df.rds") # only for debugging
    
  })

  
  
  
  
  
  
  
  
  
  # ReactiveValues
  values <- reactiveValues(convtable = NULL, df = NULL, user_df = NULL, venn_data = NULL, tab = NULL, tab_summary = NULL, submitted_genes = NULL,
                           user_div_core_matrisome = NULL, user_div_matrisome_associated = NULL, user_div_ns_core_matrisome = NULL, user_div_ns_matrisome_associated = NULL,
                           tab_formatted = NULL, tab_formatted_summary = NULL, unmapped = NULL
  )
  
  
  
  ############################################## Data analysis ######################################
  # occurence table
    output$tab_formatted <- renderFormattable({
      plot <- values$tab
      plot <- setnames(plot, 
                                old = c("division","category","genesMatrisome","Genesinput","Percentage"), 
                                new = c("Division","Category","Total matrisome genes","Genesinput","In matrisome [%]")  ) # preserves the original ordering of the table, first column remains first
      
      list <- list(
        Division = formatter(
          "span",
          style = x ~ style(
            display = "block",
            "border-radius" = "5px",
            "text-align" = "center",
            "background-color" = color_divisions_in_matrisome(plot$Division)    )),
        
        Category = formatter(
          "span",
          style = x ~ style(
            display = "block",
            "border-radius" = "5px",
            "text-align" = "center",
            "background-color" = color_category_in_matrisome(plot$Category)    )),
        
        `Total matrisome genes` = color_bar("#adb7c6", na.rm = TRUE),
        Genesinput = color_bar(main_color, na.rm = TRUE),
        `In matrisome [%]` = formatter("span",style = x ~ style(color = main_color))
      )
      # dynamic naming
      setnames(plot,old = "Genesinput",new = input$personal_reference)
      names(list)[which(names(list) == "Genesinput")] <- input$personal_reference
      # save to object
      values$tab_formatted = formattable(plot, list)
       })
  
  # occurence table - SUMMARIZED by division
  output$tab_formatted_summary <- renderFormattable({
    plot <- values$tab_summary
    plot <- setnames(plot,
                     old = c("division","category","genesMatrisome","Genesinput","Percentage"),
                     new = c("Division","Category (combined)","Total matrisome genes","Genesinput","In matrisome [%]")  ) # preserves the original ordering of the table, first column remains first

    list <- list(
      Division = formatter(
        "span",
        style = x ~ style(
          display = "block",
          "border-radius" = "5px",
          "text-align" = "center",
          "background-color" = color_divisions_in_matrisome(plot$Division)   )),
      
      `Category (combined)` = formatter(
        "span",
        style = x ~ style(
          display = "block",
          "border-radius" = "5px",
          "text-align" = "center",
          "background-color" = "#a0a0a0"   )),
      

      `Total matrisome genes` = color_bar("#b3b8bf", na.rm = TRUE),
      Genesinput = color_bar(main_color, na.rm = TRUE),
      `In matrisome [%]` = formatter("span",style = x ~ style(color = main_color))
    )
    # dynamic naming
    setnames(plot,old = "Genesinput",new = input$personal_reference)
    names(list)[which(names(list) == "Genesinput")] <- input$personal_reference
    # save to object
    values$tab_formatted_summary = formattable(plot, list)
  })
  
  output$user_divisions_table_1 <- renderDataTable({ 
    datatable(values$user_div_core_matrisome,
              options = list(
                dom = 'tl',
                pageLength = 10,
                lengthMenu = c(0,5,10,100,500,1000),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#8594EF', 'color': '#000'});",
                  "}")
                )) %>% formatStyle('category',  color = '#000000', backgroundColor = '#cdd0d6', fontWeight = 'bold')
    }) 
  output$user_divisions_table_2 <- renderDataTable({ 
    datatable(values$user_div_matrisome_associated,
              options = list(
                # columnDefs = list(list(
                #   targets = 0, render = JS(itaJS)
                # )),
                dom = 'tl',
                pageLength = 10,
                lengthMenu = c(0,5,10,100,500,1000),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#DEBB9B', 'color': '#000'});",
                  "}")
              )) %>% formatStyle('category',  color = '#000000', backgroundColor = '#cdd0d6', fontWeight = 'bold')
    }) 

  output$user_divisions_table_3 <- renderDataTable({ 
    datatable(values$user_div_ns_core_matrisome,
              options = list(
                dom = 'tl',
                pageLength = 10,
                lengthMenu = c(0,5,10,100,500,1000),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#8594EF', 'color': '#000'});",
                  "}")
              )) %>% formatStyle('category',  color = '#000000', backgroundColor = '#cdd0d6', fontWeight = 'bold')
  }) 
  output$user_divisions_table_4 <- renderDataTable({ 
    datatable(values$user_div_ns_matrisome_associated,
              options = list(
                dom = 'tl',
                pageLength = 10,
                lengthMenu = c(0,5,10,100,500,1000),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#DEBB9B', 'color': '#000'});",
                  "}")
              )) %>% formatStyle('category',  color = '#000000', backgroundColor = '#cdd0d6', fontWeight = 'bold')
  }) 
  output$user_divisions_table_5_combined <- renderDataTable({ 
    table <- values$user_df[,c(1:4,6)]
    datatable(table,
              options = list(
                dom = 'tlf',
                pageLength = 10,
                lengthMenu = c(0,5,10,100,500,1000)
              ))
  }) 
  
  # Infoboxes
  # output$info_box_matrisome_size <- renderInfoBox({
  #   matrisome_size <- nrow(values$df)
  #   infoBox(title = "Matrisome",value = matrisome_size , icon = icon("table"),
  #           color = "light-blue")
  # })
  output$info_box_genes_in_input <- renderInfoBox({
    n_genes_uploaded <- length(values$submitted_genes)
    infoBox(title = "Genes uploaded",value = n_genes_uploaded , icon = icon("cloud-upload"),
            color = "red")
  })
  output$info_box_genes_mapped <- renderInfoBox({
    n_genes_mapped <- sum(values$submitted_genes %in% values$convtable[,1])
    infoBox(title = "Genes mapped",value = n_genes_mapped , icon = icon("filter"),
            color = "red")
  })
  output$info_box_genes_in_matrisome <- renderInfoBox({
    n_genes_in_matrisome <- nrow(values$user_df)
    infoBox(title = "In matrisome",value = n_genes_in_matrisome , icon = icon("search"),
            color = "red")
  })
  # output$info_box_frac_in_matrisome <- renderInfoBox({
  #   frac_in_matrisome <- round(nrow(values$user_df  / nrow(values$convtable) ) *100, digits = 0)
  #   infoBox(title = "In matrisome",value = paste0(frac_in_matrisome,"%") , icon = icon("ok"),
  #           color = "light-blue")
  # })
  
  output$Venn_Division <-  renderD3vennR({
    venn_data <- venn_diagram_division(submitted_division = values$user_df$division,
                                       matrisome_df = values$df, convtable = values$convtable,
                                       personal_reference = input$personal_reference)
    d3venn_drawer(data = venn_data,cols = c(unname(unlist(matrisome_color$division_all)),main_color)   )
  })
  
  # plot donut plots
  output$donut_plot_user <-  renderPlot({
    main_grp <- "division"
    outer_grp <- "category"
    occ_user_mat <- build_occurence_table(values$user_df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
    col <- unname(unlist(matrisome_color$division_all[unique(occ_user_mat$division)]))
    if(dim(occ_user_mat)[1] == 0 ) return(NULL)
    with(occ_user_mat, donuts(x = frac, group = division, labels = category, col = col,pie_label_size = 0.7,margins = c(2,2,0,6),legend_size = 1.2, draw_legend = TRUE, title = input$personal_reference  ) )
  }, height = 700, width = 1000, bg="transparent")
  output$donut_plot_full <-  renderPlot({
    main_grp <- "division"
    outer_grp <- "category"
    occ_full_mat <- build_occurence_table(df = values$df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
    col <- unname(unlist(matrisome_color$division_all[unique(occ_full_mat$division)]))
    browser()
    with(occ_full_mat, donuts(x = frac, group = division, labels = category, col = col,pie_label_size = 0.7,margins = c(2,2,0,6),legend_size = 1.2, draw_legend = TRUE, title = "Complete matrisome (as published)"  ) )
  }, height = 700, width = 1000, bg="transparent")
  
  # ### networkd3 issue
  # # sakney
  # output$sankey_out <- renderSankeyNetwork({
  #   # global target settings
  #   master_node_label = "user_list"
  #   negative_label = "remaining_matrisome"
  #   columns_for_sakney <- c("method","division","category","user_list")
  #   my_color <- 'd3.scaleOrdinal() .domain(["remaining_matrisome", "user_list", "user_list_nodes","remaining_matrisome_nodes"]) .range(["#cccac9\", \"#efa7a0\", \"#f45042\", \"#727171"])'
  # 
  #   # prepare dataframe
  #   df_sankey <- values$df
  #   df_sankey$method <- sapply(df_sankey$method, function(x) strsplit(x = x,split = '[()]')[[1]][[1]] )
  #   df_sankey$user_list <- negative_label
  #   df_sankey[df_sankey$WormBaseID %in% values$user_df$WormBaseID,]$user_list <- master_node_label
  #   
  #   df_sankey <- df_sankey[,columns_for_sakney]
  #   nematode_specific_idx <- which(df_sankey$division %in% c("Nematode-specific core matrisome","Nematode-specific matrisome-associated"))
  #   df_sankey[nematode_specific_idx,"category"] <- paste0(df_sankey[nematode_specific_idx,]$category,"_NS")
  #   cols <- colnames(df_sankey)
  #   for (c in 2:length(cols)) {
  #     print(paste("connecting columns:",cols[c-1], "with",cols[c]))
  #     new_connection <- df_sankey %>% group_by_(cols[c-1],cols[c]) %>% summarise(sum = n())
  #     colnames(new_connection)[1:2] <- c("IDsource","IDtarget")
  #     if(c==2) links <- new_connection
  #     if(c > 2) links <- rbind.data.frame(links, new_connection)
  #   }
  #   # generate nodes automatically
  #   nodes <- data.frame(name=unique(c(links$IDsource,links$IDtarget)))
  #   links$source <- match(links$IDsource,nodes$name) -1 # starting at 0
  #   links$target <- match(links$IDtarget,nodes$name) -1 # starting at 0
  #   links <- as.data.frame(links)
  #   # test link color
  #   links <- find_links_to_master_node(links = links,master_node_label = master_node_label,negative_label = negative_label)
  #   # assign node groups based on link connections
  #   involved_nodes <- unname(unlist(links[links$group == master_node_label,c("IDsource","IDtarget")]))
  #   nodes$group <- paste0(negative_label,"_nodes")
  #   nodes[nodes$name %in% involved_nodes,]$group <- paste0(master_node_label,"_nodes")
  #   nodes$group <- as.factor(nodes$group)
  #   
  #   sankeyNetwork(Links = links, Nodes = nodes,
  #                 Source = "source", Target = "target",
  #                 Value = "sum", NodeID = "name", colourScale = my_color,
  #                 fontSize= 12, nodeWidth = 30, LinkGroup="group", NodeGroup="group")
  # })
  # 
  # 
  # output$sankey_out_summarized <- renderSankeyNetwork({
  #   # global target settings
  #   master_node_label = "user_list"
  #   negative_label = "remaining_matrisome"
  #   columns_for_sakney <- c("method","division","category","user_list")
  #   my_color <- 'd3.scaleOrdinal() .domain(["remaining_matrisome", "user_list", "user_list_nodes","remaining_matrisome_nodes"]) .range(["#cccac9\", \"#efa7a0\", \"#f45042\", \"#727171"])'
  #   
  #   # prepare dataframe
  #   df_sankey <- values$df
  #   df_sankey$method <- sapply(df_sankey$method, function(x) strsplit(x = x,split = '[()]')[[1]][[1]] )
  #   df_sankey$user_list <- negative_label
  #   df_sankey[df_sankey$WormBaseID %in% values$user_df$WormBaseID,]$user_list <- master_node_label
  #   
  #   df_sankey <- df_sankey[,columns_for_sakney]
  #   # nematode_specific_idx <- which(df_sankey$division %in% c("Nematode-specific core matrisome","Nematode-specific matrisome-associated"))
  #   # df_sankey[nematode_specific_idx,"category"] <- paste0(df_sankey[nematode_specific_idx,]$category,"_NS")
  #   cols <- colnames(df_sankey)
  #   for (c in 2:length(cols)) {
  #     print(paste("connecting columns:",cols[c-1], "with",cols[c]))
  #     new_connection <- df_sankey %>% group_by_(cols[c-1],cols[c]) %>% summarise(sum = n())
  #     colnames(new_connection)[1:2] <- c("IDsource","IDtarget")
  #     if(c==2) links <- new_connection
  #     if(c > 2) links <- rbind.data.frame(links, new_connection)
  #   }
  #   # generate nodes automatically
  #   nodes <- data.frame(name=unique(c(links$IDsource,links$IDtarget)))
  #   links$source <- match(links$IDsource,nodes$name) -1 # starting at 0
  #   links$target <- match(links$IDtarget,nodes$name) -1 # starting at 0
  #   links <- as.data.frame(links)
  #   # test link color
  #   links <- find_links_to_master_node(links = links,master_node_label = master_node_label,negative_label = negative_label)
  #   # assign node groups based on link connections
  #   involved_nodes <- unname(unlist(links[links$group == master_node_label,c("IDsource","IDtarget")]))
  #   nodes$group <- paste0(negative_label,"_nodes")
  #   nodes[nodes$name %in% involved_nodes,]$group <- paste0(master_node_label,"_nodes")
  #   nodes$group <- as.factor(nodes$group)
  #   
  #   sankeyNetwork(Links = links, Nodes = nodes,
  #                 Source = "source", Target = "target",
  #                 Value = "sum", NodeID = "name", colourScale = my_color,
  #                 fontSize= 12, nodeWidth = 30, LinkGroup="group", NodeGroup="group")
  # })
  # 
  # output$network <- renderForceNetwork({
  #   network_data <- data_prep_for_networkD3(df = values$user_df)
  #   print("rendering network_data")
  #   plot_network(src = network_data$src, target = network_data$target)
  # } )
  # 


  
  
  ############################################## Download & Export ######################################
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file){
      tempReport <- file.path(tempdir(),"report_landscape.Rmd")
      file.copy("report_landscape.Rmd", tempReport,overwrite = TRUE) # save first to temp dir
      # params <- list(i = input$strain_factor_order, u = input$csv_upload) # parameters to pass to the markdown from the app
      # print("params")
      # print(params)
      # rmarkdown::render(tempReport,output_file = file,params = params,envir = new.env(parent = globalenv())) # set new environment (child) to isolate the markdown code
      rmarkdown::render(tempReport,output_file = file) # set new environment (child) to isolate the markdown code
    }
  )
  
  
  
  # UPDATE THE EXCEL OUTPUT
  # values$unmapped # could not be mapped
  # values$convtable # all the ones that were mapped
  # values$submitted_genes # complete list that was submitted

  output$excel_sheet <- downloadHandler(
        filename = function() {paste(input$personal_reference, ".xlsx", sep = "")},
        content = function(file) {
          data <- flatten_df_cols(df = values$user_df, remove_col_num = 5)
          full_matrisome <- flatten_df_cols(df = values$df, remove_col_num = 5)
          write_object_to_excel(data =  data  , file = file,matrisome_color = matrisome_color, Celeg_matrisome = input$excel_add_full_matrisome_tab, 
                                 full_matrisome = full_matrisome,mapping_info = TRUE,convtable = values$convtable, unmapped = values$unmapped )
          }
      )
  
  output$csv_sheet <- downloadHandler(
                   filename = function() {paste(input$personal_reference, ".csv", sep = "")},
                   content = function(file) {
                     if (input$csv_download_selection == "user_div_core_matrisome") table <- values$user_div_core_matrisome
                     if (input$csv_download_selection == "user_div_matrisome_associated") table <- values$user_div_matrisome_associated
                     if (input$csv_download_selection == "user_div_ns_core_matrisome") table <- values$user_div_ns_core_matrisome
                     if (input$csv_download_selection == "user_div_ns_matrisome_associated") table <- values$user_div_ns_matrisome_associated
                     if (input$csv_download_selection == "user_df") table <- values$user_df
                     if (input$csv_download_selection == "df") table <- values$df
                     table <- flatten_df_cols(df = table, remove_col_num = 5)
                     write.csv( table,file, row.names = FALSE)
                     }
                   )



  # download R files
  output$r_userdf_download <- downloadHandler(
    filename = function() {paste(input$personal_reference, ".rds", sep = "")},
    content = function(file) {
      rds <- values$user_df
      saveRDS(object =  rds,file = file)
    }
  )
  output$r_df_download <- downloadHandler(
    filename = function() {paste("Celegans_matrisome", ".rds", sep = "")},
    content = function(file) {
      rds <- values$df
      saveRDS(object =  rds,file = file)
    }
  )
  
  
  # download Images
  output$download_tab_formatted <- downloadHandler(
    filename = function() {paste(input$personal_reference, "_table.png", sep = "")},
    content = function(file) {
      export_formattable(values$tab_formatted,file = file)
    }
  )
  output$download_tab_formatted_summary <- downloadHandler(
    filename = function() {paste(input$personal_reference, "_table_summarized.png", sep = "")},
    content = function(file) {
      export_formattable(values$tab_formatted_summary,file = file)
    }
  )
  output$download_donut_plot_user <- downloadHandler(
    filename = function() {paste(input$personal_reference, "_combined_fractions_user.png", sep = "")},
    content = function(file) {
      main_grp <- "division"
      outer_grp <- "category"
      occ_user_mat <- build_occurence_table(values$user_df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
      col <- unname(unlist(matrisome_color$division_all[unique(occ_user_mat$division)]))
      if(dim(occ_user_mat)[1] == 0 ) return(NULL)
      png(file, width = 1200, height = 700, units = "px", pointsize = 12,
          bg = "white", res = NA)
      donuts <- donuts(x = occ_user_mat$frac, group = occ_user_mat$division, labels = occ_user_mat$category, col = col,pie_label_size = 0.7,margins = c(2,2,0,6),legend_size = 1.2, draw_legend = TRUE, title = input$personal_reference  )
      print(donuts)
      dev.off()
    }
  )
  output$download_donut_plot_full <- downloadHandler(
    filename = function() {paste(input$personal_reference, "_combined_fractions_complete_matrisome.png", sep = "")},
    content = function(file) {
      main_grp <- "division"
      outer_grp <- "category"
      occ_full_mat <- build_occurence_table(values$df,main_grp = main_grp,outer_grp = outer_grp,add_id_column = TRUE)
      col <- unname(unlist(matrisome_color$division_all[unique(occ_full_mat$division)]))
      if(dim(occ_full_mat)[1] == 0 ) return(NULL)
      png(file, width = 1200, height = 700, units = "px", pointsize = 12,
          bg = "white", res = NA)
      donuts <- donuts(x = occ_full_mat$frac, group = occ_full_mat$division, labels = occ_full_mat$category, col = col,pie_label_size = 0.7,margins = c(2,2,0,6),legend_size = 1.2, draw_legend = TRUE, title = input$personal_reference  )
      print(donuts)
      dev.off()
    }
  )

  
  
  # make plotly plots

  
  
  
  }

### APP
shinyApp(ui, server) 

