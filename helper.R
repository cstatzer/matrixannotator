# helper


#############################################################
################ Install and load packages ##################
#############################################################
installPackages <- function(packageList=c(), biocPackageList=c(), loadAll=F){
  
  if ( length(setdiff(packageList, rownames(installed.packages()))) > 0 ) {
    install.packages(setdiff(packageList, rownames(installed.packages())))
  }
  
  if( length(setdiff(biocPackageList, rownames(installed.packages())))>0 ){source("http://bioconductor.org/biocLite.R")}
  for(package in setdiff(biocPackageList, rownames(installed.packages())) ){
    biocLite(package)
  }
  
  if(loadAll){
    for(package in packageList){library(package, character.only = TRUE)}
    for(package in biocPackageList){library(package, character.only = TRUE)}
  }
  
}

installPackages(packageList = CRAN_packageList,biocPackageList = biocPackageList,loadAll = TRUE)




#############################################################
################ Install and load packages ##################
#################"timelyportfolio/d3vennR"###################
d3venn_drawer <- function(data,cols = viridis(7),width = 900){
  d3vennR( data= data,
           # specify colors
           colours = htmlwidgets::JS(sprintf('d3.scale.category10().range(%s.map(function(col){return eval(col)}))',
                                             jsonlite::toJSON(lapply(
                                               cols
                                               ,function(color){
                                                 rgb <- t(col2rgb(color))
                                                 sprintf("d3.rgb(%s)",paste0(rgb,collapse=","))
                                               }
                                             ), auto_unbox=T)
           )),
           width = width,
           tasks = list(
                  htmlwidgets::JS('
                                  function(){
                                  var div = d3.select(this);
                                  // add a tooltip
                                  var tooltip = d3.select("body").append("div")
                                  .attr("class", "venntooltip")
                                  .style("position", "absolute")
                                  .style("text-align", "center")
                                  .style("width", 128)
                                  .style("height", 16)
                                  .style("background", "#333")
                                  .style("color","#ddd")
                                  .style("padding","2px")
                                  .style("border","0px")
                                  .style("border-radius","8px")
                                  .style("opacity",0);
                                  div.selectAll("path")
                                  .style("stroke-opacity", 0)
                                  .style("stroke", "#fff")
                                  .style("stroke-width", 0)
                                  // add listeners to all the groups to display tooltip on mousover
                                  div.selectAll("g")
                                  .on("mouseover", function(d, i) {
                                  // sort all the areas relative to the current item
                                  venn.sortAreas(div, d);
                                  // Display a tooltip with the current size
                                  tooltip.transition().duration(400).style("opacity", .9);
                                  tooltip.text(d.size + " genes");
                                  // highlight the current path
                                  var selection = d3.select(this).transition("tooltip").duration(400);
                                  selection.select("path")
                                  .style("stroke-width", 3)
                                  .style("fill-opacity", d.sets.length == 1 ? .4 : .1)
                                  .style("stroke-opacity", 1);
                                  })
                                  .on("mousemove", function() {
                                  tooltip.style("left", (d3.event.pageX) + "px")
                                  .style("top", (d3.event.pageY - 28) + "px");
                                  })
                                  .on("mouseout", function(d, i) {
                                  tooltip.transition().duration(400).style("opacity", 0);
                                  var selection = d3.select(this).transition("tooltip").duration(400);
                                  selection.select("path")
                                  .style("stroke-width", 0)
                                  .style("fill-opacity", d.sets.length == 1 ? .25 : .0)
                                  .style("stroke-opacity", 0);
                                  });
                                  }
                                  ')
      )
      )
}


############ Build dataframe for use in Venn

venn_diagram_division <- function(submitted_division,matrisome_df,convtable, personal_reference = "User submitted"){
  # assign user genes to divisions
  # data <- values$user_df$division
  out <- case_when(
    submitted_division %in% unique(matrisome_df$division)[1] ~ 0,
    submitted_division %in% unique(matrisome_df$division)[2] ~ 1,
    submitted_division %in% unique(matrisome_df$division)[3] ~ 2,
    submitted_division %in% unique(matrisome_df$division)[4] ~ 3
  )
  print("out")
  print(out)
  occtab <- table(out)
  
  mat_div <- list(
    # matrisome - always the same
    list("sets"= list(0), "label"= unique(matrisome_df$division)[1], "size"= sum(matrisome_df$division  %in%  unique(matrisome_df$division)[1])),
    list("sets"= list(1), "label"= unique(matrisome_df$division)[2], "size"= sum(matrisome_df$division  %in%  unique(matrisome_df$division)[2])),
    list("sets"= list(2), "label"= unique(matrisome_df$division)[3], "size"= sum(matrisome_df$division  %in%  unique(matrisome_df$division)[3])),
    list("sets"= list(3), "label"= unique(matrisome_df$division)[4], "size"= sum(matrisome_df$division  %in%  unique(matrisome_df$division)[4])),
    list("sets"= list(0, 1), "size"= 0),
    list("sets"= list(0, 2), "size"= 0),
    list("sets"= list(0, 3), "size"= 0),
    list("sets"= list(1, 2), "size"= 0),
    list("sets"= list(1, 3), "size"= 0),
    list("sets"= list(2, 3), "size"= 0)
  )
  
  # add submitted genes to Venn
  mat_div[[length(mat_div) + 1]] <- list("sets"= list(4), "label"= personal_reference, "size"= nrow(convtable))
  print(paste("nrow(convtable):",nrow(convtable)))
  print(convtable$ensembl_gene_id)
  
  for (i in 0:3 ) {
    print(i)
    size = 0
    if (as.character(i) %in% names(occtab)) size = unname(occtab[as.character(i)])
    mat_div[[length(mat_div) + 1]] <- list("sets"= list(as.numeric(i),4), "size"= size)
    print(paste("length(mat_div)",length(mat_div)))
  }
  return(mat_div)
}




#############################################################
################## general purpose fun ######################
#############################################################
remove_delimiters <- function(string, split = "[\\|\t\n ,+]+"){
  splitted <- strsplit(string, split = split)
  splitted <- unlist(splitted)
  return(splitted)
}

# Using BIOMART (online)
# mart=useMart("ENSEMBL_MART_ENSEMBL") # listDatasets(mart) #find the dataset of interest for the species you want to analyze
# get_ensembl <- function(dataset = "mmusculus_gene_ensembl"){
#   useMart(biomart = "ENSEMBL_MART_ENSEMBL",
#           dataset = dataset)
# }
# ensembl <- get_ensembl(dataset = "celegans_gene_ensembl")
# saveRDS(ensembl, file = "ensembl.rds")


upload <- function(inFile){
  if (is.null(inFile)) return(NULL)
  # test if excel file
  if ( grepl(x = inFile,pattern = ".xlsx") ){
    print("Excel file upload selected")
    excel_file <- read_excel(inFile$datapath)
    # if (ncol(excel_file) == 1) excel_file <- sapply(excel_file, function(x) str_split(x,pattern = ";") )
    return(excel_file)
  }
  # If not excel file test how the csv is separated
  firstline <- readLines(inFile$datapath, n = 1)
  sep <- ifelse( str_count(pattern = ",",string = firstline) > str_count(pattern = ";", string = firstline),   ",",    ";")
  # read csv using separator
  csv_file <- try(read.csv(inFile$datapath,stringsAsFactors = FALSE,sep = sep,header = FALSE)) # try to read uploaded file
  print(paste("CSV file upload selected, with sep =", sep))
  if(is(csv_file,"try-error"))  return("Error when importing the .csv file, please make sure you fulfill all import criteria described on the landing page: header as illustrated, .csv file (commas preferred), no empty lines or cells.")
  return(csv_file)
}






#############################################################
##################### Read  Matrisome #######################
#############################################################
# save the matrisome as csv file
# remove all rows so that the headers are at the top
import_matrisome <- function(matrisome_path = "./data/matrisome_celegans.csv",unique_column = "WormBaseID"){
  df <- read_csv(matrisome_path)
  # df <- readRDS("./df.rds")
  colnames(df) <- c("method","division","category","WormBaseID","Link","wikigene","systematic_wikigene","Ensembl_Compara_HUGO","Ensembl_Compara_EnsemblID","InParanoid_HUGO","InParanoid_EnsembleID","Homologene_HUGO","Homologene_EnsembleID","OrthoMCL_HUGO","OrthoMCL_EnsembleID")
  duplicated_rows <- duplicated(df[,"WormBaseID"])
  print(paste(sum(duplicated_rows),"rows were removed due to non-unique identifier in unique_column"))
  df <- df[!duplicated_rows,]
  df$combined_orthology <- ""
  return(df)
}

### clean up multiple entries per df cell
separate_orthologies <- function(df){
  for (i in 1:nrow(df)) {
    print(i)
    df$Ensembl_Compara_HUGO[i] = separated_entries_to_lists(df$Ensembl_Compara_HUGO[i] )
    df$InParanoid_HUGO[i] <- separated_entries_to_lists(df$InParanoid_HUGO[i] )
    df$Homologene_HUGO[i] <- separated_entries_to_lists(df$Homologene_HUGO[i] )
    df$OrthoMCL_HUGO[i] <- separated_entries_to_lists(df$OrthoMCL_HUGO[i] )
    # combine all orthologies
    combined_orthologies <- c( df$Ensembl_Compara_HUGO[i], df$InParanoid_HUGO[i], df$Homologene_HUGO[i], df$OrthoMCL_HUGO[i])
    combined_orthologies <- unlist( combined_orthologies,recursive = FALSE)
    combined_orthologies <- combined_orthologies[!combined_orthologies %in% c("",NA)] # remove whitespace
    if(length(combined_orthologies) == 0 ) combined_orthologies <- NA
    df$combined_orthology[i] <- list( unique(combined_orthologies) )
  }
  return(df)
}















build_occurence_table <- function(df,main_grp = "division",outer_grp = "method",add_id_column = FALSE,division_level_order = c("Core matrisome", "Matrisome-associated","Nematode-specific core matrisome", "Nematode-specific matrisome-associated") ){
  occ <- df %>%                                                           # load dataframe
    count_(vars = c(main_grp, outer_grp), sort = TRUE) %>%                # count occurences for every combination of the two specified columns
    mutate(frac = n / sum(n)) %>%                                         # compute fraction of occurences
    group_by_(main_grp) %>%                                               # group by division for the next mutate command
    mutate(fraction_per_group = sum(frac)) %>%                            # sum the fractions !per group!
    ungroup() %>%
    mutate(division = factor(division, levels = division_level_order)) %>%
    plyr::ddply(c("division","desc(frac)")) %>%                # order dataframe by specified group level order and then within group by fraction
    mutate(division = as.character(division))
    
  if (add_id_column){
    if( is.null(outer_grp)  ) id <- occ[[main_grp]]
    if( !is.null(outer_grp)  ) id <- paste(occ[[main_grp]],occ[[outer_grp]])
    occ$id <- id
  }
  return(occ)
}




#############################################################
################## Donought & Pie chart #####################
#############################################################
#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])

donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7, 1),pie_label_size = 1,legend_size = 0.7,margins = c(2,2,0,2), draw_legend = TRUE, title = "") {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col)){seq_along(ug)
    }else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  
  # start plotting
  par(xpd = T, mar = par()$mar + margins)
  plot.new()
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels,cex = pie_label_size,main = title)
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
  
  # Legend outside the plot on bottom
  if(draw_legend){
    legend(x = 1.5,y = 1,             # Location of legend
           xjust = .5,                          # Center legend box on x
           yjust = 1,                           # Top justify legend box on y
           horiz = FALSE,                        # Set legend horizontally
           legend = unique(group),          # Legend Text
           cex = legend_size,   # font size of the legend text
           col = col,          # Legend Element Colors
           pch = 19,                     # Symbol styles for legend elements
           title = "Matrisome Divisions",      # Legend Title
           title.col = "black",                # Legend title color
           box.lty = 1,                         # Legend box line type
           box.lwd = 0                         # Legend box line width
           )
  }

  par(mar=c(5, 4, 4, 2) + 0.1) # reset the plot environment to default
}



#############################################################
################### Clean up Matrisome ######################
#############################################################
separated_entries_to_lists <- function(string,pattern = c(";"),blacklist = c(NA,"","-"," ")){
  splited <- unlist(str_split(string = string,pattern = pattern))
  ws_rem <- sapply(splited, function(x){str_squish(x)})
  ws_rem <- unname(ws_rem)
  if (length(ws_rem) == 1){
    if (ws_rem %in% blacklist) {
      ws_rem <- NA
    }
  }
  return <- list( ws_rem )
  return(return)
}











color_divisions_in_matrisome <- function(data){
  data <- tolower(data)
  color <- case_when(
    grepl(x = data,pattern = "core") ~ matrisome_color$division_lighted$core,
    grepl(x = data, pattern = "associated") ~ matrisome_color$division_lighted$associated,
    TRUE ~ "black"
  )
  return(color)
}

color_category_in_matrisome <- function(data){
  data <- tolower(data)
  color <- case_when(
    grepl(x = data,pattern = "collagens") ~ matrisome_color$category_lighted$Collagens,
    grepl(x = data, pattern = "ecm glycoproteins") ~ matrisome_color$category_lighted$`ECM Glycoproteins`,
    grepl(x = data,pattern = "proteoglycans") ~ matrisome_color$category_lighted$Proteoglycans,
    grepl(x = data,pattern = "ecm regulators") ~ matrisome_color$category_lighted$`ECM Regulators`,
    grepl(x = data,pattern = "ecm-affiliated") ~ matrisome_color$category_lighted$`ECM-affiliated`,
    grepl(x = data,pattern = "secreted factors") ~ matrisome_color$category_lighted$`Secreted Factors`,
    grepl(x = data,pattern = "cuticlin") ~ matrisome_color$category_lighted$Cuticlin,
    TRUE ~ "black"
  )
  return(color)
}




export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  print("in webshot")
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}



#############################################################
################### Network generation ######################
#############################################################
data_prep_for_networkD3 <- function(df){
  ### prepare for network
  src <- c() # intialize empty gene list
  target <- c() # initialize empty orthology list
  for (row in 1:nrow(df)) {
    print(row)
    row_entries <- unlist(df[row,]$combined_orthology)
    if ( !is.na(  row_entries[1]  )  ) { #skip the rows with NA
      target <- c(target,row_entries)
      src <- c(  src, rep(df[row,]$wikigene,length(row_entries))  )
    }
  }
  return(list(src = src, target = target))
}

plot_network <- function(src, target){
  ColourScale <-  'd3.scaleOrdinal()
  .domain(["src", "target"])
  .range(["#f45042", "#600a01"]);'
  networkData <- data.frame(src, target, stringsAsFactors = FALSE)
  nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
  nodes$group <-ifelse(nodes$name %in% networkData$src, "src", "target")
  links <- data.frame(source = match(networkData$src, nodes$name) - 1,
                      target = match(networkData$target, nodes$name) - 1)
  forceNetwork(Links = links, Nodes = nodes, Source = "source",
               Target = "target", NodeID ="name", Group = "group",
               opacity = 1, opacityNoHover = 1,
               colourScale = JS(ColourScale),
               zoom = TRUE,fontSize = 10)
}



# facilitating export
flatten_df_cols <- function(df, colnames = c("Ensembl_Compara_HUGO","InParanoid_HUGO","Homologene_HUGO","OrthoMCL_HUGO","combined_orthology"),remove_col_num = NULL){
  for (cols in colnames) {
    df[[cols]] <- lapply(df[[cols]], function(x) {
      if (is.na(x[[1]])){ str <- ""
      }else{ str <- paste( unlist(x), collapse=', ')}
      str
    })
  }
  df <- df %>% mutate_all(as.character)
  if (!is.null(remove_col_num)) df <- df[,-remove_col_num]
  return(df)
}


# CSV export
write_object_to_excel <- function(data,matrisome_color,file,Celeg_matrisome = FALSE,full_matrisome,mapping_info = TRUE ,convtable, unmapped){
  
  wb <- createWorkbook()
  addWorksheet(wb, "Combined Matrisome")
  addWorksheet(wb, "Core matrisome")
  addWorksheet(wb, "Matrisome-associated")
  addWorksheet(wb, "NS core matrisome")
  addWorksheet(wb, "NS matrisome-associated")
  if(Celeg_matrisome) addWorksheet(wb, "Background - complete matrisome")
  
  # Define styling rules for DIVISION AND CATEGORY
  core <- createStyle(fontColour = "#eff0f2", bgFill = matrisome_color$division_all[["Core matrisome"]])
  associated <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$division_all[["Matrisome-associated"]])
  ns_core <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$division_all[["Nematode-specific core matrisome"]])
  ns_associated <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$division_all[["Nematode-specific matrisome-associated"]])
  collagens <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$category_lighted[["Collagens"]] )
  ECM_Glycoproteins <- createStyle(fontColour = "#eff0f2", bgFill = matrisome_color$category_lighted[["ECM Glycoproteins"]]  )
  Proteoglycans <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$category_lighted[["Proteoglycans"]]  )
  ECM_Regulators <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$category_lighted[["ECM Regulators"]]  )
  ECM_affiliated <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$category_lighted[["ECM-affiliated"]]  )
  Secreted_Factors <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$category_lighted[["Secreted Factors"]]  )
  Cuticlin <- createStyle(fontColour = "#9C0006", bgFill = matrisome_color$category_lighted[["Cuticlin"]]  )
  
  # Define additional styling
  italics <- createStyle(textDecoration = "italic")
  headerStyle <- createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")
  

  ## Entire matrisome
  # DIVISION
  writeData(wb, "Combined Matrisome", data)
  conditionalFormatting(wb, sheet = "Combined Matrisome", cols=1:2, rows=1:nrow(data)+1, type = "contains", rule = "Core matrisome", style = core)
  conditionalFormatting(wb, sheet = "Combined Matrisome", cols=1:2, rows=1:nrow(data)+1, type = "contains", rule = "Matrisome-associated", style = associated)
  conditionalFormatting(wb, sheet = "Combined Matrisome", cols=1:2, rows=1:nrow(data)+1, type = "contains", rule = "Nematode-specific core matrisome", style = ns_core)
  conditionalFormatting(wb, sheet = "Combined Matrisome", cols=1:2, rows=1:nrow(data)+1, type = "contains", rule = "Nematode-specific matrisome-associated", style = ns_associated)

  
  # Fill divisions & format headers
  addStyle(wb, sheet = "Combined Matrisome", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = "#8c95a3", border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  writeData(wb, "Core matrisome", data[data$division %in% "Core matrisome",])
  addStyle(wb, sheet = "Core matrisome", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = matrisome_color$division_all[["Core matrisome"]], border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  writeData(wb, "Matrisome-associated", data[data$division %in% "Matrisome-associated",])
  addStyle(wb, sheet = "Matrisome-associated", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = matrisome_color$division_all[["Matrisome-associated"]], border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  writeData(wb, "NS core matrisome", data[data$division %in% "Nematode-specific core matrisome",])
  addStyle(wb, sheet = "NS core matrisome", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = matrisome_color$division_all[["Nematode-specific core matrisome"]], border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  writeData(wb, "NS matrisome-associated", data[data$division %in% "Nematode-specific matrisome-associated",])
  addStyle(wb, sheet = "NS matrisome-associated", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = matrisome_color$division_all[["Nematode-specific matrisome-associated"]], border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  
  
  # set multi-sheet formatting:
  sapply(1:length(names(wb)), function(sheet) setColWidths(wb, sheet, cols = 1:ncol(data), widths = c(25,35,25,15,15,rep(x = 30,ncol(data)-5 ))     ) ) # column widths
  sapply(1:length(names(wb)), function(sheet) addStyle(wb, sheet, italics, rows = 1:nrow(data)+1, cols = c(5,6), gridExpand = TRUE)  )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "Collagens", style = collagens) )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "ECM Glycoproteins", style = ECM_Glycoproteins) )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "Proteoglycans", style = Proteoglycans) )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "ECM Regulators", style = ECM_Regulators) )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "ECM-affiliated", style = ECM_affiliated) )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "Secreted Factors", style = Secreted_Factors) )
  sapply(names(wb), function(sheet) conditionalFormatting(wb, sheet = sheet, cols=2:3, rows=1:nrow(data)+1, type = "contains", rule = "Cuticlin", style = Cuticlin) )
  
  
  
  if(Celeg_matrisome){
    writeData(wb, "Background - complete matrisome", full_matrisome)
    # italics
    addStyle(wb, sheet = "Background - complete matrisome", italics, rows = 1:nrow(full_matrisome)+1, cols = c(5,6), gridExpand = TRUE)  
    # column width
    setColWidths(wb, sheet = "Background - complete matrisome", cols = 1:ncol(full_matrisome), widths = c(25,35,25,15,15,rep(x = 30,ncol(full_matrisome)-5 ))  )  # column widths
    # header
    addStyle(wb, sheet = "Background - complete matrisome", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = "#bababa", border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(full_matrisome), gridExpand = TRUE)
    # division
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=1:2, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Core matrisome", style = core)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=1:2, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Matrisome-associated", style = associated)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=1:2, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Nematode-specific core matrisome", style = ns_core)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=1:2, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Nematode-specific matrisome-associated", style = ns_associated)
    # category
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Collagens", style = collagens)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "ECM Glycoproteins", style = ECM_Glycoproteins)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Proteoglycans", style = Proteoglycans)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "ECM Regulators", style = ECM_Regulators)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "ECM-affiliated", style = ECM_affiliated)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Secreted Factors", style = Secreted_Factors)
    conditionalFormatting(wb, sheet = "Background - complete matrisome", cols=2:3, rows=1:nrow(full_matrisome)+1, type = "contains", rule = "Cuticlin", style = Cuticlin)
  }
  
  if(mapping_info){
    addWorksheet(wb, "Successfully mapped genes")
    writeData(wb, "Successfully mapped genes", convtable)
    # italics
    addStyle(wb, sheet = "Successfully mapped genes", italics, rows = 1:nrow(convtable)+1, cols = "Gene name", gridExpand = TRUE)  
    # column width
    setColWidths(wb, sheet = "Successfully mapped genes", cols = 1:ncol(convtable), widths = c(rep(20,ncol(convtable)))  )  # column widths
    # header
    addStyle(wb, sheet = "Successfully mapped genes", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = "#409624", border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(convtable), gridExpand = TRUE)
    
    # unmapped genes
    addWorksheet(wb, "Unmapped genes") # not formatted except header
    df_unmapped <- data.frame(unmapped)
    colnames(df_unmapped) <- "Unmapped Genes"
    writeData(wb, "Unmapped genes", df_unmapped)
    # column width
    setColWidths(wb, sheet = "Unmapped genes", cols = 1:ncol(df_unmapped), widths = c(rep(20,ncol(df_unmapped)))  )  # column widths
    # header
    addStyle(wb, sheet = "Unmapped genes", createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",fgFill = "#c41c0d", border="TopBottom", borderColour = "#4F81BD"), rows = 1, cols = 1:ncol(df_unmapped), gridExpand = TRUE)
    
  } 
  
  
  # Save
  saveWorkbook(wb, file = file, TRUE)
  
  
  
  
}



#### sakney plot


find_links_to_master_node <- function(links,master_node_label = "Yes",negative_label = "No"){
  links$group <- negative_label
  current_lead <- master_node_label
  
  while (TRUE) {
    idx <- which(links$IDtarget %in% current_lead)
    if(length(idx) < 1) break
    links[idx,]$group <- master_node_label
    current_lead <- unname(unlist(links[idx,"IDsource"]))
  }
  links$group <- as.factor(links$group)
  return(links)
}

