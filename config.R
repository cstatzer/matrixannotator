CRAN_packageList = c(
  "survival",
  "survminer",
  "rms",
  "readxl",
  "ggplot2",
  "scales",
  "grDevices",
  "reshape2",
  "tibble",
  "viridis",
  "gridExtra",
  "grid",
  "gridGraphics",
  "gplots",
  "statmod",
  "readr",
  "xml2",
  "V8",
  "openxlsx",
  "pryr",
  "circlize",
  "tidyr",
  "data.table",
  "dplyr",
  "RColorBrewer",
  "xtable",
  "flexdashboard",   # tidypackages
  "shiny",
  "shinythemes",
  "googleAuthR",
  "shinyjs",
  "shinydashboard",
  "DT",
  "shinyDND",
  "shinyjqui",
  "knitr",
  "shinyWidgets",
  "dygraphs",
  "xts",
  "colourpicker",
  "stringr",
  "formattable",
  "readxl",
  "timevis",
  "htmltools",
  "networkD3",
  "d3vennR",
  "webshot",
  "kableExtra",
  "tinytex" # recommended for Rmd PDF output
)

biocPackageList <- c("biomaRt",
                     "org.Ce.eg.db",
                     "clusterProfiler")

# githubPackageList <- "timelyportfolio/d3vennR"

#others
# the webshot package requires MAC users to install phantomjs via homebrew
# brew install phantomjs



############################################################## 
########################## Colors ############################ 
############################################################## 
# general website
main_color <- "#f45042"
# matrisome colors
matrisome_color <- list()
matrisome_color$division <- list("core" = "#1336F4",
                                 "associated" = "#DB8530")
matrisome_color$division_lighted <- list("core" = "#8594EF",
                                 "associated" = "#DEBB9B")
matrisome_color$division_all <- list("Core matrisome" = "#1336F4",
                                 "Matrisome-associated" = "#DB8530",
                                 "Nematode-specific core matrisome" = "#8b9aef",
                                 "Nematode-specific matrisome-associated" = "#ce9e6d")
matrisome_color$category <- list("Collagens" = "#4BACFB", 
                                 "ECM Glycoproteins" = "#432E69", 
                                 "Proteoglycans" = "#8AF8ED",
                                 "ECM Regulators" = "#BFAD9A",
                                 "ECM-affiliated" = "#F4B076",
                                 "Secreted Factors" = "#F9CFEC",
                                 "Cuticlin" = "#D9F5D9")

matrisome_color$category_lighted <- list("Collagens" = "#4BACFB", 
                                 "ECM Glycoproteins" = "#845dcc", 
                                 "Proteoglycans" = "#8AF8ED",
                                 "ECM Regulators" = "#BFAD9A",
                                 "ECM-affiliated" = "#F4B076",
                                 "Secreted Factors" = "#F9CFEC",
                                 "Cuticlin" = "#D9F5D9")


# matrisome_color$division <-  lapply(matrisome_color$division, function(x) adjustcolor(col = x,alpha.f = 0.5))
matrisome_color$category <-  lapply(matrisome_color$category, function(x) adjustcolor(col = x,alpha.f = 0.5))
