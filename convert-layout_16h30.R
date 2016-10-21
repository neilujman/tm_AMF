.libPaths("C:/R_library")
###BEGINNING OF PROPER CODE
###
setwd("C:\\Users\\jriton\\Documents\\R\\MesProjets\\AMF_textmining\\tm_declaration")




library(tm)
library(stringr)
library(magrittr)
library(xlsx)

system_date <- Sys.Date()
today <- format(system_date, "%Y-%m-%d")
# today <- "2016-09-27" #to remove
###SEUILS TYPE PARAGRAPHS
filesPath <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/seuils/"),
    pattern = "*.pdf$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

#TO DO IF condition filesPath not empty

filesNames <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/seuils/"),
    pattern = "*.pdf$",
    full.names = FALSE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

##converting PDF to text
textNames <-
    str_replace(string = filesNames,
                pattern = "pdf$",
                replacement = "txt")
filesPath <-
    gsub("^..", "", filesPath) %>% gsub("/", "\\\\", .) #removes dot and forward slash first character
#generates destination paths
textNames %<>% paste0("text_mining\\AMF-layout_16h30\\", today, "\\seuils\\", .)

#creates the directory
date_path <- paste0("text_mining/AMF-layout_16h30/", today, "/seuils/")
#date_path <- paste0("./data/seuils/", today, "")
if (!isTRUE(file.info(date_path)$isdir))
    dir.create(date_path, recursive = TRUE)

for (i in seq_along(filesNames)) {
    system(paste0("pdftotext -layout ", "\"", filesPath[i], "\" \"", textNames[i], "\""))
}
##

# ==================================================
# declaration des dirigeants

system_date <- Sys.Date()
today <- format(system_date, "%Y-%m-%d")
# today <- "2016-09-27" #to remove
###declaration des dirigeants TYPE PARAGRAPHS
filesPath <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/declaration/"),
    pattern = "*.pdf$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

#TO DO IF condition filesPath not empty

filesNames <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/declaration/"),
    pattern = "*.pdf$",
    full.names = FALSE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

##converting PDF to text
textNames <-
    str_replace(string = filesNames,
                pattern = "pdf$",
                replacement = "txt")
filesPath <-
    gsub("^..", "", filesPath) %>% gsub("/", "\\\\", .) #removes dot and forward slash first character
#generates destination paths
textNames %<>% paste0("text_mining\\AMF-layout_16h30\\", today, "\\declaration\\", .)

#creates the directory
date_path <- paste0("text_mining/AMF-layout_16h30/", today, "/declaration/")
#date_path <- paste0("./data/seuils/", today, "")
if (!isTRUE(file.info(date_path)$isdir))
    dir.create(date_path, recursive = TRUE)

for (i in seq_along(filesNames)) {
    system(paste0("pdftotext -layout ", "\"", filesPath[i], "\" \"", textNames[i], "\""))
}


##

# ==================================================
# OPA

system_date <- Sys.Date()
today <- format(system_date, "%Y-%m-%d")
# today <- "2016-09-27" #to remove
###declaration des dirigeants TYPE PARAGRAPHS
filesPath <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/OPA/"),
    pattern = "*.pdf$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

#TO DO IF condition filesPath not empty

filesNames <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/OPA/"),
    pattern = "*.pdf$",
    full.names = FALSE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

##converting PDF to text
textNames <-
    str_replace(string = filesNames,
                pattern = "pdf$",
                replacement = "txt")
filesPath <-
    gsub("^..", "", filesPath) %>% gsub("/", "\\\\", .) #removes dot and forward slash first character
#generates destination paths
textNames %<>% paste0("text_mining\\AMF-layout_16h30\\", today, "\\OPA\\", .)

#creates the directory
date_path <- paste0("text_mining/AMF-layout_16h30/", today, "/OPA/")
#date_path <- paste0("./data/seuils/", today, "")
if (!isTRUE(file.info(date_path)$isdir))
    dir.create(date_path, recursive = TRUE)

for (i in seq_along(filesNames)) {
    system(paste0("pdftotext -layout ", "\"", filesPath[i], "\" \"", textNames[i], "\""))
}