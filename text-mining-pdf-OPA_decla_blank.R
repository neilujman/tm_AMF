# sourcer "text-mining-pdf-OPA_decla_star
# et récuperer ind.declastar.sub


.libPaths("C:/R_library")
###BEGINNING OF PROPER CODE
###
library(tm)
library(stringr)
library(magrittr)
library(xlsx)
library(dplyr)

system_date <- Sys.Date()
today <- format(system_date, "%Y-%m-%d")
# today <- "2016-09-27" #to remove
###OPA TYPE PARAGRAPHS
filesPath <- list.files(
    path = paste0("./data/AMF/", today, "/OPA/"),
    pattern = "*.pdf$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

filesNames <- list.files(
    path = paste0("./data/AMF/", today, "/OPA/"),
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
textNames %<>% paste0("text_mining\\AMF\\", today, "\\OPA\\", .)


# 
# #creates the directory
# date_path <- paste0("text_mining/AMF/", today, "/OPA/")
# #date_path <- paste0("./data/seuils/", today, "")
# if (!isTRUE(file.info(date_path)$isdir))
# 	dir.create(date_path, recursive = TRUE)
# 
# for (i in seq_along(filesNames)) {
# 	system(paste0("pdftotext ", "\"", filesPath[i], "\" \"", textNames[i], "\""))
# }
# ##


textsPath <- list.files(
    path = paste0("./text_mining/AMF/", today, "/OPA/"),
    pattern = "*.txt$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)



extractedTexts <- lapply(textsPath, readLines, encoding = "UTF-8")

# alimenter en modèle au fur et à mesure
# ne pas confondre avec les déclarations
extractedTexts_declaration <-
    extractedTexts[grep("Déclaration des achats et des ventes", extractedTexts)]
extractedTexts_communication <-
    extractedTexts[grep("Communication sur la société", extractedTexts)]

# extraction des indices des textes qui ne comporte qu'une seule ligne d'achat
ind.decla <- grep("Déclaration des achats et des ventes", extractedTexts) 
for(i in seq_along(extractedTexts_declaration)){
    if(any(str_detect(extractedTexts_declaration[[i]], "^\\* ."))){
        ind.declastar <- append(ind.declastar,i)
    }
}
ind.decla.monoachat <- c()
for(i in ind.decla){
    s=0
    extrait <- extractedTexts_declaration[[i]]
    for(ligne in extrait){
        if(str_detect(ligne, " le \\d{2}/\\d{2}/\\d{4}$")){
            s <- s + 1
        }
    }
    if(s == 1){
        ind.decla.monoachat <- c(ind.decla.monoachat, i)
    }
}
ind.decla.monoachat.blank <- setdiff(ind.decla.monoachat, ind.declastar.sub)


extractedTexts_declaration_blank <- extractedTexts_declaration[ind.decla.monoachat.blank]

filesNames_declaration <-
    filesNames[grep("Déclaration des achats et des ventes", extractedTexts)]
dropbox_links_declaration <-
    paste0(
        "https://www.dropbox.com/home/AMF/",
        today,
        "/OPA?preview=",
        filesNames_declaration
    ) %>% gsub(" ", "+", .)


filesNames_declaration_blank <-
    filesNames[ind.decla.monoachat.blank]
dropbox_links_declaration_blank <-
    paste0(
        "https://www.dropbox.com/home/AMF/",
        today,
        "/OPA?preview=",
        filesNames_declaration_blank
    ) %>% gsub(" ", "+", .)


## =========================================
## TEXT MINING ON extractedTexts_declaration
## =========================================

is.sentence <- function(str){
    res <- str_detect(str, "^$") 
    return(!res)
}
mkSentence <- function(i){
    cpt = 1
    sentence <- c()
    while(is.sentence(extrait.sub[i+cpt])){
        sentence <- append(sentence, extrait.sub[i+cpt])
        cpt <- cpt +1
    }
    return(sentence)
}
treatSentence <- function(sentence){
    finalSentence <- ""
    for(semiSentence in sentence){
        finalSentence <- str_c(finalSentence, semiSentence, " ")
    }
    return(finalSentence)
}


societe <- c()
operateur <- c()
nature <- c()
dateOp <- c()
titreConcerne <- c()
cours <- c()
nbTotal <- c()


if(NROW(extractedTexts_declaration_blank) > 0) {
    for (i in seq_along(extractedTexts_declaration_blank)) {
        extrait <- extractedTexts_declaration_blank[[i]]
        mark.op <- grep("^Opérateur$", extrait)
        mark.blank <- grep("^____+\\s?$", extrait)
        extrait.sub <- extrait[(mark.op-1):(mark.blank-1)]
        
        # recup des phrases d'intérêts
        ind <- grep("^$", extrait.sub)
        ind <- ind[-length(ind)]
        phrases <- c()
        for(i in ind){
            s0 <- mkSentence(i)
            s1 <- treatSentence(s0)
            phrases <- append(phrases, s1)
        }
        
        # extraction de l'information depuis les phrases
        # ----------------------------------------------
        ## récupération de operateur
        mark <- grep("^Opérateur", phrases)
        i <- mark + 1
        while(i<=10){
            if(str_detect(phrases[i], "^Nature et date") || str_detect(phrases[i],"^Titres concernés") || str_detect(phrases[i], "^Cours (.)") || str_detect(phrases[i], "^Nombre total de titres")){
                i <- i+ 1
            } else{
                temp.op <- phrases[i]
                break
            }
        }
        phrases <- phrases[-c(mark,i)]
        
        ## récup de nature et date
        mark <- grep("^Nature et date", phrases)
        i <- mark + 1
        while(i<=10){
            if(str_detect(phrases[i],"^Titres concernés") || str_detect(phrases[i], "^Cours (.)") || str_detect(phrases[i], "^Nombre total de titres")){
                i <- i+ 1
            } else{
                temp.nat <- phrases[i]
                break
            }
        }
        phrases <- phrases[-c(mark,i)]
        
        # récup de titre
        mark <- grep("^Titres concernés", phrases)
        i <- mark + 1
        while(i<=10){
            if(str_detect(phrases[i], "^Cours (.)") || str_detect(phrases[i], "^Nombre total de titres")){
                i <- i+ 1
            } else{
                temp.titre <- phrases[i]
                break
            }
        }
        phrases <- phrases[-c(mark,i)]
        
        # récup du cours
        mark <- grep("^Cours (.)", phrases)
        i <- mark + 1
        while(i<=10){
            if(str_detect(phrases[i], "^Nombre total de titres")){
                i <- i+ 1
            } else{
                temp.cours <- phrases[i]
                break
            }
        }
        phrases <- phrases[-c(mark,i)]
        
        # récup du nombre total
        temp.nb <- phrases[2]
        
        # stockage de l'information
        # -------------------------
        operateur <- append(operateur, temp.op)
        nature <- append(nature, temp.nat)
        dateOp <- append(dateOp, temp.nat)
        titreConcerne <- append(titreConcerne, temp.titre)
        cours <- append(cours, temp.cours)
        nbTotal <- append(nbTotal, temp.nb)
        
        # --- societe
        mark.soc <- grep("^\\(article \\d", extrait)
        temp.soc <- extrait[mark.soc + 1]
        societe <- append(societe, temp.soc)
    }
    
    # Post-traitement
    nature %<>% sapply(., str_extract, pattern= "^.+ le") 
    nature %<>% sapply(., str_replace, pattern = " le$", replacement = "")
    names(nature) <- NULL
    dateOp %<>% sapply(., str_extract, pattern= "le .+$") 
    dateOp %<>% sapply(., str_replace, pattern = "^le ", replacement = "")
    dateOp %<>% sapply(., str_replace, pattern = " $", replacement = "")
    names(dateOp) <- NULL
    # enlèvement des étoiles
    operateur %<>% sapply(., str_replace_all, pattern = "(\\*\\s*$)|(\\*{2}\\s*$)|(\\*/\\*{2}\\s*$)", replacement="")
    nature %<>% sapply(., str_replace_all, pattern = "(\\*\\s*$)|(\\*{2}\\s*$)|(\\*/\\*{2}\\s*$)", replacement="")
    dateOp %<>% sapply(., str_replace_all, pattern = "(\\*\\s*$)|(\\*{2}\\s*$)|(\\*/\\*{2}\\s*$)", replacement="")
    titreConcerne %<>% sapply(., str_replace_all, pattern = "(\\*\\s*$)|(\\*{2}\\s*$)|(\\*/\\*{2}\\s*$)", replacement="")
    cours %<>% sapply(., str_replace_all, pattern = "(\\*\\s*$)|(\\*{2}\\s*$)|(\\*/\\*{2}\\s*$)", replacement="")
    nbTotal %<>% sapply(., str_replace_all, pattern = "(\\*\\s*$)|(\\*{2}\\s*$)|(\\*/\\*{2}\\s*$)", replacement="")
    # enlèvement de code
    titreConcerne %<>% sapply(., str_replace, pattern = " code.+$", replacement ="")
    
    
    today <- format(system_date, "%Y-%m-%d")
    today %<>% rep(., NROW(extractedTexts_declaration_blank))
    
    result_declaration_blank <- 
        cbind(
            today,
            societe,
            operateur,
            nature,
            dateOp,
            titreConcerne,
            cours,
            nbTotal,
            dropbox_links_declaration_blank
        )
    colnames(result_declaration_star) <- NULL
    
    AMF_workbook <- createWorkbook()
    AMF_declaration_blank <-
        createSheet(wb = AMF_workbook, sheetName = "Offres publiques d'acquisition")
    
    
    if(file.exists("./extracted_texts/OPA_temp_blank.xlsx")){
        file.remove("./extracted_texts/OPA_temp_blank.xlsx")
    }
    
    if(!file.exists("./extracted_texts/OPA_temp_blank.xlsx")){ # condition normalement assuré par le if précédent
        row <- createRow(AMF_declaration_blank, 1)
        cells <- createCell(row, 1:9)
        
        column_titles <-
            c(
                "Scraping du",
                "Société",
                "Opérateur",
                "Nature",
                "Date de l'opération",
                "Titres concernés",
                "Cours(€)",
                "Nombres totales de titres possédés à l'issu de la transaction",
                "Lien dropbox"
            ) %>% t
        addDataFrame(
            x = column_titles,
            sheet = AMF_declaration_blank,
            row.names = FALSE,
            col.names = FALSE
        )
        saveWorkbook(AMF_workbook,
                     "./extracted_texts/OPA_temp_blank.xlsx")
    }
    existing_data <-
        read.xlsx(
            "./extracted_texts/OPA_temp_blank.xlsx",
            sheetName = "Offres publiques d'acquisition",
            header = FALSE,
            encoding = "UTF-8"
        ) %>% as.matrix
    colnames(existing_data) <- NULL
    
    #newest data on the top of the worksheet
    new_data <- rbind(existing_data, result_declaration_blank)
    temp <- new_data [1, ]
    temp_2 <-
        new_data[2:NROW(new_data), ] %>% .[order(.[, 1], decreasing = TRUE), ]
    new_data <- rbind(temp, temp_2)
    
    addDataFrame(
        x = new_data,
        sheet = AMF_declaration_blank,
        row.names = FALSE,
        col.names = FALSE
    )
    
    setColumnWidth(AMF_declaration_blank, colIndex = 1, colWidth = 15)
    setColumnWidth(AMF_declaration_blank, colIndex = 2, colWidth = 22)
    setColumnWidth(AMF_declaration_blank, colIndex = 3, colWidth = 22)
    setColumnWidth(AMF_declaration_blank, colIndex = 4, colWidth = 22)
    setColumnWidth(AMF_declaration_blank, colIndex = 5, colWidth = 22)
    setColumnWidth(AMF_declaration_blank, colIndex = 6, colWidth = 22)
    setColumnWidth(AMF_declaration_blank, colIndex = 7, colWidth = 22)
    setColumnWidth(AMF_declaration_blank, colIndex = 8, colWidth = 53)
    setColumnWidth(AMF_declaration_blank, colIndex = 9, colWidth = 65)
    
    cs.title <- CellStyle(AMF_workbook) +
        Font(AMF_workbook, isBold = TRUE, isItalic = TRUE) +
        Fill(backgroundColor = "grey") +
        Alignment(h="ALIGN_CENTER")
    ligne.titre <- getRows(AMF_declaration_blank, rowIndex = 1)
    cells <- getCells(ligne.titre, colIndex = 1:9)
    lapply(cells, setCellStyle, cellStyle  =cs.title)
    
    
    saveWorkbook(AMF_workbook,
                 "./extracted_texts/OPA_temp_blank.xlsx")
}




#write.xlsx(result_declaration_star, "2016-10-10_OPA_star.xlsx")



#########################################