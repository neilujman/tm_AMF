.libPaths("C:/R_library")
library(translateR)
library(xlsx)
library(stringr)
library(stringi)

# foo <- translate(dataset = result_seuil, content.field = "crossing", source.lang = fr, target.lang = en)
# 


drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
drop_get(path = "/AMF_Textes_extraits/database-AMF.xlsx", local_file = "./extracted_texts/database-AMF.xlsx", overwrite = TRUE, verbose = TRUE)


wb.eng <- loadWorkbook("./extracted_texts/database-AMF.xlsx")

sheets.eng <- getSheets(wb.eng)


manage.sheet <- sheets.eng[[1]]#management declaration
reg.sheet <- sheets.eng[[2]]#regulatory threshold
short.sheet <- sheets.eng[[3]]
    
    

#df.fr <- read.xlsx(file = "./extracted_texts/BD-AMF.xlsx", sheetIndex = 1, encoding = "UTF-8")
#df.eng <- df.fr

trad.declarant <- function(str){
    str <- stri_replace_all_fixed(str, c("é", "à"), c("e", "a"), vectorize_all = F)
    str.trad <- str
    reg.declarant <- "(personne)|(dministrateur)|(ADMINISTRATEUR)|(Pr.sident-Directeur\\sg.n.ral)|(Pr.sident\\sDirecteur\\sG.n.ral)|(PDG)|(Pr.sident.Directeur\\sG.n.ral)|(Vice.Pr.sident Ex.cutif)|(.resident du .onseil de .urveillance)|(Dirigeant)|(, Directeur G.n.ral)"
    reg.declarant <- paste0(reg.declarant, "|(.oci.te .nonyme . .onseil d'.dministration)|(Directeur Strategie (et )?Developpement)")
    reg.declarant <- paste0(reg.declarant, "|(DIRECTEUR GENERAL DELEGUE)|(DIRECTEUR GENERAL ADJOINT)")
    reg.declarant <- paste0(reg.declarant, "|(holding animatrice)|(PRESIDENT DIRECTEUR GENERAL)|(Soci.t. anonyme)")
    reg.declarant <- paste0(reg.declarant, "|(.ctionnaire)")
    switch(
        EXPR = stri_count_regex(str, reg.declarant),
        {
            str.trad <- switch(
                EXPR = str_extract(str, reg.declarant),
                "personne" = {str_replace(str, "personne morale li.e .", "legal entity related to")
                    },
                "dministrateur" = {
                    str_replace(str, "(a|A)dministrateur", "Administrator")
                },
                "ADMINISTRATEUR" = str_replace(str, "ADMINISTRATEUR", "Administrator"),
                "President-Directeur general" = str_replace(str, "Pr.sident.Directeur g.n.ral", "Chief Executive Officer"),
                "President-Directeur General" = {str_replace(str, pattern ="Pr.sident.Directeur G.n.ral", replacement = "Chief Executive Officer")},
                "President Directeur General" = str_replace(str, "Pr.sident Directeur G.n.ral", "Chief Executive Officer"),
                "PDG" = str_replace(str, "PDG", "Chief Executif Officer"),
                "Vice-President Executif" = str_replace(str, "Vice-Pr.sident Ex.cutif", "Executive VP"),
                "President du Conseil de Surveillance" = str_replace(str, "President du Conseil de Surveillance", "Chairman of the Supervisory Board"),
                "President du conseil de surveillance" = str_replace(str, "President du conseil de surveillance", "Chairman of the Supervisory Board"),
                ", Directeur General" = str_replace(str, "Directeur General", "Managing Director"),
                "Directeur Strategie Developpement" = str_replace(str, "Directeur Strategie Developpement", "VP for Strategy and Development"),
                "Directeur Strategie et Developpement" = str_replace(st, "Directeur Strategie et Developpement", "VP for Strategy and Development"),
                "DIRECTEUR GENERAL DELEGUE" = str_replace(str, "DIRECTEUR GENERAL DELEGUE", "Deputy General Manager"),
                "DIRECTEUR GENERAL ADJOINT" = str_replace(str, "DIRECTEUR GENERAL ADJOINT", "Deputy Director-General"),
                "holding animatrice" = str_replace(str, "holding animatrice", "holding body")
            )
        },
        {
            str.trad <- switch(
                EXPR = str_extract(str, reg.declarant),
                "personne" = {
                    stri_replace_all_regex(
                        str,
                        c(".ersonne morale li.e .", ".ersonne li.e .","Vice.Pr.sident Ex.cutif", "Dirigeant", ".oci.t. .nonyme . .onseil d'.dministration", "PDG", "(A|a)dministrateur", "PRESIDENT DIRECTEUR GENERAL"), 
                        c("legal entity related to", "legal entity related to","Executive VP", "Director", "Limited company with a board of directors", "Chief Executive Officer", "Administrator", "Chief Executive Officer"), 
                        vectorize_all = F
                    )
                },
                "Societe Anonyme a Conseil d'administration" = {
                    stri_replace_all_regex(str, c("personne morale li.e .","Vice.Pr.sident Ex.cutif", "Dirigeant", ".oci.t. .nonyme . .onseil d'.dministration"), c("legal entity related to", "Executive VP", "Director", "Limited company with a board of directors"), vectorize_all = F)
                }
            )
        },{
            strad <- switch(
                EXPR = str_extract(str, reg.declarant),
                "Société anonyme" = {stri_replace_all_regex(
                    str,
                    c(".ersonne morale li.e .", ".ersonne li.e .","Vice.Pr.sident Ex.cutif", "Dirigeant", ".oci.t. .nonyme . .onseil d'.dministration", "PDG", "(A|a)dministrateur", "PRESIDENT DIRECTEUR GENERAL", ".oci.t. anamyme"), 
                    c("legal entity related to", "legal entity related to","Executive VP", "Director", "Limited company with a board of directors", "Chief Executive Officer", "Administrator", "Chief Executive Officer", "limited company"), 
                    vectorize_all = F
                    
                )
                    
                },
                "Actionnaire" = {
                    stri_replace_all_regex(
                        str,
                        c(".ctionnaire", ".ersonne morale li.e .", "administrateur"),
                        c("Shareholder", "legale entity related to", "administrator"),
                        vectorize_all = F
                    )
                }
            )
        }
    )
    return(str.trad)
}


trad.instru <- function(str){
    str.trad <- str
    switch(EXPR = stri_count_regex(str, "(^Action$)|(Parts de Fonds)|souscription|(Fonds D'Actionnariat)|(Option d'achat)"),
           "1"={
               str.trad <- switch(
                   EXPR = str_extract(str, "(^Action$)|(Parts de Fonds)|souscription|(Fonds D'Actionnariat)|(Option d'achat)"),
                   "Action" = "Stock",
                   "Parts de Fonds" = str_replace(str, "Parts de Fonds Commun de Placement d'Entreprise", "Mutual Fund Share"),
                   "souscription" = str_replace(str, "Options de souscription", "Subscription Option"),
                   "Fonds D'Actionnariat" = str.replace(str, "Fonds D'Actionnariat", "Shareholder funds"),
                   "Option d'achat" = str_replace(str, "Option d'achat", "Call option"),
                   {str} # default case
               )
           },
           "2"={
               str.trad <- switch(
                   EXPR = str_extract(str, "(^Action$)|(Parts de Fonds)|souscription|(Fonds D'Actionnariat)"),
                   "Action" = "Stock",
                   "Parts de Fonds" = str_replace(str, "Parts de Fonds Commun de Placement d'Entreprise", "Mutual Fund Share"),
                   "souscription" = str_replace(str, "Options de souscription", "Subscription Option"),
                   "Fonds D'Actionnariat" = str.replace(str, "Fonds D'Actionnariat", "Shareholder funds"),
                   {str} # default case
               )
               str.trad <- switch(
                   EXPR = str_extract(str.trad, "(Fonds D'Actionnariat)"),
                   "Action" = "Stock",
                   "Parts de Fonds" = str_replace(str.trad, "Parts de Fonds Commun de Placement d'Entreprise", "Mutual Fund Share"),
                   "souscription" = str_replace(str.trad, "Options de souscription", "Subscription Option"),
                   "Fonds D'Actionnariat" = str_replace(str.trad, "Fonds D'Actionnariat", "Shareholder funds"),
                   {str.trad} # default case
               )
           }
    )
    return(str.trad)
}

trad.nature <- function(str){
    str.trad <- switch(
        EXPR = str_extract(str, "Acquisition|Cession|Perception|Souscription|Exercice"),
        "Acquisition" = {str_replace(str,"Acquisition", "Acquisition")},
        "Cession" = str_replace(str, "Cession", "Cession"),
        "Perception" = str_replace(str, "Perception du dividende en actions", "Collecting the dividend in shares"),
        "Souscription" = str_replace(str, "Souscription", "Subscription"),
        "Exercice" = str_replace(str, "Exercice", "Exercise"),
        {str}
    )
    return(str.trad)
}



# traduction de declaration des dirigeants
# ----------------------------------------
if(NROW(extractedTexts_modele2) > 0){
    
    nbCols <- 8
    title.line <- getRows(manage.sheet, rowIndex = 1)
    cells <- getCells(title.line, colIndex = 1:nbCols)
    invisible(sapply(1:(nbCols),
                     function(i, cells, cs){
                         setCellValue(cells[[i]], cs[i])
                     },
                     cells = cells, 
                     cs = c(
                         "Scraping of",
                         "Company name", "Declarant", "Financial Instrument",
                         "Nature of the Operation", "Amount of the Operation (€)", "Operation Date",
                         "Dropbox link")
    ))
    # invisible(lapply(cells[[2:7]], setCellStyle, cellStyle=CellStyle(wb.eng) + 
    #                      Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = TRUE) + 
    #                      Font(wb.eng, isItalic = T, isBold = T, color = "whitesmoke") + 
    #                      Fill(foregroundColor = "#4F81BD")))
    invisible(
        sapply(
            2:7, function(i, cells){
                setCellStyle(cells[[i]], cellStyle=CellStyle(wb.eng) + 
                                 Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = TRUE) + 
                                 Font(wb.eng, isItalic = T, isBold = T, color = "whitesmoke") + 
                                 Fill(foregroundColor = "#4F81BD"))
            }, cells = cells)
    )
    setRowHeight(title.line, multiplier = 2)
    
    invisible(sapply(1:nbCols,function(i, largeurs){
        setColumnWidth(manage.sheet, colIndex = i, colWidth = largeurs[i])
    },largeurs=c(20,35,45,20,20,20,20,65)))
    
    nbLigne <- manage.sheet$getLastRowNum()+1
    nbLigne.today <- dim(result_decla)[1]
    manage.sheet$shiftRows(as.integer(1), as.integer(nbLigne-1), as.integer(nbLigne.today))
    # nbCol.today <- dim(result_decla)[2]
    result_manage <- result_decla
    
    temp <- sapply(result_decla$declarant, trad.declarant)
    names(temp) <- NULL
    result_manage$declarant <- temp
    
    temp <- sapply(result_decla$instru, trad.instru)
    names(temp) <- NULL
    result_manage$instru <- temp
    
    result_manage$dateOp <- stri_replace_all_regex(
        result_decla$dateOp,
        c("janvier","f.vrier","mars","avril","mai","juin","juillet","ao.t","septembre","octobre","novembre","d.cembre"),
        c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
        vectorize = FALSE
    )
    
    
    addDataFrame(result_manage, manage.sheet, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
    # for(i in 1:dim(result_manage)[1]){
    #     manage.ligne <- getRows(manage.sheet, rowIndex = i+1)
    #     decla.ligne <- getRows(decla.sheet, rowIndex = i+1)
    #     for(j in 1:8){
    #         manage.cellule <- getCells(manage.ligne, colIndex = j)[[1]]
    #         decla.cellule <- getCells(decla.ligne, colIndex = j)[[1]]
    #         if(is.CellStyle(getCellStyle(decla.cellule))){
    #             setCellStyle(manage.cellule, getCellStyle(decla.cellule))
    #         }
    #     }
    # }
    cs <- CellStyle(wb.eng)
    invisible(sapply(2:(nbLigne.today+1), function(i){
        cells <- getCells(row = getRows(manage.sheet, rowIndex = i), colIndex = 1:8)
        cs.2 <- if(i%%2==0) {
            cs +  fill.blue + border.left + alignment.left + Font(wb.eng, isBold = T) + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {Font(wb.eng, isBold = T, color = "darkgreen")},
                "Cession" = {Font(wb.eng, isBold = T, color = "#C60800")},
                {}
            )
        }
        else {
            cs + fill.white + border.left + alignment.left + Font(wb.eng, isBold = T) + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {Font(wb.eng, isBold = T, color = "darkgreen")},
                "Cession" = {Font(wb.eng, isBold = T, color = "#C60800")},
                {}
            )
        }
        setCellStyle(cells[[2]], cs.2)
        cs.3 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.left else cs + fill.white + border.middle + alignment.left
        setCellStyle(cells[[3]], cs.3)
        cs.4 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center else cs + fill.white + border.middle + alignment.center
        setCellStyle(cells[[4]], cs.4)
        cs.5 <- if(i%%2==0) {
            cs + fill.blue + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {Font(wb.eng, color = "darkgreen")},
                "Cession" = {Font(wb.eng, color = "#C60800")},
                {}
            )
        }else{
            cs + fill.white + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {Font(wb.eng, color = "darkgreen")},
                "Cession" = {Font(wb.eng, color = "#C60800")},
                {}
            )
        }
        setCellStyle(cells[[5]], cs.5)
        cs.6 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center + dataformat.nombre else cs + fill.white + border.middle + alignment.center + dataformat.nombre
        setCellStyle(cells[[6]], cs.6)
        cs.7 <- if(i%%2==0) cs + fill.blue + border.right + alignment.center + DataFormat("yyyy-mm-dd")  else cs + fill.white + border.right + alignment.center
        setCellStyle(cells[[7]], cs.7)
    }))
}

# ------------------------------------------------------------------

# ------------------------------------------------------------------

trad.threshold <- function(str){
    str.trad <- str
    str.trad <- str_replace(str.trad, "du capital et des droits de vote", " of the capital and the voting rights")
    str.trad <- str_replace(str.trad, "des droits de vote", "of the voting rights")
    str.trad <- str_replace(str.trad, "du capital", "of the capital")
    str.trad <- str_replace(str.trad, "et", "and")
    return(str.trad)
}


trad.crossing <- function(str){
    str.trad <- str
    str.trad <- str_replace(str.trad, "Hausse", "Increase")
    str.trad <- str_replace(str.trad, "Baisse", "Decrease")
}


if(NROW(extractedTexts_franchissement) > 0){
    nbCols <- 7
    title.line <- getRows(reg.sheet, rowIndex = 1)
    cells <- getCells(title.line, colIndex = 1:nbCols)
    
    invisible(lapply(cells, setCellStyle, cellStyle=CellStyle(wb.eng) + 
                         Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = TRUE) + 
                         Font(wb.eng, isItalic = T, isBold = T, color = "whitesmoke") + 
                         Fill(foregroundColor = "#4F81BD")))
    setRowHeight(title.line, multiplier = 2)
    invisible(sapply(1:nbCols,
                     function(i, cells, cs){
                         setCellValue(cells[[i]], cs[i])
                     },
                     cells = cells, 
                     cs = c(
                         "Scraping of",
                         "Company Name", "Declarant", "Crossed Threshold(s)",
                         "Type of Crossing", "Date of Crossing", "Dropbox link")
    ))
    invisible(sapply(1:nbCols,function(i, largeurs){
        setColumnWidth(reg.sheet, colIndex = i, colWidth = largeurs[i])
    },largeurs=c(15,35,45,35,20,20,64)))
    
    
    nbLigne <- reg.sheet$getLastRowNum()+1
    nbLigne.today <- dim(result_seuil)[1]
    reg.sheet$shiftRows(as.integer(1), as.integer(nbLigne-1), as.integer(nbLigne.today))
    
    
    result_reg <- result_seuil
    
    result_reg$threshold_crossed <- sapply(result_reg$threshold_crossed, trad.threshold)
    result_reg$crossing <- sapply(result_reg$crossing, trad.crossing)
    result_reg$crossing_date <- stri_replace_all_regex(
        result_seuil$crossing_date,
        c("janvier","f.vrier","mars","avril","mai","juin","juillet","ao.t","septembre","octobre","novembre","d.cembre"),
        c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
        vectorize = FALSE
    )
    
    addDataFrame(result_reg, reg.sheet, col.names = FALSE, row.names = FALSE, startRow = 2)
    
    invisible(sapply(2:(nbLigne.today+1), function(i){
        cells <- getCells(row = getRows(reg.sheet, rowIndex = i), colIndex = 1:7)
        cs.2 <- if(i%%2==0) 
            cs +  fill.blue + border.left + alignment.left + Font(wb.eng, isBold=T, color = switch(
                EXPR = getCellValue(cells[[5]]),
                "Increase" = {"darkgreen"},
                "Decrease" = {"#C60800"},
                {"#000000"})) 
        else 
            cs + fill.white + border.left + alignment.left + Font(wb.eng, isBold=T, color = switch(
                EXPR = getCellValue(cells[[5]]),
                "Increase" = {"darkgreen"},
                "Decrease" = {"#C60800"},
                {"#000000"}))
        setCellStyle(cells[[2]], cs.2)
        cs.3 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.left else cs + fill.white + border.middle + alignment.left
        setCellStyle(cells[[3]], cs.3)
        cs.4 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center else cs + fill.white + border.middle + alignment.center
        setCellStyle(cells[[4]], cs.4)
        cs.5 <- if(i%%2==0) {
            cs + fill.blue + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Increase" = {Font(wb.eng, color = "darkgreen")},
                "Decrease" = {Font(wb.eng, color = "#C60800")},
                {}
            )
        }else{
            cs + fill.white + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Increase" = {Font(wb.eng, color = "darkgreen")},
                "Decrease" = {Font(wb.eng, color = "#C60800")},
                {}
            )
        }
        setCellStyle(cells[[5]], cs.5)
        cs.6 <- if(i%%2==0) cs + fill.blue + border.right + alignment.center else cs + fill.white + border.right + alignment.center
        setCellStyle(cells[[6]], cs.6)
    }))
    
    #df <- read.xlsx("extracted_texts/BD-AMF.xlsx", sheetIndex=2, startRow = 2)
    
    
    #result_reg <- result_seuil
    
}

# --------------------------------------------------
# publi == short position
# --------------------------------------------------

trad.type <- function(str){
    str.trad <- str
    str.trad <- str_replace(str.trad, "COURTE", "Short")
    str.trad <- str_replace(str.trad, "LONGUE", "Long")
}


if(NROW(extractedTexts_position) > 0){
    nbCols <- 7
    title.line <- getRows(short.sheet, rowIndex = 1)
    cells <- getCells(title.line, colIndex = 1:nbCols)
    
    # invisible(lapply(cells, setCellStyle, cellStyle=CellStyle(wb.eng) + 
    #                      Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = TRUE) + 
    #                      Font(wb.eng, isItalic = T, isBold = T, color = "whitesmoke") + 
    #                      Fill(foregroundColor = "#4F81BD")))
    invisible(
        sapply(
            2:6, function(i, cells){
                setCellStyle(cells[[i]], cellStyle=CellStyle(wb.eng) + 
                                 Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = TRUE) + 
                                 Font(wb.eng, isItalic = T, isBold = T, color = "whitesmoke") + 
                                 Fill(foregroundColor = "#4F81BD"))
            }, cells = cells)
    )
    setRowHeight(title.line, multiplier = 2)
    invisible(sapply(1:nbCols,
                     function(i, cells, cs){
                         setCellValue(cells[[i]], cs[i])
                     },
                     cells = cells, 
                     cs = c(
                         "Scraping of",
                         "Name of the Issuer",
                         "Holder of the Position",
                         "% of the Position",
                         "Position Date",
                         "Position Type",
                         "Dropbox link")
    ))
    invisible(sapply(1:nbCols,function(i, largeurs){
        setColumnWidth(short.sheet, colIndex = i, colWidth = largeurs[i])
    },largeurs=c(15,40,40,20,20,20,64)))
    
    
    nbLigne <- short.sheet$getLastRowNum()+1
    nbLigne.today <- dim(result_publi)[1]
    short.sheet$shiftRows(as.integer(1), as.integer(nbLigne-1), as.integer(nbLigne.today))
    
    
    result_short <- result_publi
    
    result_short$typePosition <- sapply(result_short$typePosition, trad.type)
    result_short$datePosition <- sapply(
        as.character(result_publi$datePosition), function(str){
            jour <- str_sub(str, 9,10)
            annee <- str_sub(str, 1, 4)
            mois <- stri_replace_all_regex(
                str_sub(str, 6, 7),
                c("01","02","03","04","05","06","07","08","09","10","11","12"),
                c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                vectorize = FALSE
            )
            str_sub(sprintf("%s %s %s", jour, mois, annee))
        }
    )
    
    addDataFrame(result_short, short.sheet, col.names = FALSE, row.names = FALSE, startRow = 2)
    
    
    
    invisible(sapply(2:(nbLigne), function(i){
        cells <- getCells(row = getRows(short.sheet, rowIndex = i), colIndex = 1:7)
        cs.2 <- if(i%%2==0) 
            cs +  fill.blue + border.left + alignment.left + Font(wb.eng, isBold = T) 
        else 
            cs + fill.white + border.left + alignment.left + Font(wb.eng, isBold = T)
        setCellStyle(cells[[2]], cs.2)
        cs.3 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.left else cs + fill.white + border.middle + alignment.left
        setCellStyle(cells[[3]], cs.3)
        cs.4 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center else cs + fill.white + border.middle + alignment.center
        setCellStyle(cells[[4]], cs.4)
        cs.5 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center else cs + fill.white + border.middle + alignment.center
        setCellStyle(cells[[5]], cs.5)
        cs.6 <- if(i%%2==0) cs + fill.blue + border.right + alignment.left else cs + fill.white + border.right + alignment.left
        setCellStyle(cells[[6]], cs.6)
    }))
}



saveWorkbook(wb.eng, "./extracted_texts/database-AMF.xlsx")


##upload to Dropbox
#authentificate
drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
# EuronextFilesPath <- list.files(path = ".", pattern ="\\w+\\.html$", full.names = TRUE, recursive = TRUE, all.files = FALSE, ignore.case = TRUE) 
# invisible(sapply(EuronextFilesPath, drop_upload, dest = paste0("/mail_euronext"), overwrite = FALSE))
drop_upload(file = "./extracted_texts/database-AMF.xlsx", dest= "AMF_textes_extraits/database-AMF_test.xlsx")



