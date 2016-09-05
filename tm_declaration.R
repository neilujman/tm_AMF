.libPaths(new = "C:/R_library")

library(rdrop2)
library(stringr)

drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)



maDate <-"2016-08-08"
chemin <-paste0("AMF/",maDate,"/declaration")
mesFic <- drop_dir(chemin)$path


# AVERTISSEMENT
print("????????????????????????????????????????????????????????????")
print("???????? AVEZ-VOUS VIDER LE REPERTOIRE texts_tmp ????????")
print("????????????????????????????????????????????????????????????")


# les fichiers de la dropbox vont dans TextesDuJour
setwd("texts_tmp")
for(fic in mesFic)
    drop_get(path = fic, overwrite = TRUE)
setwd("..")


setwd("texts_tmp")
mesFic <- dir(pattern = "pdf$")
for(fic in mesFic){
    rep.html <- str_replace(string = fic, pattern = "pdf$", replacement = "")
    commande <- paste0("pdftohtml ", "\"", fic, "\" \"", rep.html, "\"")
    system(commande)
}
setwd("..")



setwd("texts_tmp")
mesFic <- dir(pattern = "pdf$")
for(fic in mesFic){
    rep.html <- str_replace(string = fic, pattern = "pdf$", replacement = "txt")
    commande <- paste0("pdftotext ", "\"", fic, "\" \"", rep.html, "\"")
    system(commande)
}
setwd("..")


bd <- read.xlsx("BD-AMF.xlsx", sheetIndex = 1)
bd.nbLignes <- dim(bd)[1] +1 # on ajoute la ligne des titre


library(stringr)
myPaths <- character()
for(aFile in dir("texts_tmp", pattern="txt$")){
    myPaths <- append(myPaths,file.path("texts_tmp",aFile))
}

myTexts <- character()
for(aFile in myPaths){
    aText <- readLines(aFile)
    aText <- str_c(aText, collapse = "\n")
    myTexts <- append(myTexts, aText)
}

nbTexts <- length(myTexts)

# in order to put the differents components
societe=rep(NA, nbTexts)
declarant = rep(NA, nbTexts)
poste = rep(NA, nbTexts)
instruFin = rep(NA, nbTexts)
natOp = rep(NA, nbTexts) # nature de l'operation
dateOp = rep(NA, nbTexts)
montantOp = rep(NA, nbTexts)

rechercheSociete <- function(txt){
    # txt: a text where we want to find the name of the society
    nom <- str_extract(txt, "D.CLARANT.\n\n?.+?\n\n?")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(D.CLARANT.)|(\n)", "")
        return(nom)
    }else{
        return(NA)
    }
    
}
rechercheDeclarant <- function(txt){
    nom <- str_extract(txt, "D.CLARANT : .+? INSTRUMENT FINANCIER")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(D.CLARANT : )|( INSTRUMENT FINANCIER)", "")
        nom <- str_replace_all(nom, ",.+?$", "")
        return(nom)
    }else{
        nom <- str_extract(txt, "D.CLARANT : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(D.CLARANT : )", "")
            nom <- str_replace_all(nom, ",.+?\n", "")
        }else
            return(NA)
    }
}
recherchePoste <- function(txt){
    nom <- str_extract(txt, "D.CLARANT : .+? INSTRUMENT FINANCIER")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(D.CLARANT : )|( INSTRUMENT FINANCIER)", "")# all is written on one line
        nom <- str_replace_all(nom, "^.+?,", "")
        return(nom)
    }else{
        nom <- str_extract(txt, "D.CLARANT : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(D.CLARANT : )|(\n)", "")
            nom <- str_replace_all(nom, "^.+?,", "")
        }else
            return(NA)
    }
}
rechercheInstruFin <- function(txt){
    nom <- str_extract(txt, "INSTRUMENT FINANCIER : .+? NATURE")# all is written on one line
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(INSTRUMENT FINANCIER : )|( NATURE)", "")
        nom <- str_replace_all(nom, "^.+?,", "")
        return(nom)
    }else{
        nom <- str_extract(txt, "INSTRUMENT FINANCIER : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(INSTRUMENT FINANCIER : )|(\n)", "")
            nom <- str_replace_all(nom, "^.+?,", "")
        }else
            return(NA)
    }
}
rechercheNatOp <- function(txt){
    nom <- str_extract(txt, "NATURE DE L'OP.RATION : .+? DATE")# all is written on one line
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(NATURE DE L'OP.RATION : )|( DATE)", "")
        #nom <- str_replace_all(nom, "^.+?,", "")
        return(nom)
    }else{
        nom <- str_extract(txt, "NATURE DE L'OP.RATION : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(NATURE DE L'OP.RATION : )|(\n)", "")
            #nom <- str_replace_all(nom, "^.+?,", "")
        }else
            return(NA)
    }
}
rechercheDateOp <- function(txt){
    nom <- str_extract(txt, "DATE DE L'OP.RATION : .+? DATE")# all is written on one line
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(DATE DE L'OP.RATION : )|( DATE)", "")
        #nom <- str_replace_all(nom, "^.+?,", "")
        return(nom)
    }else{
        nom <- str_extract(txt, "DATE DE L'OP.RATION : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(DATE DE L'OP.RATION : )|(\n)", "")
            #nom <- str_replace_all(nom, "^.+?,", "")
        }else
            return(NA)
    }
}
rechercheMontantOp <- function(txt){
    # txt: a text where we want to find the name of the society
    nom <- str_extract(txt, "MONTANT DE L'OP.RATION : .+?Euros?")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(MONTANT DE L'OP.RATION : )|(Euros?)", "")
        return(nom)
    }else{
        return(NA)
    }
    
}

AMF.wb <- loadWorkbook("BD-AMF.xlsx")


# retrieve of "societe", "declarant", "poste", "instrument financier",
#   "nature de l'operation", "date de l'operation", "montant de l'operation".
for(i in 1:nbTexts){
    maFeuille <- getSheets(AMF.wb)[[1]]
    maLigne <- createRow(maFeuille, rowIndex = bd.nbLignes + i)
    aText <- myTexts[i]
    
    maCellule <- createCell(maLigne, colIndex = 1)
    setCellValue(maCellule[[1]],rechercheDateOp(aText))
    maCellule <- createCell(maLigne, colIndex = 2)
    setCellValue(maCellule[[1]],rechercheSociete(aText))
    maCellule <- createCell(maLigne, colIndex = 3)
    setCellValue(maCellule[[1]],rechercheDeclarant(aText))
    maCellule <- createCell(maLigne, colIndex = 4)
    setCellValue(maCellule[[1]],recherchePoste(aText))
    maCellule <- createCell(maLigne, colIndex = 5)
    setCellValue(maCellule[[1]],rechercheInstruFin(aText))
    maCellule <- createCell(maLigne, colIndex = 6)
    setCellValue(maCellule[[1]],rechercheNatOp(aText))
    maCellule <- createCell(maLigne, colIndex = 7)
    setCellValue(maCellule[[1]],rechercheMontantOp(aText))
    maCellule8<- createCell(maLigne, colIndex = 8)
    setCellValue(maCellule8[[1]],drop_dir(chemin)$path[i])
    
    monFic <- drop_dir(chemin)$path[i]
    monFic1 <-str_replace_all(string = monFic, pattern = " ", replacement = "+")
    nomFic1 = str_replace(monFic1,pattern = paste0("/AMF/",maDate,"/declaration/"),"")
    monAdresse = paste0("https://www.dropbox.com/home/AMF/",maDate,"/declaration?preview=",nomFic1)
    addHyperlink(cell = maCellule8[[1]], address = monAdresse)
}


saveWorkbook(AMF.wb, "BD-AMF.xlsx")




