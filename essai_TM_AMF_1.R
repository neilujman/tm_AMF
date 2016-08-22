# ce script ne tient pas compte du nombre d'opérations par fichiers


library(stringr)
library(xlsx)

# we try on a parser for the amf declaration files


# we automized the creation of temporary directory for new
# declaration files
dir.create("texts_tmp")




# mesPdfs <- dir("texts")
# monRep <- "texts_declaration_pdf"
# for(unPdf in mesPdfs){
#     system(paste0("\"", exe, "\" \"", monRep, "\"", unPdf, "\""), wait = F)
# }


# =========================================================================
# a code cell

# we create a vector which contains each files
# a file will will be only in one row
# we use \n separator in a a row
library(stringr)
myPaths <- character()
for(aFile in dir("texts")){
    myPaths <- append(myPaths,file.path("texts",aFile))
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
# retrieve of "societe", "declarant", "poste", "instrument financier",
#   "nature de l'operation", "date de l'operation", "montant de l'operation".
for(i in 1:nbTexts){
    aText <- myTexts[i]
    societe[i] <- rechercheSociete(aText)
    declarant[i] <- rechercheDeclarant(aText)
    poste[i] <- recherchePoste(aText)
    instruFin[i] <- rechercheInstruFin(aText)
    natOp[i] <- rechercheNatOp(aText)
    dateOp[i] <- rechercheDateOp(aText)
    montantOp[i] <- rechercheMontantOp(aText)
}
    


# ======================================================================


# ======================================================================
declaration.df <- data.frame(
    "Societe" = societe,
    "Declarant" = declarant,
    "Poste" = poste,
    "Instrument financier" = instruFin,
    "Nature de l'operation" = natOp,
    "Date de l'operation" = dateOp,
    "Montant de l'operation" = montantOp
)

library(xlsx)

# write.xlsx(declaration.df, "BD-AMF.xlsx")

AMF.workBook <- createWorkbook()
AMF.sheet1 <- createSheet(wb=AMF.workBook, sheetName="Déclarations des dirigeants")
addDataFrame(x=declaration.df, sheet=AMF.sheet1, row.names = FALSE, col.names =TRUE)
# Changer la largeur des colonnes
setColumnWidth(AMF.sheet1, colIndex=1, colWidth=30)
setColumnWidth(AMF.sheet1, colIndex=2, colWidth=60)
setColumnWidth(AMF.sheet1, colIndex=3, colWidth=70)
setColumnWidth(AMF.sheet1, colIndex=4, colWidth=35)
setColumnWidth(AMF.sheet1, colIndex=5, colWidth=23)
setColumnWidth(AMF.sheet1, colIndex=6, colWidth=25)
setColumnWidth(AMF.sheet1, colIndex=7, colWidth=24)
# pour formatter la colonne 6 "Date de l'operation"
col6.dataFormat <- DataFormat(("[$-40C]jjjj j mmmm aaaa"))
col6.cellStyle <- CellStyle(AMF.workBook, dataFormat = col6.dataFormat)
col6_rowIndex <- which(!is.na(declaration.df[,6]))
col6_rowIndex[-1] # we put off the title's row of the column 6
col6.rows <- getRows(sheet = AMF.sheet1, rowIndex = col6_rowIndex)
col6.cells <- getCells(row = col6.rows, colIndex = 6)
for(i in col7_rowIndex){
  setCellStyle(cell = col7.cells[[i]], cellStyle = col7.cellStyle)
}
# pour formatter la colonne 7 "montant de l'opération" en nombres
col7.dataFormat <- DataFormat(("# ##0,00"))
col7.cellStyle <- CellStyle(AMF.workBook, dataFormat = col7.dataFormat)
col7_rowIndex <- which(!is.na(declaration.df[,7]))
col7_rowIndex[-1] # we put off the title's row of the column 7
col7.rows <- getRows(sheet = AMF.sheet1, rowIndex = col7_rowIndex)
col7.cells <- getCells(row = col7.rows, colIndex = 7)
for(i in col7_rowIndex){
  setCellStyle(cell = col7.cells[[i]], cellStyle = col7.cellStyle)
}

# Styles pour les noms de colonnes
TABLE_COLNAMES_STYLE <- CellStyle(AMF.workBook) + Font(AMF.workBook, isBold=TRUE) #+
#     Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
#     Border(color="red", position=c("TOP", "BOTTOM"), 
#            pen=c("BORDER_THIN", "BORDER_THICK")) 
colnames.row <- getRows(AMF.sheet1, rowIndex = 1)
colnames.cell <- getCells(colnames.row)
setCellStyle(colnames.cell$'1.1', TABLE_COLNAMES_STYLE)
setCellStyle(colnames.cell$'1.2', TABLE_COLNAMES_STYLE)
setCellStyle(colnames.cell$'1.3', TABLE_COLNAMES_STYLE)
setCellStyle(colnames.cell$'1.4', TABLE_COLNAMES_STYLE)
setCellStyle(colnames.cell$'1.5', TABLE_COLNAMES_STYLE)
setCellStyle(colnames.cell$'1.6', TABLE_COLNAMES_STYLE)
setCellStyle(colnames.cell$'1.7', TABLE_COLNAMES_STYLE)



saveWorkbook(AMF.workBook, "BD-AMF.xlsx")




# we take a memory of handled text file



