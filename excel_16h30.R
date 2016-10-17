#==========================================
# nécésssite les df result_* issus de mine_16h30.R


library(xlsx)


# sourcer mine_16h30.R

drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
drop_get(path = "/AMF_Textes_extraits/BD-AMF.xlsx", local_file = "./extracted_texts/BD-AMF.xlsx", overwrite = TRUE, verbose = TRUE)



# excel
wb <- loadWorkbook("./extracted_texts/BD-AMF.xlsx")
sheets <- getSheets(wb)
decla.sheet <- sheets[[1]]  # with df result_decla from mine_16h30.R 
seuil.sheet <- sheets[[2]]  # with df result_seuil from mine_16h30.R
publi.sheet <- sheets[[3]]  # with df result_publi from mine_16h30.R
opa.sheet <- sheets[[4]]    # with df result_opa from mine_16h30.R


# partie declaration des dirigeants
nbLigne <- decla.sheet$getLastRowNum()+1
nbLigne.today <- dim(result_decla)[1]
decla.sheet$shiftRows(as.integer(1),as.integer(nbLigne-1),as.integer(nbLigne.today))
nbCol.today <- dim(result_decla)[2]

# attach(result_decla)

# for(i in 1:nbLigne.today){
#     ligne <- createRow(decla.sheet, rowIndex = i+1)
#     cells <- createCell(ligne, colIndex = 1:nbCol.today)
#     setCellValue(cells[[1]], today)
#     setCellValue(cells[[2]], valeur[i])
#     setCellValue(cells[[3]], declarant[i])
#     setCellValue(cells[[4]], instru[i])
#     setCellValue(cells[[5]], nature[i])
#     setCellValue(cells[[6]], montant[i])
#     setCellValue(cells[[7]], dateOp[i])
#     setCellValue(cells[[8]], dropbox_links_modele2[i])
# }
# detach(result_decla)
addDataFrame(result_decla, decla.sheet, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
cb.titre <- CellBlock(decla.sheet, 1, 1, 1, 8)
ligne.titre <- getCells(getRows(decla.sheet, rowIndex = 1))
invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER"))))
CB.setFill(cb.titre, fill = Fill(foregroundColor = "#22427C", backgroundColor = "green"), 1, 1:8)
CB.setFont(cb.titre, font = Font(wb, color="whitesmoke", isItalic = TRUE, isBold = TRUE), 1, 1:8)
setCellValue(ligne.titre[[1]], "Scraping du")
setCellValue(ligne.titre[[2]], "Valeur")
setCellValue(ligne.titre[[3]], "Déclarant")
setCellValue(ligne.titre[[4]], "Instrument financier")
setCellValue(ligne.titre[[5]], "Nature")
setCellValue(ligne.titre[[6]], "Montant de l'opération")
setCellValue(ligne.titre[[7]], "Date de l'opération")
setCellValue(ligne.titre[[8]], "Lien Dropbox")


setColumnWidth(decla.sheet, colIndex=1, colWidth=14)
setColumnWidth(decla.sheet, colIndex=2, colWidth=32)
setColumnWidth(decla.sheet, colIndex=3, colWidth=40)
setColumnWidth(decla.sheet, colIndex=4, colWidth=22)
setColumnWidth(decla.sheet, colIndex=5, colWidth=16)
setColumnWidth(decla.sheet, colIndex=6, colWidth=22)
setColumnWidth(decla.sheet, colIndex=7, colWidth=20)
setColumnWidth(decla.sheet, colIndex=8, colWidth=25)



border <- Border(color="grey", position = c("BOTTOM", "LEFT", "TOP", "RIGHT"), pen = c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN", "BORDER_THIN"))
cs.bord <- CellStyle(wb, border = border)
for(i in 1:nbLigne.today){
    ligne <- getRows(decla.sheet, rowIndex = i+1)
    cells <- getCells(ligne)
    for(j in 1:nbCol.today){
        setCellStyle(cells[[j]], cs.bord)
    }
    
}


cs.red <- cs.bord +
    Font(wb, color = "red")
cs.green <- cs.bord +
    Font(wb, color = "darkgreen")
cs.nombre <- cs.bord + DataFormat("### ### ##0")
cs.date <- cs.bord + DataFormat("dd/mm/yyyy")


for(i in 1:nbLigne.today){
    ligne <- getRows(decla.sheet, rowIndex = i+1)
    
    cell <- getCells(ligne, colIndex = 5)[[1]]
    if(getCellValue(cell)=="Cession"){
        setCellStyle(cell,cs.red)
    }
    if(getCellValue(cell)=="Acquisition"){
        setCellStyle(cell,cs.green)
    }
    cell <- getCells(ligne, colIndex = 6)[[1]]
    setCellValue(cell, as.numeric(getCellValue(cell)))
    setCellStyle(cell, cs.nombre)
    cell <- getCells(ligne, colIndex = 7)[[1]]
    setCellStyle(cell, cs.date)
}

#decla.sheet$autoSizeColumn(as.integer(3))




# partie declaration des dirigeants
nbLigne <- seuil.sheet$getLastRowNum()+1
nbLigne.today <- dim(result_seuil)[1]
seuil.sheet$shiftRows(as.integer(1),as.integer(nbLigne-1),as.integer(nbLigne.today))
nbCol.today <- dim(result_seuil)[2]
#
# cb <- CellBlock(seuil.sheet, 2, 1, nbLigne.today, nbCol.today, create = TRUE)
# CB.setMatrixData(cb, as.matrix(result_seuil), 1, 1)
addDataFrame(result_seuil, seuil.sheet, startRow = 2, startColumn = 1, col.names = FALSE, row.names = FALSE)

cb.titre <- CellBlock(seuil.sheet, 1, 1, 1, 7)
ligne.titre <- getCells(getRows(seuil.sheet, rowIndex = 1))
invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER"))))
CB.setFill(cb.titre, fill = Fill(foregroundColor = "#22427C", backgroundColor = "green"), 1, 1:7)
CB.setFont(cb.titre, font = Font(wb, color="whitesmoke", isItalic = TRUE, isBold = TRUE), 1, 1:7)

CB.setRowData(cb.titre, c(
    "Scraping du",
    "Valeur",
    "Déclarant",
    "Seuil(s) franchi(s)",
    "Type de franchissement",
    "Date du franchissement",
    "Liens Dropbox"),
    rowIndex = 1
)

for(i in 1:nbLigne.today){
    cells <- getCells(row = getRows(seuil.sheet, rowIndex = i+1))
    if(getCellValue(cells[[5]])=="hausse"){
        setCellValue(cells[[5]],"Hausse")
        setCellStyle(cells[[5]], cellStyle = cs.green)
    }
    if(getCellValue(cells[[5]])=="baisse"){
        setCellValue(cells[[5]],"Baisse")
        setCellStyle(cells[[5]], cellStyle = cs.red)
    }
}

setColumnWidth(seuil.sheet, colIndex = 1, colWidth = 15)
setColumnWidth(seuil.sheet, colIndex = 2, colWidth = 30)
setColumnWidth(seuil.sheet, colIndex = 3, colWidth = 50)
setColumnWidth(seuil.sheet, colIndex = 4, colWidth = 40)
setColumnWidth(seuil.sheet, colIndex = 5, colWidth = 20)
setColumnWidth(seuil.sheet, colIndex = 6, colWidth = 23)
setColumnWidth(seuil.sheet, colIndex = 7, colWidth = 65)


# ===============================
# ===============================
# PUBLICATION DE POSITION
# ===============================
# ===============================
nbLigne <- publi.sheet$getLastRowNum()+1
nbLigne.today <- dim(result_publi)[1]
publi.sheet$shiftRows(as.integer(1),as.integer(nbLigne-1),as.integer(nbLigne.today))
nbCol.today <- dim(result_publi)[2]
#
# cb <- CellBlock(seuil.sheet, 2, 1, nbLigne.today, nbCol.today, create = TRUE)
# CB.setMatrixData(cb, as.matrix(result_seuil), 1, 1)
addDataFrame(result_publi, publi.sheet, startRow = 2, startColumn = 1, col.names = FALSE, row.names = FALSE)


cb.titre <- CellBlock(publi.sheet, 1, 1, 1, 7)
ligne.titre <- getCells(getRows(publi.sheet, rowIndex = 1))
invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER"))))
CB.setFill(cb.titre, fill = Fill(foregroundColor = "#22427C", backgroundColor = "green"), 1, 1:7)
CB.setFont(cb.titre, font = Font(wb, color="whitesmoke", isItalic = TRUE, isBold = TRUE), 1, 1:7)

CB.setRowData(cb.titre, c(
    "Scraping du",
    "Détenteur de la position",
    "Nom de l'émetteur",
    "% de la position",
    "Date de la position",
    "Type de position",
    "Lien Dropbox"),
    rowIndex = 1
)


setColumnWidth(publi.sheet, colIndex = 1, colWidth = 15)
setColumnWidth(publi.sheet, colIndex = 2, colWidth = 40)
setColumnWidth(publi.sheet, colIndex = 3, colWidth = 40)
setColumnWidth(publi.sheet, colIndex = 4, colWidth = 20)
setColumnWidth(publi.sheet, colIndex = 5, colWidth = 20)
setColumnWidth(publi.sheet, colIndex = 6, colWidth = 23)
setColumnWidth(publi.sheet, colIndex = 7, colWidth = 65)



# ====================
# ====================
#   OPA
# ====================
# ====================
nbLigne <- opa.sheet$getLastRowNum()+1
nbLigne.today <- dim(result_monoachat)[1]
opa.sheet$shiftRows(as.integer(1),as.integer(nbLigne-1),as.integer(nbLigne.today))
nbCol.today <- dim(result_monoachat)[2]
addDataFrame(result_monoachat, opa.sheet, startRow = 2, startColumn = 1, col.names = FALSE, row.names = FALSE)

cb.titre <- CellBlock(opa.sheet, 1, 1, 1, 9)
ligne.titre <- getCells(getRows(opa.sheet, rowIndex = 1))
invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER", v = "VERTICAL_CENTER", wrapText = TRUE))))
CB.setFill(cb.titre, fill = Fill(foregroundColor = "#22427C", backgroundColor = "green"), 1, 1:9)
CB.setFont(cb.titre, font = Font(wb, color="whitesmoke", isItalic = TRUE, isBold = TRUE), 1, 1:9)

# # ce titre est super long
# cell.titre.nbTot <- getCells(getRows(opa.sheet, rowIndex = 1), colIndex = 8)
# cs.titre.nbTot <- CellStyle(wb) + Alignment(wrapText = TRUE)
# setCellStyle(cell.titre.nbTot, cs.titre.nbTot)
# 

CB.setRowData(cb.titre, c(
    "Scraping du",
    "Société",
    "Opérateur",
    "Nature",
    "Date de l'opération",
    "Titres concernés",
    "Cours (€)",
    "Nombres totales de titres possédés \nà l'issu de la transaction",
    "Lien dropbox"),
    rowIndex = 1
)

setColumnWidth(opa.sheet, colIndex = 1, colWidth = 15)
setColumnWidth(opa.sheet, colIndex = 2, colWidth = 30)
setColumnWidth(opa.sheet, colIndex = 3, colWidth = 30)
setColumnWidth(opa.sheet, colIndex = 4, colWidth = 15)
setColumnWidth(opa.sheet, colIndex = 5, colWidth = 15)
setColumnWidth(opa.sheet, colIndex = 6, colWidth = 40)
setColumnWidth(opa.sheet, colIndex = 7, colWidth = 15)
setColumnWidth(opa.sheet, colIndex = 8, colWidth = 50)
setColumnWidth(opa.sheet, colIndex = 9, colWidth = 65)







saveWorkbook(wb, "./extracted_texts/BD-AMF.xlsx")

