#==========================================
# nécésssite les df result_* issus de mine_16h30.R


library(xlsx)


# sourcer mine_16h30.R

drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
drop_get(path = "/AMF_Textes_extraits/BD-AMF.xlsx", local_file = "./extracted_texts/BD-AMF.xlsx", overwrite = TRUE, verbose = TRUE)



# excel
wb <- loadWorkbook("./extracted_texts/BD-AMF.xlsx")
sheets <- getSheets(wb)
decla.sheet <- sheets[[1]]  # with df result_decla from mine-layout_16h30.R 
seuil.sheet <- sheets[[2]]  # with df result_seuil from mine-layout_16h30.R
publi.sheet <- sheets[[3]]  # with df result_publi from mine-layout_16h30.R
opa.sheet <- sheets[[4]]    # with df result_opa from mine-layout_16h30.R

if(NROW(extractedTexts_modele2) > 0){
    # partie declaration des dirigeants
    nbLigne <- decla.sheet$getLastRowNum()+1
    nbLigne.today <- dim(result_decla)[1]
    decla.sheet$shiftRows(as.integer(1),as.integer(nbLigne-1),as.integer(nbLigne.today))
    nbCol.today <- dim(result_decla)[2]
    
  
    addDataFrame(result_decla, decla.sheet, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
    ligne.titre <- getCells(getRows(decla.sheet, rowIndex = 1))
    invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER"))))
    setCellValue(ligne.titre[[1]], "Scraping du")
    setCellValue(ligne.titre[[2]], "Valeur")
    setCellValue(ligne.titre[[3]], "Déclarant")
    setCellValue(ligne.titre[[4]], "Instrument financier")
    setCellValue(ligne.titre[[5]], "Nature")
    setCellValue(ligne.titre[[6]], "Montant de l'opération (€)")
    setCellValue(ligne.titre[[7]], "Date de l'opération")
    setCellValue(ligne.titre[[8]], "Lien Dropbox")
    invisible(sapply(c(1,8), function(i){
        setCellStyle(ligne.titre[[i]], CellStyle(wb)+Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = T)+Font(wb, isItalic=TRUE, isBold=TRUE))
    }))
    invisible(sapply(2:7, function(i){
        setCellStyle(ligne.titre[[i]], CellStyle(wb)+Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = T)+Font(wb, isItalic=TRUE, isBold=TRUE, color = "whitesmoke")+ Fill(foregroundColor = "#4F81BD"))
    }))
    setRowHeight(getRows(decla.sheet,1), multiplier = 2)
    
    
    
    
    setColumnWidth(decla.sheet, colIndex=1, colWidth=15)
    setColumnWidth(decla.sheet, colIndex=2, colWidth=35)
    setColumnWidth(decla.sheet, colIndex=3, colWidth=45)
    setColumnWidth(decla.sheet, colIndex=4, colWidth=22)
    setColumnWidth(decla.sheet, colIndex=5, colWidth=20)
    setColumnWidth(decla.sheet, colIndex=6, colWidth=22)
    setColumnWidth(decla.sheet, colIndex=7, colWidth=20)
    setColumnWidth(decla.sheet, colIndex=8, colWidth=25)
    
    
    font.red <- Font(wb, color = "#C60800")
    font.green <- Font(wb, color = "darkgreen")
    fill.white <- Fill(foregroundColor = "#FFFFFF", backgroundColor = "#FFFFFF")
    fill.blue <- Fill(foregroundColor = "#DCE6F1") 
    fill.red <- Fill(backgroundColor = "#FF0000")
    dataformat.nombre <- DataFormat("### ### ##0")
    dataformat.date <- DataFormat("dd/mm/yyyy")
    border.left <- Border(color="#357AB7", c("BOTTOM", "LEFT", "TOP"), c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN"))
    border.right <- Border(color="#357AB7", c("BOTTOM", "RIGHT", "TOP"), c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN"))
    border.middle <- Border(color="#357AB7", c("BOTTOM", "TOP"), c("BORDER_THIN", "BORDER_THIN"))
    alignment.center <- Alignment(horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER")
    alignment.left <- Alignment(horizontal = "ALIGN_LEFT", vertical = "VERTICAL_CENTER")
    cs <- CellStyle(wb)
    invisible(sapply(2:(nbLigne.today+1), function(i){
        cells <- getCells(row = getRows(decla.sheet, rowIndex = i), colIndex = 1:8)
        cs.2 <- if(i%%2==0) {
            cs +  fill.blue + border.left + alignment.left + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {Font(wb, isBold = T, color = "darkgreen")},
                "Cession" = {Font(wb, isBold = T, color = "#C60800")},
                {}
            )
        }
        else {
            cs + fill.white + border.left + alignment.left + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {Font(wb, isBold = T, color = "darkgreen")},
                "Cession" = {Font(wb, isBold = T, color = "#C60800")},
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
                "Acquisition" = {font.green},
                "Cession" = {font.red},
                {}
            )
        }else{
            cs + fill.white + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Acquisition" = {font.green},
                "Cession" = {font.red},
                {}
            )
        }
        setCellStyle(cells[[5]], cs.5)
        cs.6 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center + dataformat.nombre else cs + fill.white + border.middle + alignment.center + dataformat.nombre
        setCellStyle(cells[[6]], cs.6)
        cs.7 <- if(i%%2==0) cs + fill.blue + border.right + alignment.center else cs + fill.white + border.right + alignment.center
        setCellStyle(cells[[7]], cs.7)
    }))
    
    
    
    #decla.sheet$autoSizeColumn(as.integer(3))
    
}


if(NROW(extractedTexts_franchissement) > 0){
    # partie seuil
    nbLigne <- seuil.sheet$getLastRowNum()+1
    nbLigne.today <- dim(result_seuil)[1]
    seuil.sheet$shiftRows(as.integer(1),as.integer(nbLigne-1),as.integer(nbLigne.today))
    nbCol.today <- dim(result_seuil)[2]
 
    addDataFrame(result_seuil, seuil.sheet, startRow = 2, startColumn = 1, col.names = FALSE, row.names = FALSE)
    
    cb.titre <- CellBlock(seuil.sheet, 1, 1, 1, 7)
    ligne.titre <- getCells(getRows(seuil.sheet, rowIndex = 1))
    invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER"))))
  
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
    setRowHeight(getRows(seuil.sheet, 1), multiplier = 2)
    invisible(sapply(c(1,7), function(i){
        setCellStyle(ligne.titre[[i]], CellStyle(wb) + Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER") + Font(wb, isItalic = T, isBold = T))
    }))
    invisible(sapply(2:6, function(i){
        setCellStyle(ligne.titre[[i]], CellStyle(wb) + Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER") + Font(wb, isItalic = T, isBold = T, color = "whitesmoke") + Fill(foregroundColor = "#4F81BD"))
    }))
    
    
 
    invisible(sapply(2:(nbLigne.today+1), function(i){
        cells <- getCells(row = getRows(seuil.sheet, rowIndex = i), colIndex = 1:7)
        cs.2 <- if(i%%2==0) cs +  fill.blue + border.left + alignment.left else cs + fill.white + border.left + alignment.left
        setCellStyle(cells[[2]], cs.2)
        cs.3 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.left else cs + fill.white + border.middle + alignment.left
        setCellStyle(cells[[3]], cs.3)
        cs.4 <- if(i%%2==0) cs + fill.blue + border.middle + alignment.center else cs + fill.white + border.middle + alignment.center
        setCellStyle(cells[[4]], cs.4)
        cs.5 <- if(i%%2==0) {
            cs + fill.blue + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Hausse" = {font.green},
                "Baisse" = {font.red},
                {}
            )
        }else{
            cs + fill.white + border.middle + alignment.center + switch(
                EXPR = getCellValue(cells[[5]]),
                "Hausse" = {font.green},
                "Baisse" = {font.red},
                {}
            )
        }
        setCellStyle(cells[[5]], cs.5)
        cs.6 <- if(i%%2==0) cs + fill.blue + border.right + alignment.center else cs + fill.white + border.right + alignment.center
        setCellStyle(cells[[6]], cs.6)
    }))
    invisible(sapply(1:7,function(i, largeurs){
        setColumnWidth(seuil.sheet, colIndex = i, colWidth = largeurs[i])
    },largeurs=c(15,35,45,35,20,20,64)))
    
 
}

# ===============================
# ===============================
# PUBLICATION DE POSITION
# ===============================
# ===============================
if(NROW(extractedTexts_position) > 0){
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
    # invisible(lapply(ligne.titre, setCellStyle, cellStyle=CellStyle(wb, alignment = Alignment(h = "ALIGN_CENTER"))))
    # CB.setFill(cb.titre, fill = Fill(foregroundColor = "#22427C", backgroundColor = "green"), 1, 1:7)
    # CB.setFont(cb.titre, font = Font(wb, color="whitesmoke", isItalic = TRUE, isBold = TRUE), 1, 1:7)
    setRowHeight(getRows(publi.sheet, 1), multiplier = 2)
    invisible(sapply(c(1,7), function(i){
        setCellStyle(ligne.titre[[i]], CellStyle(wb) + Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER") + Font(wb, isItalic = T, isBold = T))
    }))
    invisible(sapply(2:6, function(i){
        setCellStyle(ligne.titre[[i]], CellStyle(wb) + Alignment(h = "ALIGN_CENTER", v="VERTICAL_CENTER") + Font(wb, isItalic = T, isBold = T, color = "whitesmoke") + Fill(foregroundColor = "#4F81BD"))
    }))
    
    
    CB.setRowData(cb.titre, c(
        "Scraping du",
        "Nom de l'émetteur",
        "Détenteur de la position",
        "% de la position",
        "Date de la position",
        "Type de position",
        "Lien Dropbox"),
        rowIndex = 1
    )
    
    
    
    
    invisible(sapply(1:7,function(i, largeurs){
        setColumnWidth(seuil.sheet, colIndex = i, colWidth = largeurs[i])
    },largeurs=c(15,40,40,20,20,20,64)))
    

    invisible(sapply(2:(nbLigne+1), function(i){
        cells <- getCells(row = getRows(publi.sheet, rowIndex = i), colIndex = 1:7)
        cs.2 <- if(i%%2==0) cs +  fill.blue + border.left + alignment.left else cs + fill.white + border.left + alignment.left
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


saveWorkbook(wb, "./extracted_texts/BD-AMF.xlsx")


##upload to Dropbox
#authentificate
drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
# EuronextFilesPath <- list.files(path = ".", pattern ="\\w+\\.html$", full.names = TRUE, recursive = TRUE, all.files = FALSE, ignore.case = TRUE) 
# invisible(sapply(EuronextFilesPath, drop_upload, dest = paste0("/mail_euronext"), overwrite = FALSE))
drop_upload(file = "./extracted_texts/BD-AMF.xlsx", dest= "AMF_textes_extraits/BD-AMF_test.xlsx")


