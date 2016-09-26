.libPaths("C:/R_library")
library(stringr)
library(xlsx)
library(magrittr)



system_date <- Sys.Date()
today <- format(system_date, "%Y-%m-%d")

###declaration TYPE PARAGRAPHS
filesPath <- list.files(
	path = paste0("./data/AMF/", today, "/declaration/"),
	pattern = "*.pdf$",
	full.names = TRUE,
	recursive = FALSE,
	all.files = FALSE,
	ignore.case = TRUE
)


filesNames <- list.files(
	path = paste0("./data/AMF/", today, "/declaration/"),
	pattern = "*.pdf$",
	full.names = FALSE,
	recursive = FALSE,
	all.files = FALSE,
	ignore.case = TRUE
)


# ##converting PDF to html
# htmlNames <-
# 	str_replace(string = filesNames,
# 				pattern = "\\.pdf$",
# 				replacement = "")
# filesPath <-
# 	gsub("^..", "", filesPath) %>% gsub("/", "\\\\", .) #removes dot and forward slash first character
# #generates destination paths
# htmlNames %<>% paste0("text_mining\\AMF\\", today, "\\declaration\\", .)
# 

##converting PDF to text
textNames <-
	str_replace(string = filesNames,
				pattern = "pdf$",
				replacement = "txt")
filesPath <-
	gsub("^..", "", filesPath) %>% gsub("/", "\\\\", .) #removes dot and forward slash first character
#generates destination paths
textNames %<>% paste0("text_mining\\AMF\\", today, "\\declaration\\", .)



#creates the directory
date_path <- paste0("text_mining/AMF/", today, "/declaration/")
#date_path <- paste0("./data/seuils/", today, "")
if (!isTRUE(file.info(date_path)$isdir))
	dir.create(date_path, recursive = TRUE)

for (i in seq_along(filesNames)) {
	system(paste0("pdftotext -enc \"UTF-8\" ", "\"", filesPath[i], "\" \"", textNames[i], "\""))
}
##



textsPath <- list.files(
	path = paste0("./text_mining/AMF/", today, "/declaration/"),
	pattern = "*.txt$",
	full.names = TRUE,
	recursive = FALSE,
	all.files = FALSE,
	ignore.case = TRUE
)


extractedTexts <- lapply(textsPath, readLines, encoding = "UTF-8")



extractedTexts_modele1 <- extractedTexts[grep("INFORMATIONS COMPL.MENTAIRES", extractedTexts)] # correspond à l'ancien modele
extractedTexts_modele2 <- extractedTexts[grep("COORDONNEES DE L..METTEUR", extractedTexts)] # correspond au nouveau modele


filesNames_modele1 <-
	filesNames[grep("INFORMATIONS COMPL.MENTAIRES", extractedTexts)]
dropbox_links_modele1 <-
	paste0(
		"https://www.dropbox.com/home/AMF/",
		today,
		"/declaration?preview=",
		filesNames_modele1
	) %>% gsub(" ", "+", .)

# ici, on subdivise en sous-modèle
# modele1_1 : declarant instru nature date sont sur le même ligne
filesNames_modele1_1 <- 
	filesNames_modele1[grep("^DECLARANT.+?INSTRU.+?NATURE.+?DATE", extractedTexts_modele1)]




filesNames_modele2 <-
	filesNames[grep("COORDONNEES DE L..METTEUR", extractedTexts)]
dropbox_links_modele2 <-
	paste0(
		"https://www.dropbox.com/home/AMF/",
		today,
		"/declaration?preview=",
		filesNames_modele2
	) %>% gsub(" ", "+", .)






valeur <- c()
declarant <- c()
instru <- c()
nature <- c()
montant <- c()
dateOp <- c()

# Text-mining on modele1
for(i in seq_along(extractedTexts_modele1)){
	# le texte en vecteur sur lequel on tr
	extrait <- extractedTexts_modele1[[i]]
	
	# extracting the column "valeur"
	ind <- str_extract(extrait, "LA PR.SENTE D.CLARATION")
	ind <-  which(!is.na(ind)==TRUE)[1] +2
	temp <- extrait[ind]
	valeur <- append(valeur, temp)
	
	# extracting "declarant"
	ind <- str_extract(extrait, "D.CLARANT.+?INSTRUMENT FINANCIER")
	ind <-  which(!is.na(ind)==TRUE)[1]
	temp <- str_extract(extrait[ind], "D.CLARANT.+?INSTRUMENT FINANCIER")
	temp <- str_replace(temp, "D.CLARANT : ", "")
	temp <- str_replace(temp, " INSTRUMENT FINANCIER", "")
	if(!is.na(temp)){
	declarant <- append(declarant, temp)
	}else{
		ind <- grep(pattern = "^D.CLARANT", extrait)
		temp <- extrait[ind]
		temp <- str_replace(temp, "D.CLARANT : ", "")
		declarant <- append(declarant, temp)
	}
	
	# extracting "instrument"
	ind <- str_extract(extrait, "D.CLARANT.+?INSTRUMENT FINANCIER")
	ind <-  which(!is.na(ind)==TRUE)[1]
	temp <- str_extract(extrait[ind], "INSTRUMENT FINANCIER.+?NATURE")
	temp <- str_replace(temp, "INSTRUMENT FINANCIER : ", "")
	temp <- str_replace(temp, " NATURE", "")
	if(!is.na(temp)){
		instru <- append(instru, temp)
	}else{
		ind <- grep(pattern = "INSTRUMENT", extrait)
		temp <- extrait[ind]
		temp <- str_replace(temp, "INSTRUMENT FINANCIER : ", "")
		instru <- append(instru, temp)
	}
	
	# extracting "nature"
	ind <- str_extract(extrait, "INSTRUMENT FINANCIER")
	ind <-  which(!is.na(ind)==TRUE)[1]
	temp <- str_extract(extrait[ind], "NATURE.+?DATE")
	temp <- str_replace(temp, "NATURE DE L.OP.RATION : ", "")
	temp <- str_replace(temp, " DATE", "")
	if(!is.na(temp)){
		nature <- append(nature, temp)
	}else{
		ind <- str_extract(extrait,"^INSTRUMENT.+?NATURE DE L.OP.RATION")
		ind <-  which(!is.na(ind)==TRUE)[1]
		temp <- extrait[ind]
		temp <- str_replace(temp, "INSTRUMENT.+?OP.RATION : ", "")
		if(!is.na(temp)){
			nature <- append(nature, temp)
		} else{
			ind <- str_extract(extrait,"^NATURE")
			ind <-  which(!is.na(ind)==TRUE)[1]
			temp <- extrait[ind]
			temp <- str_replace(temp, "NATURE.+?OP.RATION : ", "")
			if(!is.na(temp)){
				nature <- append(nature, temp)
			} else{
				nature <- append(nature, "Pas trouvé")
			}
		}
	}
	
	# extracting the column "montant"
	ind <- str_extract(extrait, "MONTANT")
	ind <-  which(!is.na(ind)==TRUE)[1]
	temp <- str_extract(extrait[ind], "MONTANT.+?Euro")
	temp <- str_replace(temp, "MONTANT DE L.OP.RATION : ", "")
	temp <- str_replace(temp, " Euro", "")
	montant <- append(montant, temp)
	
	# extracting the column "date"
	ind <- str_extract(extrait, "D.CLARANT.+?INSTRUMENT FINANCIER")
	ind <-  which(!is.na(ind)==TRUE)[1]
	temp <- str_extract(extrait[ind], "DATE.+?DATE")
	temp <- str_replace(temp, "DATE.+? : ", "")
	temp <- str_replace(temp, " DATE", "")
	if(!is.na(temp)){
		dateOp <- append(dateOp, temp)
	}else{
		ind <- str_extract(extrait,"^DATE")
		ind <-  which(!is.na(ind)==TRUE)[1]
		temp <- extrait[ind]
		temp <- str_replace(temp, "^DATE DE L.OP.RATION : ", "")
		temp <- str_replace(temp, " DATE DE R.CEPTION.+?$", "")
		if(!is.na(temp)){
			dateOp <- append(dateOp, temp)
		}else{
			dateOp <- append(dateOp, "Pas trouvé")
		}
	}
	
	
}



for(i in seq_along(instru)){
	if(instru[i]=="Shares"){
		instru[i] <- "Actions"
	}
}




# df <- data.frame(valeur=valeur, declarant=declarant, instru = instru, nature = nature, montant=montant, dateOp = dateOp)
# write.xlsx(df, "truc.xlsx", Encoding("UTF-8"))

result <- 
	cbind(
		today,
		valeur,
		declarant,
		instru,
		nature,
		montant,
		dateOp,
		dropbox_links_modele1
		
	)
colnames(result) <- NULL


AMF_workbook <- createWorkbook()
AMF_decla <-
	createSheet(wb = AMF_workbook, sheetName = "Déclaration des dirigeants")
#generating Excel xlsx file
if (!file.exists("./extracted_texts/declaration_modele1.xlsx")) {
	#initialize output file and its columns
	#initializes rows and cells for Dropbox links
	row   <- createRow(AMF_decla, 1)
	cells  <- createCell(row, colIndex = 1:8)
	
	column_titles <-
		c(
			"Scraping du",
			"Valeur",
			"Déclarant",
			"Instrument financier",
			"Nature de l'opération",
			"Montant de l'opération",
			"Date de l'opération",
			"Lien DropBox"
		) %>% t
	addDataFrame(
		x = column_titles,
		sheet = AMF_decla,
		row.names = FALSE,
		col.names = FALSE
	)
	saveWorkbook(AMF_workbook,
				 "./extracted_texts/declaration_modele1.xlsx")
}


existing_data <-
	read.xlsx(
		"./extracted_texts/declaration_modele1.xlsx",
		# sheetName = "Déclaration des dirigeants", # ne marche pas 
		sheetIndex = 1,
		header = FALSE,
		encoding = "UTF-8"
	) %>% as.matrix
colnames(existing_data) <- NULL


#newest data on the top of the worksheet
new_data <- rbind(existing_data, result)
temp <- new_data [1, ]
temp_2 <-
	new_data[2:NROW(new_data), ] %>% .[order(.[, 1], decreasing = TRUE), ]
new_data <- rbind(temp, temp_2)

addDataFrame(
	x = new_data,
	sheet = AMF_decla,
	row.names = FALSE,
	col.names = FALSE
)


setColumnWidth(AMF_decla, colIndex=1, colWidth=14)
setColumnWidth(AMF_decla, colIndex=2, colWidth=24)
setColumnWidth(AMF_decla, colIndex=3, colWidth=32)
setColumnWidth(AMF_decla, colIndex=4, colWidth=14)
setColumnWidth(AMF_decla, colIndex=5, colWidth=16)
setColumnWidth(AMF_decla, colIndex=6, colWidth=22)
setColumnWidth(AMF_decla, colIndex=7, colWidth=20)
setColumnWidth(AMF_decla, colIndex=8, colWidth=25)


saveWorkbook(AMF_workbook,
			 "./extracted_texts/declaration_modele1.xlsx")



drop_auth(
	new_user = FALSE,
	key = "zp7o3uxjg4sd67v",
	secret = "gpb8q7bskcgojt1",
	cache = TRUE
)
drop_upload(
	"./extracted_texts/declaration_modele1.xlsx",
	dest = "/AMF_textes_extraits",
	overwrite = TRUE
)
