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



filesNames_declaration <-
	filesNames[grep("Déclaration des achats et des ventes", extractedTexts)]
dropbox_links_declaration <-
	paste0(
		"https://www.dropbox.com/home/AMF/",
		today,
		"/seuils?preview=",
		filesNames_declaration
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


if(NROW(extractedTexts_declaration) > 0) {
	for (i in seq_along(extractedTexts_declaration)) {
		extrait <- extractedTexts_declaration[[i]]
		mark.op <- grep("^Opérateur$", extrait)
		mark.star <- grep("^\\* .", extrait)
		extrait.sub <- extrait[(mark.op-1):(mark.star-1)]
		
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
	
	today <- format(system_date, "%Y-%m-%d")
	today %<>% rep(., NROW(extractedTexts_declaration))
	
	result_declaration <- 
		cbind(
			today,
			societe,
			operateur,
			nature,
			dateOp,
			titreConcerne,
			cours,
			nbTotal,
			dropbox_links_declaration
		)
	colnames(result_declaration) <- NULL
	
	
}





#########################################################################

## récupération des phrases d'intérêts, ie celles du tableau
####
extrait <- extractedTexts_declaration[[2]]
extrait.sub <- extrait[9:32]

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


ind <- grep("^$", extrait.sub)
ind <- ind[-length(ind)]
phrases <- c()
for(i in ind){
	print(i)
	s0 <- mkSentence(i)
	s1 <- treatSentence(s0)
	phrases <- append(phrases, s1)
}
####

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

# récup du nombre total
phrases <- phrases[-c(mark,i)]


temp.nb <- phrases[2]


#########################################