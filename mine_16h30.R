.libPaths("C:/R_library")

setwd("C:\\Users\\jriton\\Documents\\R\\MesProjets\\AMF_textmining\\tm_declaration")




# =====================================================
# =====================================================
# we have to do manually conversion with ec2 and winscp
# =====================================================
# =====================================================

# library(xlsx)
# 
# wb <- loadWorkbook("./extracted_texts/BD-AMF.xlsx")
# sheets <- getSheets(wb)
# 
# ws.seuils <- sheets[[1]]
# ws.decla <- sheets[[2]]
# ws.publi <- sheets[[3]]
# ws.opa <- sheets[[4]]


#\ ============================
# from tm seuils olivier v1.9.2
# =============================

system_date <-
    Sys.Date() #save multiple computations and prevents unexpected behavior in extremely rare case where the code runs from one day to another
today <- format(system_date, "%Y-%m-%d")
veille <- format(Sys.Date()-1, "%Y-%m-%d")

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
textNames %<>% paste0("text_mining\\AMF\\", today, "\\seuils\\", .)

#creates the directory
date_path <- paste0("text_mining/AMF/", today, "/seuils/")
#date_path <- paste0("./data/seuils/", today, "")
if (!isTRUE(file.info(date_path)$isdir))
    dir.create(date_path, recursive = TRUE)

# for (i in seq_along(filesNames)) {
#     system(paste0("pdftotext ", "\"", filesPath[i], "\" \"", textNames[i], "\""))
# }
##

textsPath <- list.files(
    path = paste0("./text_mining/AMF/", today, "/seuils/"),
    pattern = "*.txt$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)

extractedTexts <- lapply(textsPath, readLines, encoding = "UTF-8")

extractedTexts_franchissement <-
    extractedTexts[grep("Déclarations? de franchissement", extractedTexts)]

filesNames_franchissement <-
    filesNames[grep("Déclarations? de franchissements?", extractedTexts)]
dropbox_links_franchissement <-
    paste0(
        "https://www.dropbox.com/home/AMF/",
        today,
        "/seuils?preview=",
        filesNames_franchissement
    ) %>% gsub(" ", "+", .)



##TEXT MINING ON extractedTexts_franchissement
companies <- c()
operators <- c()
crossing <- c()
crossing_date <- c()
threshold_crossed <- c()

if(NROW(extractedTexts_franchissement) > 0) {
    for (i in seq_along(extractedTexts_franchissement)) {
        #extracting companies' names
        mark <- grep("\\(Euronext|\\(Alternext", extractedTexts_franchissement[[i]])
        temp <- extractedTexts_franchissement[[i]][mark - 1]
        companies <- append(companies, temp)
        
        #extracting the rest
        subText <-
            do.call(paste, as.list(extractedTexts_franchissement[[i]][(mark + 1):(mark +
                                                                                      10)])) #subsetting the first lines
        
        #extracting the operators
        temp <-
            str_match(
                subText,
                "(?:Par courriers? re.us? le (\\d{1,2}|1er) .+? \\d{4}, )(?:compl.t. par un courrier re.u le (\\d{1,2}|1er) .+? \\d{4}, )?(?:.*?)([A-Z][a-zA-Z\\s’Îïéèôë\\.\\,&]+)(?:\\(|a déclaré|1\\s)"
            ) %>% trimws %>% .[, 4]
        if(is.na(temp))
            temp <- str_match(subText, "(?:Par courriers? re.us? le (?:\\d{1,2}|1er) .+?(?: \\d{4})?, )(?:compl.t. par un courrier re.u le (?:\\d{1,2}|1er) .+? \\d{4}, )?(.*?)(?:\\(|, a déclaré)")[, 2]
        operators <- append(operators, temp)
        
        #extracting the crossing
        temp <-
            str_match(subText,
                      "(?:(?:avoir franchi.* (?:en)|(?:à la))|(?:le franchissement en)) (\\w+)")[, 2]
        if (is.na(temp) || !temp %in% c("baisse", "hausse"))
            temp <- str_match(subText, "baisse|hausse")[, 1]
        crossing <- append(crossing, temp)
        
        #extracting the crossing_date
        temp <-
            str_match(subText,
                      "(?:hausse, le|baisse, le) ((\\d{1,2}|1er) .+? \\d{4})")[, 2]
        if (is.na(temp))
            temp <- str_match(subText, "(?:avoir franchi, le )(.+?)(?:,)")[, 2]
        crossing_date <- append(crossing_date, temp)
        
        #extracting the threshold_crossed
        temp <-
            str_match(
                subText,
                "(?:(?:du|les?) seuils? de )(.+?)(?:de la soci.t.)"
                # "(?:hausse, le|baisse, le) (?:(?:\\d{1,2}|1er) .+? \\d{4}, )(?:.+?)?(?:(?:du|les?) seuils? de )(.+?)(?:de la soci.t.)"
            ) %>% trimws %>% .[, 2]
        threshold_crossed <- append(threshold_crossed, temp)
    } # end for (extractedTexts_franchissement)
    #today %<>% rep(., NROW(extractedTexts_franchissement)) #clean the warning of cbind on the length of the vectors
    # result_seuil <-
    #     cbind(
    #         today,
    #         companies,
    #         operators,
    #         threshold_crossed,
    #         crossing,
    #         crossing_date,
    #         dropbox_links_franchissement
    #     )
    # colnames(result_seuil) <- NULL
    # #result_seuil <- data.frame(rep(today,NROW(extractedTexts_franchissement)), companies, operators, threshold_crossed, crossing, crossing_date, dropbox_links_franchissement)
    result_seuil <- data.frame(
        scrapDate <- rep(today, NROW(extractedTexts_franchissement)),
        companies = companies,
        operators = operators,
        threshold_crossed = threshold_crossed,
        crossing = crossing,
        crossing_date = crossing_date,
        dropbox_links_franchissement = dropbox_links_franchissement
    )
}


##text-mining publication de position
##TEXT MINING ON extractedTexts_franchissement
extractedTexts_position <-
    extractedTexts[grep("Publication des positions", extractedTexts)]
filesNames_position <-
    filesNames[grep("Publication des positions", extractedTexts)]
dropbox_links_position <-
    paste0(
        "https://www.dropbox.com/home/AMF/",
        today,
        "/seuils?preview=",
        filesNames_position
    ) %>% gsub(" ", "+", .)


detenteur <- c()
emetteur <- c()
typePosition <- c()
pctPosition <- c()
datePosition <- c()

if(NROW(extractedTexts_position) > 0) {
    for (i in seq_along(extractedTexts_position)) {
        extrait <- extractedTexts_position[[i]]
        #extracting detenteur
        mark <- grep("^POSITION HOLDER$", extractedTexts_position[[i]])
        temp <- extractedTexts_position[[i]][mark + 3]
        detenteur <- append(detenteur, temp)
        
        # #extracting the rest
        # subText <-
        # 	do.call(paste, as.list(extractedTexts_franchissement[[i]][(mark + 1):(mark +
        # 																		  	10)])) #subsetting the first lines
        # 
        # #extracting the emetteur
        mark <- grep("^NOM DE L..METTEUR$", extractedTexts_position[[i]])
        temp <- extractedTexts_position[[i]][mark + 3]
        emetteur <- append(emetteur, temp)
        # 
        # #extracting the typePosition (type de position)
        # temp <-
        # 	str_match(extractedTexts_position[[i]],
        # 			  "(?:(?:avoir franchi.* (?:en)|(?:à la))|(?:le franchissement en)) (\\w+)")[, 2]
        # if (is.na(temp) || !temp %in% c("baisse", "hausse"))
        # 	temp <- str_match(subText, "baisse|hausse")[, 1]
        # crossing <- append(crossing, temp)
        ind <- str_extract(extrait, "^POSITION .+? NETTE")
        ind <-  which(!is.na(ind)==TRUE)[1]
        temp <- extrait[ind] %>% str_replace(., "POSITION ", "") %>% str_replace(., " NETTE.*$", "")
        typePosition <- append(typePosition, temp)
        
        # 
        # #extracting the pctPosition
        temp <- extrait[ind + 3]
        pctPosition <- append(pctPosition, temp)
        # 
        # #extracting the datePosition
        ind <- str_extract(extrait, "^DATE DE POSITION")
        ind <-  which(!is.na(ind)==TRUE)[1] + 3
        temp <- extrait[ind] 
        datePosition <- append(datePosition, temp)
    }
    today <- format(system_date, "%Y-%m-%d") # we recreate today because it has been modified before into a vector
   # today %<>% rep(., NROW(extractedTexts_position)) #clean the warning of cbind on the length of the vectors
    result_publi <-
        cbind(
            today,
            detenteur,
            emetteur,
            pctPosition,
            datePosition,
            typePosition,
            dropbox_links_position
        )
    colnames(result_publi) <- NULL
    result_publi <- data.frame(
        scrapDate = rep(today, NROW(extractedTexts_position)),
        detenteur = detenteur,
        emetteur = emetteur,
        pctPosition = pctPosition,
        datePosition = datePosition,
        typePosition = typePosition,
        dropbox_links_position = dropbox_links_position
    )
}   


# ==================================
# ==================================
# from text-mining-pdf-declaration.R
# ==================================
# ==================================
.libPaths("C:/R_library")
library(stringr)
library(xlsx)
library(magrittr)
library(rdrop2)



system_date <- Sys.Date()
today <- format(system_date, "%Y-%m-%d")

###declaration TYPE PARAGRAPHS
filesPath <- list.files(
    path = paste0("./data/AMF_16h30/", today, "/declaration/"),
    pattern = "*.pdf$",
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    ignore.case = TRUE
)


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
textNames %<>% paste0("text_mining\\AMF\\", today, "\\declaration\\", .)





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


# Text-mining on modele2
for(i in seq_along(extractedTexts_modele2)){
    # le texte en vecteur sur lequel on tr
    extrait <- extractedTexts_modele2[[i]]
    
    # extracting the column "valeur"
    ind <- str_extract(extrait, "COORDONN.ES DE L..METTEUR")
    ind <-  which(!is.na(ind)==TRUE)[1] + 1
    temp <- extrait[ind] %>% str_replace(., "NOM : ", "")
    valeur <- append(valeur, temp)
    
    # extracting "declarant"
    ind <- str_extract(extrait, "PERSONNE .TROITEMENT LI.E")
    ind <-  which(!is.na(ind)==TRUE)[1] + 1
    temp <- extrait[ind]
    declarant <- append(declarant, temp)
    # 
    # extracting "instrument"
    ind <- str_extract(extrait, "INSTRUMENT FINANCIER")
    ind <-  which(!is.na(ind)==TRUE)[1]
    temp <- extrait[ind] %>% str_replace(., "DESCRIPTION DE L.INSTRUMENT FINANCIER : ", "")
    instru <- append(instru, temp)
    
    
    
    # 
    # # extracting "nature"
    ind <- str_extract(extrait, "NATURE DE LA TRANSACTION")
    ind <- which(!is.na(ind)==TRUE)[1]
    temp <- extrait[ind] %>% str_replace(., "NATURE DE LA TRANSACTION : ", "") 
    nature <- append(nature, temp)
    
    # 
    # # extracting the column "montant"
    ind <- str_extract(extrait, "PRIX UNITAIRE")
    ind <-  which(!is.na(ind)==TRUE)[1]
    prix <- extrait[ind] %>% str_replace(., "^PRIX UNITAIRE : ", "") %>% str_replace(., " Euros?\\s*$", "") 
    # formattage de prix en numeric
    prix <- as.numeric(str_replace_all(prix, " ", ""))
    volume <- extrait[ind+1] %>% str_replace(., "^VOLUME : ", "")
    volume <- as.numeric(str_replace_all(volume, " ", ""))
    temp <- prix * volume
    montant <- append(montant, temp)
    # 
    # # extracting the column "date"
    ind <- str_extract(extrait, "DATE DE LA TRANSACTION")
    ind <-  which(!is.na(ind)==TRUE)[1]
    temp <- extrait[ind] %>% str_replace(., "DATE DE LA TRANSACTION : ", "")
    dateOp <- append(dateOp, temp)
    
    
}



for(i in seq_along(instru)){
    if(instru[i]=="Shares"){
        instru[i] <- "Actions"
    }
}

result_decla <- 
    cbind(
        today,
        valeur,
        declarant,
        instru,
        nature,
        montant,
        dateOp,
        dropbox_links_modele2
        
    )
colnames(result_decla) <- NULL
result_decla <- data.frame(
    today = rep(today,length(extractedTexts_modele2)),
    valeur = valeur,
    declarant = declarant,
    instru = instru,
    nature = nature,
    montant = montant,
    dateOp = dateOp,
    dropbox_links_modele2
)
# ==================
# ==================
# OPA 
# ==================
# ==================


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
# extractedTexts_declaration <-
#     extractedTexts[grep("Déclaration des achats et des ventes", extractedTexts)]
# extractedTexts_communication <-
#     extractedTexts[grep("Communication sur la société", extractedTexts)]

# extraction des indices des textes qui ne comporte qu'une seule ligne d'achat
ind.decla <- grep("Déclaration des achats et des ventes", extractedTexts) 
# for(i in seq_along(extractedTexts_declaration)){
#     if(any(str_detect(extractedTexts_declaration[[i]], "^\\* ."))){
#         ind.declastar <- append(ind.declastar,i)
#     }
# }
ind.decla.monoachat <- c()
for(i in ind.decla){
    s=0
    extrait <- extractedTexts[[i]]
    for(ligne in extrait){
        if(str_detect(ligne, "le \\d{2}/\\d{2}/\\d{4}$")){
            s <- s + 1
        }
    }
    if(s == 1){
        ind.decla.monoachat <- c(ind.decla.monoachat, i)
    }
}


# extractedTexts_declaration_blank <- extractedTexts_declaration[ind.decla.monoachat.blank]

filesNames_monoachat <-
    filesNames[ind.decla.monoachat]
dropbox_links_monoachat <-
    paste0(
        "https://www.dropbox.com/home/AMF/",
        today,
        "/OPA?preview=",
        filesNames_monoachat
    ) %>% gsub(" ", "+", .)


# filesNames_declaration_blank <-
#     filesNames[ind.decla.monoachat.blank]
# dropbox_links_declaration_blank <-
#     paste0(
#         "https://www.dropbox.com/home/AMF/",
#         today,
#         "/OPA?preview=",
#         filesNames_declaration_blank
#     ) %>% gsub(" ", "+", .)


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


if(length(ind.decla.monoachat) > 0) {
    for (i in ind.decla.monoachat) {
        extrait <- extractedTexts[[i]]
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
        mark.soc <- grep("^\\(Euronext|\\(Alternext", extrait)
        temp.soc <- extrait[mark.soc - 1]
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
#    today %<>% rep(., length(ind.decla.monoachat))
    
    # result_monoachat <- 
    #     cbind(
    #         today,
    #         societe,
    #         operateur,
    #         nature,
    #         dateOp,
    #         titreConcerne,
    #         cours,
    #         nbTotal,
    #         dropbox_links_monoachat
    #     )
    # colnames(result_monoachat) <- NULL
    result_monoachat <- data.frame(
        scrapDate = rep(today, length(ind.decla.monoachat)),
        societe = societe,
        operateur,
        nature = nature,
        dateOp = dateOp,
        titreConcerne = titreConcerne,
        cours = cours,
        nbTotal = nbTotal,
        dropbox_links_monoachat = dropbox_links_monoachat
    )
}








