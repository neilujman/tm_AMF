# on va travailler sur un texte qui comporte deux opérations
library(stringr)

texte <- readLines(con = file("texts/VICAT S.A. - 2016DD442182.txt")) %>% str_c(., collapse="\n")


nbOp <- function(texte){
# cherche le nombre d'operations "nbOperations" comptabilisés dans un communiqué "texte".
    motif <- "PRIX UNITAIRE"
    i <- 1
    motif.i <- paste(motif, i)
    while(str_detect(string = texte, pattern= motif.i)){
        i <- i+1
        motif.i <- paste(motif, i)
    }
    switch(i,
           "1"={
               nbOperations <- 1
           },
           "2"={
               nbOperations <- 1
           },
           "3"={
               nbOperations <- 2
           },
           "4"={
               nbOperations <- 3
           }
    )
    return(nbOperations)
}

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




ajoutSociete <- function(txt, nbOp, societe){
    # txt: a text where we want to find the name of the society
    nom <- str_extract(txt, "D.CLARANT.\n\n?.+?\n\n?")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(D.CLARANT.)|(\n)", "")
        
    }else{
        nom = NA
    }
    for(i in 1:nbOp){
        societe <<- append(societe, nom)
    }
}
ajoutDeclarant <- function(txt, nbOp, declarant){
    nom <- str_extract(txt, "D.CLARANT : .+? INSTRUMENT FINANCIER")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(D.CLARANT : )|( INSTRUMENT FINANCIER)", "")
        nom <- str_replace_all(nom, ",.+?$", "")
        
    }else{
        nom <- str_extract(txt, "D.CLARANT : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(D.CLARANT : )", "")
            nom <- str_replace_all(nom, ",.+?\n", "")
        }else
            nom <- NA
    }
    for(i in 1:nbOp){
        declarant <<- append(declarant, nom)
    }
}
ajoutPoste <- function(txt, nbOp, poste){
    nom <- str_extract(txt, "D.CLARANT : .+? INSTRUMENT FINANCIER")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(D.CLARANT : )|( INSTRUMENT FINANCIER)", "")# all is written on one line
        nom <- str_replace_all(nom, "^.+?,", "")
        
    }else{
        nom <- str_extract(txt, "D.CLARANT : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(D.CLARANT : )|(\n)", "")
            nom <- str_replace_all(nom, "^.+?,", "")
        }else
            nom <- NA
    }
    for(i in 1:nbOp){
        poste <<- append(poste, nom)
    }
}
ajoutInstruFin <- function(txt, nbOp, instruFin){
    nom <- str_extract(txt, "INSTRUMENT FINANCIER : .+? NATURE")# all is written on one line
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(INSTRUMENT FINANCIER : )|( NATURE)", "")
        nom <- str_replace_all(nom, "^.+?,", "")
        
    }else{
        nom <- str_extract(txt, "INSTRUMENT FINANCIER : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(INSTRUMENT FINANCIER : )|(\n)", "")
            nom <- str_replace_all(nom, "^.+?,", "")
        }else
            nom <- NA
    }
    for(i in 1:nbOp){
        instruFin <<- append(instruFin, nom)
    }
}
ajoutNatOp <- function(txt, nbOp, natOp){
    nom <- str_extract(txt, "NATURE DE L'OP.RATION : .+? DATE")# all is written on one line
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(NATURE DE L'OP.RATION : )|( DATE)", "")
        
    }else{
        nom <- str_extract(txt, "NATURE DE L'OP.RATION : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(NATURE DE L'OP.RATION : )|(\n)", "")
            
        }else
            nom <- NA
    }
    for(i in 1:nbOp){
        natOp <<- append(natOp, nom)
    }
}
ajoutDateOp <- function(txt, nbOp, dateOp){
    nom <- str_extract(txt, "DATE DE L'OP.RATION : .+? DATE")# all is written on one line
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(DATE DE L'OP.RATION : )|( DATE)", "")
        
    }else{
        nom <- str_extract(txt, "DATE DE L'OP.RATION : .+?\n")
        if(!is.na(nom)){
            nom <- str_replace_all(nom, "(DATE DE L'OP.RATION : )|(\n)", "")
         
        }else
            nom <- NA
    }
    for(i in 1:nbOp){
        dateOp <<- append(dateOp, nom)
    }
}
ajoutMontantOp <- function(txt, nbOp, montantOp){
    # txt: a text where we want to find the name of the society
    nom <- str_extract(txt, "MONTANT DE L'OP.RATION : .+?Euros?")
    if(!is.na(nom)){
        #nom <- str_sub(nom, 11, str_length(nom))
        nom <- str_replace_all(nom, "(MONTANT DE L'OP.RATION : )|(Euros?)", "")
        
    }else{
        nom <- NA
    }
    for(i in 1:nbOp){
        montantOp <<- append(montantOp, nom)
    }
    
}

# in order to put the differents components
societe=c()
declarant = c()
poste = c()
instruFin = c()
natOp = c() # nature de l'operation
dateOp = c()
montantOp = c()

# retrieve of "societe", "declarant", "poste", "instrument financier",
#   "nature de l'operation", "date de l'operation", "montant de l'operation".
nbTexts <- length(myTexts)
for(i in 1:nbTexts){
    aText <- myTexts[i]
    nbOperations <- nbOp(aText)
    ajoutSociete(aText, nbOperations, societe)
    ajoutDeclarant(aText, nbOperations, declarant)
    ajoutPoste(aText, nbOperations, poste)
    ajoutInstruFin(aText, nbOperations, instruFin)
    ajoutNatOp(aText, nbOperations, natOp)
    ajoutDateOp(aText, nbOperations, dateOp)
    ajoutMontantOp(aText, nbOperations, montantOp)
}



declaration.df <- data.frame(
    "Societe" = societe,
    "Declarant" = declarant,
    "Poste" = poste,
    "Instrument financier" = instruFin,
    "Nature de l'operation" = natOp,
    "Date de l'operation" = dateOp,
    "Montant de l'operation" = montantOp
)