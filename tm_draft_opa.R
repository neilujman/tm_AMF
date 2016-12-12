.libPaths("C:/R_library")
library(dplyr)
library(stringi)
library(stringr)

# texte 1

text.file.path <- "C:/Users/jriton/Documents/R/MesProjets/AMF_textmining/tm_declaration/text_mining/AMF-layout_16h30/2016-12-12/OPA/AUSY - 216C2765.txt"

extrait <- readLines(con = text.file.path, encoding = "UTF-8")
myDatas <- data.frame(Operateur=c(NULL), Nature.date=c(NULL), Titre.concerne=c(NULL), Cours=c(NULL), Nombre.total=c(NULL))



ind.central <- grep("Euronext|Alternext", extrait)

societe <- extrait[ind.central-2]


ind.header <- grep("Titres concernés", extrait)

i <- 0
while(!is.na(unlist(stri_extract_all_regex(extrait[ind.header+i],"\\w"))[1])){
    i <- i+1
    
}
# ind.datas.begin is the index of the first line containing the information we want
ind.datas.begin <- ind.header+2+i
# we seek for the index of the end of the information
i <- 0
while(!is.na(unlist(stri_extract_all_regex(extrait[ind.datas.begin+i],"\\w"))[1])){
    i <- i+1
}
ind.datas.end <- ind.datas.begin + i - 1
extracted.datas <- extrait[ind.datas.begin:ind.datas.end]

# header analysis
header.line <- extrait[ind.header]
header.element <- stri_split_regex(header.line, "\\s\\s+") %>% unlist %>% setdiff(., c(""))
header.graduation <- str_locate(header.line, header.element)

cat("il y a", length(extracted.datas), "lignes de données")

# ligne 1 
data1 <- stri_split_regex(extracted.datas[1], "\\s\\s+") %>% unlist %>% setdiff(., c(""))
# car nous avons pile poil 5 éléments comme le nombre de colonne
data.tmp <- data.frame(
    Operateur=data1[1], 
    Nature.date=data1[2], 
    Titre.concerne=data1[3],
    Cours=data1[4],
    Nombre.total=data1[5]
)

myDatas <- bind_rows(myDatas, data.tmp)

# ligne 2
data2 <- stri_split_regex(extracted.datas[2], "\\s\\s+") %>% unlist %>% setdiff(., c(""))
# ici on a moins d'élément que de colonnes
