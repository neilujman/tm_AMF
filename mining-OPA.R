# sourcer text-mining-pdf-OPA_decla_star et ..._decla_blank
# récupérer ind.decla.monoachat et ind.decla
ind.decla.polyachat <- setdiff(ind.decla, ind.decla.monoachat) # comm avec plusieurs ligne de date



.libPaths("C:/R_library")
library(stringr)
zz <- file("text_mining/AMF/2016-10-07/OPA/html/ALCATEL LUCENT - 216C2268-1.html")
page.str <- readLines(con = zz, encoding = "UTF-8")
close(zz)
setClass(
    Class = "Cellule",
    representation = representation(
        left = "integer",
        top = "integer",
        texte = "character"
    )
)
setMethod(
    f="initialize",
    signature = "Cellule",
    definition = function(.Object, left, top, texte){#, texte, aVoisine
        .Object@left <- left
        .Object@top <- top
        .Object@texte <- texte
        # 
        return(.Object)
    }
)
setMethod(
    f ="[",
    signature = "Cellule",
    definition = function(x, i, j, drop){
        switch(
            EXPR=i,
            "left" = {return(x@left)},
            "top" = {return(x@top)},
            "texte" = {return(x@texte)}
        )
    }
)
setReplaceMethod(
    f = "[",
    signature = "Cellule",
    definition = function(x,i,j,value){
        switch(EXPR=i,
               "left"={x@left <- value},
               "top"={x@top<-value},
               "texte"={x@texte<-value})
    }
)

setClass(
    Class = "Page",
    representation = representation(
        #numero = "integer",
        cellules = "vector" # en fait c'est une liste de Cellule
        #nbCellules = "integer"
    )
)


setMethod(
    f = "initialize",
    signature = "Page",
    definition = function(.Object, page.str){
        # page.str est le communiqué sous la forme html
        
        ind <- grep("^<p .+$", page.str)
        vec.str <- page.str[ind]
        
        #vec <- vec[vec!=NA]
        
        monvec <- c()
        for(ligne in vec.str){
           
            left <- str_extract(ligne, "left:\\d+px")%>%str_replace(.,"^left:","")%>%str_replace(., "px$", "")%>% as.integer(.)
            top <- str_extract(ligne, "top:\\d+px")%>%str_replace(.,"^top:","")%>%str_replace(., "px$", "")%>% as.integer(.)
            
            if(str_detect(ligne, "<b>")){
                texte <- str_extract(ligne, "<b>.+</b>") %>% str_replace_all(., "(<b>)|(</b>)", "") 
            }else{
                texte <- str_extract(ligne, "class=\"ft.+</p>$") %>% str_replace_all(., "(^class=.+?>)|(</p>$)", "")
            }
            
            cell <- new("Cellule", left, top, texte)
            monvec <- append(monvec, cell)
        }
        
        
        .Object@cellules <- monvec
        
        return(.Object)
        
    }
)
setMethod(
    f = "[",
    signature = "Page",
    definition = function(x, i, j, drop){
        switch(EXPR = i,
               #"numero" = {return(x@numero)},
               "cellules" = {return(x@cellules)}#,
               #"nbCellules" = {return(x@nbCellules)}
        )
    }
)


foo <- new("Page", page.str)
myCells <- foo["cellules"]
grep("Nature.+date.+opération", page.str)
# recherche de la cellule contenant "Nature date opération"
i=1
convient <- FALSE
while(convient==FALSE){
    if(str_detect(myCells[[i]]["texte"], "Nature.+date.+opération")){
        convient = TRUE
        break
    }
    i<-i+1
}
left.nature <- myCells[[i]]["left"]
top.nature <- myCells[[i]]["top"]

grep("le \\d{2}/\\d{2}/\\d{4}", page.str)# vérification
# recherche de la cellule contenant "Nature date opération"
i=1
convient <- FALSE
while(convient==FALSE){
    if(str_detect(myCells[[i]]["texte"], "Nature.+date.+opération")){
        convient = TRUE
        break
    }
    i<-i+1
}

# on récupère les cellules à gauche de nature
gauche_nature <- c()
i=1
for(cellule in myCells){
    if(cellule["left"]<left.nature && cellule["top"] > top.nature){
        gauche_nature <- append(gauche_nature, i)
    }
    
    i<-i+1
}