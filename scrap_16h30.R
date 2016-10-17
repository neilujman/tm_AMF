.libPaths("C:/R_library")

setwd("C:\\Users\\jriton\\Documents\\R\\MesProjets\\AMF_textmining\\tm_declaration")



# =========================
# From amf_scraper-seuils.R v1.6
# =========================

library(rvest)
library(magrittr)
library(rdrop2)
library(stringr)

library(dplyr)
#library(httr)
#library(downloader)
#library(RSelenium)
#library(plyr)
#library(tools)

# lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
# lego_movie %>%
#   html_nodes("#titleCast .itemprop span") %>%
#   html_text()

system_date <-
    Sys.Date() #save multiple computations and prevents unexpected behavior in extremely rare case where the code runs from one day to another
today <- format(system_date, "%Y-%m-%d")
#veille <- format(Sys.Date()-1, "%Y-%m-%d")
#0-6 starting on Sunday
if ((system_date %>% as.character(.) %>% as.POSIXlt(.) %>% .$wday) == 1) {
    #Are we Monday today?
    veille <- format(Sys.Date()-3, "%Y-%m-%d")
} else {
    veille <- format(Sys.Date()-1, "%Y-%m-%d")
}


load(file = paste0("./docs_id/docs_id_", veille, ".RData"))# load docs_id_decla, docs_id_op, docs_id_seuils et docs_id_prospectus
docs_id_veille_seuils <- data.frame(id = seq_along(docs_id_seuils), doc_id = docs_id_seuils)
docs_id_veille_decla <- data.frame(id = seq_along(docs_id_decla), doc_id = docs_id_decla)
docs_id_veille_opa <- data.frame(id = seq_along(docs_id_opa), doc_id = docs_id_opa)
docs_id_veille_prospectus <- data.frame(id = seq_along(docs_id_prospectus), doc_id = docs_id_prospectus)

#0-6 starting on Sunday
if ((system_date %>% as.character(.) %>% as.POSIXlt(.) %>% .$wday) == 1) {
    #Are we Monday today?
    start_date <-
        format(system_date - 3, "%d/%m/%Y") %>% strsplit(., "/") %>% unlist(.) #last Friday
    end_date <-
        format(system_date, "%d/%m/%Y") %>% strsplit(., "/") %>% unlist(.) #yesterday
} else {
    start_date <-
        format(system_date - 1, "%d/%m/%Y") %>% strsplit(., "/") %>% unlist(.) #yesterday
    end_date <- start_date %>% strsplit(., "/") %>% unlist(.)
}

amf_base_url <- "http://www.amf-france.org"

##sort by descending order
amf_seuils_1 <-
    "http://www.amf-france.org/Resultat-de-recherche-BDIF.html?isSearch=true&DOC_TYPE=BDIF&TEXT=&REFERENCE=&RG_NUM_ARTICLE=&RG_LIVRE=&DATE_PUBLICATION="
amf_seuils_2 <- "%2F"
amf_seuils_3 <- "&DATE_OBSOLESCENCE="
amf_seuils_4 <-
    "&DATE_VIGUEUR_DEBUT=&DATE_VIGUEUR_FIN=&LANGUAGE=fr&INCLUDE_OBSOLESCENT=false&subFormId=sp&BDIF_TYPE_INFORMATION=BDIF_TYPE_INFORMATION_SEUIL_PACTE_DEROGATION&BDIF_RAISON_SOCIALE=&bdifJetonSociete=&BDIF_TYPE_DOCUMENT=&BDIF_TYPE_OPERATION=&BDIF_MARCHE=&BDIF_INSTRUMENT_FINANCIER=&BDIF_NOM_PERSONNE=&ORDER_BY=NOW_TO_OLD"
amf_seuils_url <-
    paste0(
        amf_seuils_1,
        start_date[1],
        amf_seuils_2,
        start_date[2],
        amf_seuils_2,
        start_date[3],
        amf_seuils_3,
        end_date[1],
        amf_seuils_2,
        end_date[2],
        amf_seuils_2,
        end_date[3],
        amf_seuils_4
    )

amf_seuils_session <- html_session(amf_seuils_url)

first_scan <- function(amf_seuils_url) {
    amf_html <<- read_html(amf_seuils_url)
    
    #XPathing into the page source code
    amf_seuils_nodes <<-
        amf_html %>% html_nodes(., xpath = "//div[@id='titre_contenu']/descendant::span[@class='info_resultat']") %>% html_text(.)
}
first_scan(amf_seuils_url)

#fetching numbers to track current location and progress
seuils_numbers <- unlist(strsplit(amf_seuils_nodes, " "))
seuils_documents_number <- as.integer(seuils_numbers[1])
seuils_pages_number <- as.integer(seuils_numbers[4])
seuils_current_page_results_number <- as.integer(seuils_numbers[6])
seuils_current_page_number <- as.integer(seuils_numbers[10])


docs_id_seuils <- c()
if (!(seuils_current_page_results_number <= 0 |
      seuils_documents_number <= 0)) {
    while (!(seuils_current_page_number > seuils_pages_number)) {
        #pages looping
        
        if (seuils_current_page_number != 1)
            #scan new page e.g. page 2
            first_scan(amf_seuils_url)
        
        #fetching links of results
        amf_seuils_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='lien_plus3']") %>% xml_attr(., "href")
        
        #Following all links of results in the page and getting new list of links
        jumpman <-
            lapply(sapply(amf_base_url, paste0, amf_seuils_nodes), function(x)
                jump_to(amf_seuils_session, x)$response$url)
        
        #fetching document names
        amf_seuils_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@name='titreBdif']") %>% html_text(.)
        document_titles <-
            lapply(amf_seuils_nodes, function(x)
                str_extract(x, "[A-Z]{2}.*$"))
        #remove all invalid windows file characters
        document_titles <- str_replace_all(document_titles, "[\\/:*?\"<>_|]", " ") %>% as.list
       
        #fetching document numbers
        amf_seuils_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='informations']/span") %>% html_text(.) %>% .[which(grepl(" AMF", .))]
        document_numbers <-
            sapply(1:length(amf_seuils_nodes), function(x)
                amf_seuils_nodes[x] %>% strsplit(., " "))
        document_numbers <-
            sapply(1:length(document_numbers), function(x)
                document_numbers[[x]][4] %>% sub("\\n.+", "", .))
        
        current_doc_num <- data.frame(id=seq_along(document_numbers), doc_id = document_numbers)
        
        kept_doc_num <- anti_join(current_doc_num, docs_id_veille_seuils, by="doc_id")
        kept_ind <- kept_doc_num$id
        
        
        if(length(kept_ind)>0){
            jumpman <- jumpman[kept_ind]
            document_numbers <- document_numbers[kept_ind]
            document_titles <- document_titles[kept_ind]
            
            #fetching all links of results in the page
            #fetching direct links
            jumpman <-
                sapply(jumpman, function(x)
                    x %>% read_html(.) %>% html_nodes(., xpath = "//div[@class='tabs-container']/descendant::*[@href]") %>% xml_attr(., "href") %>% paste0(amf_base_url, .))
            
            #fix case where more than one pdf are present
            if(is.matrix(jumpman))
                jumpman %<>% .[1,]
            
            #creates the directory
            date_path <- paste0("./data/AMF_16h30/", today, "/seuils")
            #date_path <- paste0("./data/seuils/", today, "")
            if (!isTRUE(file.info(date_path)$isdir)) 
                dir.create(date_path, recursive=TRUE, mode = "0777")
            
            invisible(sapply(1:length(jumpman), function(x)
                download.file(
                    jumpman[[x]][1],
                    paste0(date_path, "/", document_titles[[x]][1], " - ", document_numbers[x], ".pdf"),
                    mode = "wb",
                    quiet = TRUE
                )))
            
        }    
        
        seuils_current_page_number <- seuils_current_page_number + 1
        
        if (seuils_current_page_number <= seuils_pages_number) { #navigating to next the page
            amf_seuils_nodes <-
                amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='suivant']/a[@href]") %>% xml_attr(., "href")
            amf_seuils_url <-
                jump_to(amf_seuils_session, amf_seuils_nodes)$response$url
        }
        docs_id_seuils <- append(docs_id_seuils, document_numbers)
    }# end while seuils
}


##upload to Dropbox
#authentificate
drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
AMFFilesPath <- list.files(path = paste0("./data/AMF_16h30/", today, "/seuils"), pattern ="\\w+\\.pdf$", full.names = TRUE, recursive = TRUE, all.files = FALSE, ignore.case = TRUE)
invisible(sapply(AMFFilesPath, drop_upload, dest = paste0("/AMF_16h30/", today, "/seuils"), overwrite = FALSE))






# ==============================
# from amf_scraper-declaration.R v1.3.5
# ==============================
amf_base_url <- "http://www.amf-france.org"
#rebuilding the link of the results page
##sort by descending order
amf_decla_1 <-
    "http://www.amf-france.org/Resultat-de-recherche-BDIF.html?PAGE_NUMBER=1&BDIF_TYPE_INFORMATION=BDIF_TYPE_INFORMATION_DECLARATION_DIRIGEANT&LANGUAGE=fr&valid_form=Lancer+la+recherche&subFormId=dd&DATE_OBSOLESCENCE="
amf_decla_2 <- "%2F"
amf_decla_3 <- "&DATE_PUBLICATION="
amf_decla_4 <-
    "&BDIF_RAISON_SOCIALE=&REFERENCE=&isSearch=true&formId=BDIF&BDIF_NOM_PERSONNE=&bdifJetonSociete=&DOC_TYPE=BDIF"
amf_decla_url <-
    paste0(
        amf_decla_1,
        end_date[1],
        amf_decla_2,
        end_date[2],
        amf_decla_2,
        end_date[3],
        amf_decla_3,
        start_date[1],
        amf_decla_2,
        start_date[2],
        amf_decla_2,
        start_date[3],
        amf_decla_4
    )

amf_decla_session <- html_session(amf_decla_url)

first_scan <- function(amf_decla_url) {
    amf_html <<- read_html(amf_decla_url)
    
    #XPathing into the page source code
    amf_decla_nodes <<-
        amf_html %>% html_nodes(., xpath = "//div[@id='titre_contenu']/descendant::span[@class='info_resultat']") %>% html_text(.)
}
first_scan(amf_decla_url)

#fetching numbers to track current location and progress
decla_numbers <- unlist(strsplit(amf_decla_nodes, " "))
decla_documents_number <- as.integer(decla_numbers[1])
decla_pages_number <- as.integer(decla_numbers[4])
decla_current_page_results_number <- as.integer(decla_numbers[6])
decla_current_page_number <- as.integer(decla_numbers[10])


docs_id_decla <- c()

if (!(decla_current_page_results_number <= 0 |
      decla_documents_number <= 0)) {
    while (!(decla_current_page_number > decla_pages_number)) {
        #pages looping
        
        if (decla_current_page_number != 1)
            #scan new page e.g. page 2
            first_scan(amf_decla_url)
        
        #fetching links of results
        amf_decla_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='lien_plus3']") %>% xml_attr(., "href")
        
        #Following all links of results in the page and getting new list of links
        jumpman <-
            lapply(sapply(amf_base_url, paste0, amf_decla_nodes), function(x)
                jump_to(amf_decla_session, x)$response$url)
        
        #fetching document names
        amf_decla_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@name='titreBdif']") %>% html_text(.)
        document_titles <-
            lapply(amf_decla_nodes, function(x)
                str_extract(x, "[A-Z]{2}.*$"))
        #remove all invalid windows file characters
        document_titles <- str_replace_all(document_titles, "[\\/:*?\"<>_|]", " ") %>% as.list
        
        #fetching document numbers
        amf_decla_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='informations']/span") %>% html_text(.) %>% .[which(grepl(" AMF", .))]
        document_numbers <-
            sapply(1:length(amf_decla_nodes), function(x)
                amf_decla_nodes[x] %>% strsplit(., " "))
        document_numbers <-
            sapply(1:length(document_numbers), function(x)
                document_numbers[[x]][4] %>% sub("\\n.+", "", .))
        
        
        current_doc_num <- data.frame(id=seq_along(document_numbers), doc_id = document_numbers)
        
        kept_doc_num <- anti_join(current_doc_num, docs_id_veille_decla, by="doc_id")
        kept_ind <- kept_doc_num$id
        
        
        if(length(kept_ind)>0){
            jumpman <- jumpman[kept_ind]
            document_numbers <- document_numbers[kept_ind]
            document_titles <- document_titles[kept_ind]
        # document_numbers <-
        #   sapply(1:length(amf_decla_nodes), function(x)
        #     amf_decla_nodes[x] %>% strsplit(., " ")) %>% sapply(1:length(.), function(x)
        #       .[[x]][4] %>% sub("\\n.+", "", .))
        
        #fetching all links of results in the page
        #fetching direct links
        jumpman <-
            sapply(jumpman, function(x)
                x %>% read_html(.) %>% html_nodes(., xpath = "//div[@class='tabs-container']/descendant::*[@href]") %>% xml_attr(., "href") %>% paste0(amf_base_url, .))
        
        #fix case where more than one pdf are present
        if(is.matrix(jumpman))
            jumpman %<>% .[1,]
        
        #creates the directory
        date_path <- paste0("./data/AMF_16h30/", today, "/declaration")
        #date_path <- paste0("./data/decla/", today, "")
        if (!isTRUE(file.info(date_path)$isdir)) 
            dir.create(date_path, recursive=TRUE)
        
        invisible(sapply(1:length(jumpman), function(x)
            download.file(
                jumpman[[x]][1],
                paste0(date_path, "/", document_titles[[x]][1], " - ", document_numbers[x], ".pdf"),
                mode = "wb",
                quiet = TRUE
            )))
        }
        decla_current_page_number <- decla_current_page_number + 1
        
        if (decla_current_page_number <= decla_pages_number) { #navigating to next the page
            amf_decla_nodes <-
                amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='suivant']/a[@href]") %>% xml_attr(., "href")
            amf_decla_url <-
                jump_to(amf_decla_session, amf_decla_nodes)$response$url
        }
        docs_id_decla <- append(docs_id_decla, document_numbers)
    } # end while decla = declaration des dirigeants
}

##upload to Dropbox
#authentificate
drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
AMFFilesPath <- list.files(path = paste0("./data/AMF_16h30/", today, "/declaration"), pattern ="\\w+\\.pdf$", full.names = TRUE, recursive = TRUE, all.files = FALSE, ignore.case = TRUE)
invisible(sapply(AMFFilesPath, drop_upload, dest = paste0("/AMF_16h30/", today, "/declaration"), overwrite = FALSE))





# ==================
# ==================
# from OPA v1.3
# ==================
# ==================
amf_base_url <- "http://www.amf-france.org"
#rebuilding the link of the results page

##sort by descending order
amf_opa_1 <-
    "http://www.amf-france.org/Resultat-de-recherche-BDIF.html?isSearch=true&DOC_TYPE=BDIF&TEXT=&REFERENCE=&RG_NUM_ARTICLE=&RG_LIVRE=&DATE_PUBLICATION="
amf_opa_2 <- "%2F"
amf_opa_3 <- "&DATE_OBSOLESCENCE="
amf_opa_4 <-
    "&DATE_VIGUEUR_DEBUT=&DATE_VIGUEUR_FIN=&LANGUAGE=fr&INCLUDE_OBSOLESCENT=false&subFormId=op&BDIF_TYPE_INFORMATION=BDIF_TYPE_INFORMATION_OFFRE_PUBLIQUE_ACQUISITION&BDIF_RAISON_SOCIALE=&bdifJetonSociete=&BDIF_TYPE_DOCUMENT=&BDIF_TYPE_OPERATION=&BDIF_MARCHE=&BDIF_INSTRUMENT_FINANCIER=&BDIF_NOM_PERSONNE=&ORDER_BY=NOW_TO_OLD"
amf_opa_url <-
    paste0(
        amf_opa_1,
        start_date[1],
        amf_opa_2,
        start_date[2],
        amf_opa_2,
        start_date[3],
        amf_opa_3,
        end_date[1],
        amf_opa_2,
        end_date[2],
        amf_opa_2,
        end_date[3],
        amf_opa_4
    )

amf_opa_session <- html_session(amf_opa_url)

first_scan <- function(amf_opa_url) {
    amf_html <<- read_html(amf_opa_url)
    
    #XPathing into the page source code
    amf_opa_nodes <<-
        amf_html %>% html_nodes(., xpath = "//div[@id='titre_contenu']/descendant::span[@class='info_resultat']") %>% html_text(.)
}
first_scan(amf_opa_url)

#fetching numbers to track current location and progress
opa_numbers <- unlist(strsplit(amf_opa_nodes, " "))
opa_documents_number <- as.integer(opa_numbers[1])
opa_pages_number <- as.integer(opa_numbers[4])
opa_current_page_results_number <- as.integer(opa_numbers[6])
opa_current_page_number <- as.integer(opa_numbers[10])

docs_id_opa <- c()

if (!(opa_current_page_results_number <= 0 |
      opa_documents_number <= 0)) {
    while (!(opa_current_page_number > opa_pages_number)) {
        #pages looping
        
        if (opa_current_page_number != 1)
            #scan new page e.g. page 2
            first_scan(amf_opa_url)
        
        #fetching links of results
        amf_opa_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='lien_plus3']") %>% xml_attr(., "href")
        
        #Following all links of results in the page and getting new list of links
        jumpman <-
            lapply(sapply(amf_base_url, paste0, amf_opa_nodes), function(x)
                jump_to(amf_opa_session, x)$response$url)
        
        #fetching document names
        amf_opa_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@name='titreBdif']") %>% html_text(.)
        document_titles <-
            lapply(amf_opa_nodes, function(x)
                str_extract(x, "[A-Z]{2}.*$"))
        
        #remove all invalid windows file characters
        document_titles <- str_replace_all(document_titles, "[\\/:*?\"<>_|]", " ") %>% as.list
        
        #fetching document numbers
        amf_opa_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='informations']/span") %>% html_text(.) %>% .[which(grepl(" AMF", .))]
        document_numbers <-
            sapply(1:length(amf_opa_nodes), function(x)
                amf_opa_nodes[x] %>% strsplit(., " "))
        document_numbers <-
            sapply(1:length(document_numbers), function(x)
                document_numbers[[x]][4] %>% sub("\\n.+", "", .))
        
        
        current_doc_num <- data.frame(id=seq_along(document_numbers), doc_id = document_numbers)
        
        kept_doc_num <- anti_join(current_doc_num, docs_id_veille_decla, by="doc_id")
        kept_ind <- kept_doc_num$id
        
        
        if(length(kept_ind)>0){
            jumpman <- jumpman[kept_ind]
            document_numbers <- document_numbers[kept_ind]
            document_titles <- document_titles[kept_ind]
        # document_numbers <-
        #   sapply(1:length(amf_opa_nodes), function(x)
        #     amf_opa_nodes[x] %>% strsplit(., " ")) %>% sapply(1:length(.), function(x)
        #       .[[x]][4] %>% sub("\\n.+", "", .))
        
        #fetching all links of results in the page
        #fetching direct links
        jumpman <-
            sapply(jumpman, function(x)
                x %>% read_html(.) %>% html_nodes(., xpath = "//div[@class='tabs-container']/descendant::*[@href]") %>% xml_attr(., "href") %>% paste0(amf_base_url, .))
        
        #fix case where more than one pdf are present
        if(is.matrix(jumpman))
            jumpman %<>% .[1,]
        
        #creates the directory
        date_path <- paste0("./data/AMF_16h30/", today, "/OPA")
        #date_path <- paste0("./data/opa/", today, "")
        if (!isTRUE(file.info(date_path)$isdir)) 
            dir.create(date_path, recursive=TRUE)
        
        invisible(sapply(1:length(jumpman), function(x)
            download.file(
                jumpman[[x]][1],
                paste0(date_path, "/", document_titles[[x]][1], " - ", document_numbers[x], ".pdf"),
                mode = "wb",
                quiet = TRUE
            )))
        }
        opa_current_page_number <- opa_current_page_number + 1
        
        if (opa_current_page_number <= opa_pages_number) { #navigating to next the page
            amf_opa_nodes <-
                amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='suivant']/a[@href]") %>% xml_attr(., "href")
            amf_opa_url <-
                jump_to(amf_opa_session, amf_opa_nodes)$response$url
        }
        docs_id_opa <- append(docs_id_opa, document_numbers)
    } # end while opa
}

##upload to Dropbox
#authentificate
drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
AMFFilesPath <- list.files(path = paste0("./data/AMF_16h30/", today, "/OPA"), pattern ="\\w+\\.pdf$", full.names = TRUE, recursive = TRUE, all.files = FALSE, ignore.case = TRUE)
invisible(sapply(AMFFilesPath, drop_upload, dest = paste0("/AMF_16h30/", today, "/OPA"), overwrite = FALSE))




# =========================
# =========================
# scrap prospectus
# from v1.9
# =========================
# =========================
amf_base_url <- "http://www.amf-france.org"

##sort by descending order
amf_prospectus_1 <-
    "http://www.amf-france.org/Resultat-de-recherche-BDIF.html?isSearch=true&DOC_TYPE=BDIF&TEXT=&REFERENCE=&RG_NUM_ARTICLE=&RG_LIVRE=&DATE_PUBLICATION="
amf_prospectus_2 <- "%2F"
amf_prospectus_3 <- "&DATE_OBSOLESCENCE="
amf_prospectus_4 <-
    "&DATE_VIGUEUR_DEBUT=&DATE_VIGUEUR_FIN=&LANGUAGE=fr&INCLUDE_OBSOLESCENT=false&subFormId=sp&BDIF_TYPE_INFORMATION=BDIF_TYPE_INFORMATION_SEUIL_PACTE_DEROGATION&BDIF_RAISON_SOCIALE=&bdifJetonSociete=&BDIF_TYPE_DOCUMENT=&BDIF_TYPE_OPERATION=&BDIF_MARCHE=&BDIF_INSTRUMENT_FINANCIER=&BDIF_NOM_PERSONNE=&ORDER_BY=NOW_TO_OLD"
amf_prospectus_url <-
    paste0(
        amf_prospectus_1,
        start_date[1],
        amf_prospectus_2,
        start_date[2],
        amf_prospectus_2,
        start_date[3],
        amf_prospectus_3,
        end_date[1],
        amf_prospectus_2,
        end_date[2],
        amf_prospectus_2,
        end_date[3],
        amf_prospectus_4
    )

amf_prospectus_session <- html_session(amf_prospectus_url)

first_scan <- function(amf_prospectus_url) {
    amf_html <<- read_html(amf_prospectus_url)
    
    #XPathing into the page source code
    amf_prospectus_nodes <<-
        amf_html %>% html_nodes(., xpath = "//div[@id='titre_contenu']/descendant::span[@class='info_resultat']") %>% html_text(.)
}
first_scan(amf_prospectus_url)

#fetching numbers to track current location and progress
prospectus_numbers <- unlist(strsplit(amf_prospectus_nodes, " "))
prospectus_documents_number <- as.integer(prospectus_numbers[1])
prospectus_pages_number <- as.integer(prospectus_numbers[4])
prospectus_current_page_results_number <- as.integer(prospectus_numbers[6])
prospectus_current_page_number <- as.integer(prospectus_numbers[10])


docs_id_prospectus <- c()
if (!(prospectus_current_page_results_number <= 0 |
      prospectus_documents_number <= 0)) {
    while (!(prospectus_current_page_number > prospectus_pages_number)) {
        #pages looping
        
        if (prospectus_current_page_number != 1)
            #scan new page e.g. page 2
            first_scan(amf_prospectus_url)
        
        #fetching links of results
        amf_prospectus_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='lien_plus3']") %>% xml_attr(., "href")
        
        #Following all links of results in the page and getting new list of links
        jumpman <-
            lapply(sapply(amf_base_url, paste0, amf_prospectus_nodes), function(x)
                jump_to(amf_prospectus_session, x)$response$url)
        
        #fetching document names
        amf_prospectus_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@name='titreBdif']") %>% html_text(.)
        document_titles <-
            lapply(amf_prospectus_nodes, function(x)
                str_extract(x, "[A-Z]{2}.*$"))
        #remove all invalid windows file characters
        document_titles <- str_replace_all(document_titles, "[\\/:*?\"<>_|]", " ") %>% as.list
        
        #fetching document numbers
        amf_prospectus_nodes <-
            amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='informations']/span") %>% html_text(.) %>% .[which(grepl(" AMF", .))]
        document_numbers <-
            sapply(1:length(amf_prospectus_nodes), function(x)
                amf_prospectus_nodes[x] %>% strsplit(., " "))
        document_numbers <-
            sapply(1:length(document_numbers), function(x)
                document_numbers[[x]][4] %>% sub("\\n.+", "", .))
        
        current_doc_num <- data.frame(id=seq_along(document_numbers), doc_id = document_numbers)
        
        kept_doc_num <- anti_join(current_doc_num, docs_id_veille_prospectus, by="doc_id")
        kept_ind <- kept_doc_num$id
        
        
        if(length(kept_ind)>0){
            jumpman <- jumpman[kept_ind]
            document_numbers <- document_numbers[kept_ind]
            document_titles <- document_titles[kept_ind]
            
            #fetching all links of results in the page
            #fetching direct links
            jumpman <-
                sapply(jumpman, function(x)
                    x %>% read_html(.) %>% html_nodes(., xpath = "//div[@class='tabs-container']/descendant::*[@href]") %>% xml_attr(., "href") %>% paste0(amf_base_url, .))
            
            #fix case where more than one pdf are present
            if(is.matrix(jumpman))
                jumpman %<>% .[1,]
            
            #creates the directory
            date_path <- paste0("./data/AMF_16h30/", today, "/prospectus")
            #date_path <- paste0("./data/prospectus/", today, "")
            if (!isTRUE(file.info(date_path)$isdir)) 
                dir.create(date_path, recursive=TRUE, mode = "0777")
            
            invisible(sapply(1:length(jumpman), function(x)
                download.file(
                    jumpman[[x]][1],
                    paste0(date_path, "/", document_titles[[x]][1], " - ", document_numbers[x], ".pdf"),
                    mode = "wb",
                    quiet = TRUE
                )))
            
        }    
        
        prospectus_current_page_number <- prospectus_current_page_number + 1
        
        if (prospectus_current_page_number <= prospectus_pages_number) { #navigating to next the page
            amf_prospectus_nodes <-
                amf_html %>% html_nodes(., xpath = "//div[@id='resultatsrecherche']/descendant::*[@class='suivant']/a[@href]") %>% xml_attr(., "href")
            amf_prospectus_url <-
                jump_to(amf_prospectus_session, amf_prospectus_nodes)$response$url
        }
        docs_id_prospectus <- append(docs_id_prospectus, document_numbers)
    }# end while prospectus
}


##upload to Dropbox
#authentificate
drop_auth(new_user = FALSE, key = "zp7o3uxjg4sd67v", secret = "gpb8q7bskcgojt1", cache = TRUE)
AMFFilesPath <- list.files(path = paste0("./data/AMF_16h30/", today, "/prospectus"), pattern ="\\w+\\.pdf$", full.names = TRUE, recursive = TRUE, all.files = FALSE, ignore.case = TRUE)
invisible(sapply(AMFFilesPath, drop_upload, dest = paste0("/AMF_16h30/", today, "/prospectus"), overwrite = FALSE))





save(docs_id_seuils, docs_id_decla, docs_id_opa, docs_id_prospectus, file = paste0("./docs_id/docs_id_",today,".RData"))

# =====================================================
# =====================================================
# we have to do manually conversion with ec2 and winscp
# =====================================================
# =====================================================







