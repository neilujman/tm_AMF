setwd("C:\\Users\\jriton\\Documents\\R\\MesProjets\\AMF_textmining\\tm_declaration")


if(!((system_date %>% as.character(.) %>% as.POSIXlt(.) %>% .$wday) %in% c(0,6))) {
    source(file = "scrap_16h30.R", encoding = "UTF-8")
    source(file = "convert-layout_16h30.R", encoding = "UTF-8")
    source(file = "mine-layout_16h30.R", encoding = "UTF-8")
    source(file = "excel_16h30.R", encoding = "UTF-8")
    source(file = "traduction_16h30.R", encoding = "UTF-8")
    source(file = "mailing/mailing_english.R", encoding="utf-8")
}