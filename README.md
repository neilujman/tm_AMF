---

title: "projet tm_declaration"
author: "Julien Riton"

---

# tm_declaration
Le programme principal est `main_16h30.R`.
Il est composé aussi du batch `tm_AMF_16h30.bat`

Le programme principal lance successivement :
`scrap_16h30.R`, `convert-layout_16h30.R`, `mine-layout_16h30.R` et `excel_16h30.R`.

## scrap16h30.R
La ligne suivante charge les identifiants des communiqués scrapés la veille :
```
load(file = paste0("./docs_id/docs_id_", veille, ".RData"))
```
On initialise les identifiants du jour à `c()` avec les vecteurs
```
docs_id_seuils
docs_id_decla
docs_id_opa
docs_id_prospectus
```
avant d'entamer la boucle `while` respectives à chacun des dossiers.

Dans chaque boucle `while`:
```
current_doc_num <- data.frame(id=seq_along(document_numbers), doc_id = document_numbers)
kept_doc_num <- anti_join(current_doc_num, docs_id_veille_prospectus, by="doc_id")
kept_ind <- kept_doc_num$id
```