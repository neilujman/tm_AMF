---

title: "projet tm_declaration"
author: "Julien Riton"

---

# tm_declaration
Le programme principal est `main_16h30.R`.
Il est composé aussi du batch `tm_AMF_16h30.bat`

Le programme principal lance successivement :

* `scrap_16h30.R`,
* `convert-layout_16h30.R`,
* `mine-layout_16h30.R`,
* `excel_16h30.R`
* `traduction_16h30.R`.

## scrap_16h30.R
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


## convert-layout_16h30.R
On choisit l'option -layout au convertisseur pdf->txt pour des raisons de compatibilité entre Windows et Linux.
Rappelons qu'à terme la solution de text-mining de l'AMF sera hébergée sur EC2.
L'option -layout permet aussi de bien formatter au niveau du txt les tableaux écrit en pdf.
Les fichiers issus de cette conversion seront rangés dans ./text_mining/AMF-layout_16h30.


## mine-layout_16h30.R
Ce script nous construit les data-frames `result_decla`, `result_seuil` et `result_publi`.

---
# TODO
12/12/16:
In `tm_draft_opa.R`, we have just find the graduation for the header of the table of OPA-déclaration text.
We will serve of this graduation (ie the beginning and the head of each column title) in order to find which column
an element of a row table belong to.
Case where we have 5 elements in a row is trivial, just because we have 5 column (we have an ordered bijection).
