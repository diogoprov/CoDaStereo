# Builds the example datasets shipped in inst/extdata from the raw case-study
# sheets of the companion paper (kept outside the repository, in the parent
# project folder). Run from the package root:
#   Rscript data-raw/prepare_examples.R
src <- ".."
rd  <- function(f) utils::read.csv(file.path(src, f), sep = ";", fileEncoding = "UTF-8-BOM",
                                   check.names = FALSE)
site <- function(x) gsub("-", "_", x, fixed = TRUE)
wr  <- function(x, f) utils::write.csv(x, file.path("inst/extdata", f), row.names = FALSE)

s <- rd("spleen_dbp.csv")
wr(data.frame(Specimen   = paste(site(s$Local), s$Animal, sep = "_"),
              Site       = site(s$Local),
              Animal_ID  = s$Animal,
              Field_ID   = s$Foto,
              Sinusoids  = s$Sinusoide,
              Red_pulp   = s$Polpa_vermelha,
              White_pulp = s$Polpa_branca,
              MMCs       = s$MMCs,
              Blood_vessels = s$Vasos_sanguineos),
   "spleen_fields.csv")

k <- rd("kidney_DBP.csv")
wr(data.frame(Specimen  = paste(site(k$Local), k$Animal, sep = "_"),
              Site      = site(k$Local),
              Animal_ID = k$Animal,
              Field_ID  = k$Foto,
              Proximal_tubule    = k$Tubulo_proximal,
              Distal_tubule      = k$Tubulo_distal,
              Renal_corpuscle    = k$Corpusculo_renal,
              Collecting_duct    = k$Ducto_coletor,
              Interstitial_space = k$Espaco_intersticial),
   "kidney_fields.csv")

l <- rd("hemato_Leuco_DBP.csv")
wr(data.frame(Specimen    = paste(site(l$local), l$animal, sep = "_"),
              Species     = l$sp,
              Site        = site(l$local),
              Monocytes   = l$Monocito,
              Lymphocytes = l$Linfocito,
              Neutrophils = l$Neutrofilo,
              Eosinophils = l$Eosinofilo),
   "leukocytes.csv")

a <- rd("hemato_AN_DBP.csv")
wr(data.frame(Specimen = paste(site(a$local), a$animal, sep = "_"),
              Species  = a$sp,
              Site     = site(a$local),
              Lobed    = a$Lobado,
              Budded   = a$Bud,
              Notched  = a$Entalhada,
              Anucleate = a$AN,  # "AN" in the original sheet: anucleate erythrocytes
              Kidney_shaped = a$Reniforme,
              Abnormal = a$Total_anormalidades,
              Normal   = a$Total_cell - a$Total_anormalidades),
   "nuclear_abnormalities.csv")
