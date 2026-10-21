# Regenerate manifest.json for Posit Connect Cloud, listing only the files the
# deployed app needs. Run from the package root: Rscript dev/write_manifest.R
f <- system("git ls-files", intern = TRUE)
f <- f[grepl("^(app[.]R|DESCRIPTION|NAMESPACE|LICENSE|R/|www/|inst/)", f)]
rsconnect::writeManifest(appDir = ".", appFiles = f, appPrimaryDoc = "app.R", appMode = "shiny")
