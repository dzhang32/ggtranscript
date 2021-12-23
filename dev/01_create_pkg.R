## ********************
## Create the R package
## ********************

## To get started, install R from https://cran.r-project.org/
## and RStudio Desktop https://rstudio.com/products/rstudio/download/#download
## You can install both of them for free.

## Next, open RStudio as the code that will run benefits from running inside
## RStudio for interactivity purposes.

## Next, you might need to install several R packages that you can install with
## the following code:
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
remotes::install_cran(
    c(
        "available",
        "BiocManager",
        "biocthis",
        "devtools",
        "knitr",
        "pkgdown",
        "RefManageR",
        "rmarkdown",
        "rstudioapi",
        "sessioninfo",
        "styler",
        "usethis"
    )
)
if (!requireNamespace("BiocStyle", quietly = TRUE)) {
    BiocManager::install("BiocStyle")
}
## In case you want the development version of biocthis from GitHub
# BiocManager::install("lcolladotor/biocthis")

## Here's a very quick summary of why these packages are useful:
## * available: to check the name of your package
## * BiocManager: to install Bioconductor packages
## * BiocStyle: for styling your vignette and linking to other packages
## * devtools: to develop R packages
## * knitr: for making your vignette
## * pkgdown: for creating beautiful documentation websites
## * RefManageR: for citing utilities in your package vignette
## * rmarkdown: for making the README.md and processing your vignette
## * remotes: for installing R packages from several locations
## * rstudioapi: for navigating across files in RStudio
## * sessioninfo: for detailed R session information useful to you and your users
## * usethis: for creating templates that will jump start your R package work


## Package names have some properties. You can also use:
available::available("ggtranscript")
## to check that your package name is not taken and that it doesn't have
## a meaning that you might not be aware of.

usethis::create_package("ggtranscript")
## This opens a new window in RStudio

## Note: If you create packages frequently, check the help file for
## usethis::use_description() for more information on how to set some R author
## defaults.

## Add package development files from biocthis
biocthis::use_bioc_pkg_templates()

## Move to the next step: setting up Git and GitHub for your package
rstudioapi::navigateToFile(usethis::proj_path("dev", "02_git_github_setup.R"))

## This template was made using https://lcolladotor.github.io/biocthis/
