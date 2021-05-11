# reproducible-gully-report
Code for creating a reproducible technical report on the [Gully MPA](https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/gully/index-eng.html).

This repository uses scripts in the R language within RStudio to create a reproducible Fisheries and Oceans Canada Technical Report; in this instance of oceanographic data collected in the Gully MPA.

The [renv](https://rstudio.github.io/renv/index.html) package was used to start a clean R environment with nothing but the base R packages.  This package is used to 
manage all of the R packages that get installed so that the environment used to create the Gully technical report is documented so that it can be reused in future years. 

The [csasdown](https://github.com/pbs-assess/csasdown) package was the foundational package used for creating the final PDF version of the technical report.

Hopefully the work captured in this repository will be used many times in years to come.
