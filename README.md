# reproducible-gully-report
This repository used scripts in the [R language](https://cran.r-project.org/) within the [RStudio](https://rstudio.com/) development environment to create a reproducible [Fisheries and Oceans Canada](https://www.dfo-mpo.gc.ca/) technical report on the [Gully MPA](https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/gully/index-eng.html).

The [renv](https://rstudio.github.io/renv/index.html) package was used to start a clean R environment with nothing but the base R packages.  This package is used to 
manage all of the R packages that were installed so that the environment used to create the Gully technical report was documented so that it can be reused in future years.

The [csasdown](https://github.com/pbs-assess/csasdown) package was the foundational package used for creating the final PDF version of the technical report.

Hopefully the work captured in this repository will be used many times in years to come.
