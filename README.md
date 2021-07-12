# reproducible-gully-report
This repository houses scripts in the [R language](https://cran.r-project.org/) developed in the [RStudio](https://rstudio.com/) development environment to create a reproducible [Fisheries and Oceans Canada](https://www.dfo-mpo.gc.ca/) technical report on oceanographic monitoring data taken from the [Gully MPA](https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/gully/index-eng.html).

The [renv](https://rstudio.github.io/renv/index.html) package was used to start a clean R environment with nothing but the base R packages and manage all of the additional R packages required to create the technical report.

The [csasdown](https://github.com/pbs-assess/csasdown) package was the key package used for creating the final PDF version of the technical report.  It is built upon other R packages; especially [bookdown](https://bookdown.org/).

Hopefully the work captured in this repository will be a solid foundation for reproducing similar technical reports in the future.

To easily reproduce the report in this repository, you must have R and RStudio installed.

Download the code and open the GullyReproducibleReport.Rproj file, which is the RStudio project.
