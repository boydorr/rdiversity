# rdiversity: diversity measurement in R 
[![](https://img.shields.io/badge/docs-rdiversity-blue)](https://boydorr.github.io/rdiversity/)
[![test-build](https://github.com/boydorr/rdiversity/workflows/build/badge.svg)](https://github.com/boydorr/rdiversity/actions)
[![codecov](https://codecov.io/gh/boydorr/rdiversity/branch/master/graph/badge.svg)](https://codecov.io/gh/boydorr/rdiversity)
[![CodeFactor](https://www.codefactor.io/repository/github/boydorr/rdiversity/badge)](https://www.codefactor.io/repository/github/boydorr/rdiversity)
[![License: GPL-3.0](https://img.shields.io/badge/licence-GPL--3-yellow)](https://opensource.org/licenses/GPL-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.597470.svg)](https://doi.org/10.5281/zenodo.597470)
[![Total downloads](http://cranlogs.r-pkg.org/badges/grand-total/rdiversity?color=yellow)](http://cranlogs.r-pkg.org/badges/grand-total/rdiversity)

`rdiversity` is a package for R based around a framework for measuring biodiversity using similarity-sensitive diversity measures. It provides functionality for measuring alpha, beta and gamma diversity of metacommunities (*e.g.* ecosystems) and their constituent subcommunities, where similarity may be defined as taxonomic, phenotypic, genetic, phylogenetic, functional, and so on. It uses the diversity framework described in the arXiv paper [arXiv:1404.6520 (q-bio.QM)](https://arxiv.org/abs/1404.6520), "How to partition diversity". 

This package has now reached a stable release and is cross-validated against our Julia package [Diversity.jl](https://github.com/richardreeve/Diversity.jl), which is developed independently. Please [raise an issue](https://github.com/boydorr/rdiversity/issues) if you find any problems.

To install rdiversity from CRAN, simply run the following from an R console:

```{r}
install.packages("rdiversity")
```
