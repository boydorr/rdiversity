# RDiversity

`RDiversity` is a package for R based around a framework of similarity-sensitive diversity measures. It calculates the diversity of a population and its constituent subcommunities inclusive of similarity between individuals; taxonomic, phenotypic, genetic, phylogenetic, etc. 

## Installation

To install the newest version of RDiversity from github, simply run the following from an R console:
```r
install.packages("devtools")
devtools::install_github("boydorr/RDiversity")
```

## Package tools

Calculate diversity:
* `subcommunity.alpha()` : estimate of naive-community supercommunity diversity 
* `subcommunity.alpha.bar` : similarity-sensitive diversity of subcommunity *j* in isolation
* `subcommunity.rho` : redundancy of subcommunity *j*
* `subcommunity.rho.bar` : representativeness of subcommunity *j*
* `subcommunity.beta` : distinctiveness of subcommunity *j*
* `subcommunity.beta.bar` : estimate of effective number of distinct subcommunities
* `subcommunity.gamma` : contribution per individual toward supercommunity diversity 
* `supercommunity.A` : naive-community supercommunity diversity 
* `supercommunity.A.bar` : average similarity-sensitive diversity of subcommunities 
* `supercommunity.R` : average redundancy of subcommunities
* `supercommunity.R.bar` : average representativeness of subcommunities
* `supercommunity.B` : average distinctiveness of subcommunities
* `supercommunity.B.bar` : effective number of distinct subcommunities 
* `supercommunity.G` : supercommunity similarity-sensitive diversity 