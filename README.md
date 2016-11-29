# rdiversity  

[![Build Status](https://travis-ci.org/boydorr/rdiversity.svg?branch=master)](https://travis-ci.org/boydorr/rdiversity)
[![Build status](https://ci.appveyor.com/api/projects/status/463vspjivh08o9x1?svg=true)](https://ci.appveyor.com/project/mysteryduck/rdiversity)
[![Coverage Status](https://coveralls.io/repos/github/boydorr/rdiversity/badge.svg?branch=master)](https://coveralls.io/github/boydorr/rdiversity?branch=master)

`rdiversity` is a package for R based around a framework of similarity-sensitive diversity measures. It calculates the diversity of a population and its constituent subcommunities inclusive of similarity between individuals. Similarity may be defined as taxonomic, phenotypic, genetic, phylogenetic, functional, and so on. 

This package is still in alpha and under heavy development, and so we do not guarantee its correctness, although we are aware of no problems with it. Please raise an issue if you find any problems.

## Installation

To install the latest development version of rdiversity, simply run the following from an R console:

```r
install.packages("devtools")
devtools::install_github("boydorr/rdiversity")
```

## Getting started


Before calculating diversity a `metacommunity` object must be created. This object contains all the information needed to calculate diversity.

```r
# Load the package into R
library(rdiversity)

# Example population
pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))

# Create metacommunity object
meta <- metacommunity(pop)
```

The `metacommunity()` function takes two arguments, `partition` and `similarity`, and creates an object containing:  

* `@type_abundance` : the abundance of types within a population,  
* `@similarity` : the pair-wise similarity of types within a population,  
* `@ordinariness` : the ordinariness of types within a population,  
* `@subcommunity_weights` :  the relative weights of subcommunities within a population,  
* `@type_weights` : the relative weights of types within a population, and  
* `@.Data` : the contents of the partition argument, which is usually the *raw* abundance of types within a population.
    

## Calculating diversity
First we need to calculate the low-level diversity component seperately, by passing a `metacommunity` object to the appropriate function; `raw_alpha()`, `norm_alpha()`, `raw_beta()`, `norm_beta()`, `raw_rho()`, `norm_rho()`, or `raw_gamma()`. 

```r
# First, calculate the normalised subcommunity alpha component
component <- norm_alpha(meta)
```

Afterwhich, `subdiv()` or `metadiv()` are used to calculate subcommunity or metacommunity diversity, respectively (since both subcommunity and metacommunity diversity measures are transformations of the same low-level components, this is computationally more efficient).

```r
# Then, calculate species richness
subdiv(component, 0)

# or the average species richness across the whole population
metadiv(component, 0)

# We can also generate a diversity profile by calculating multiple q-values simultaneously
df <- subdiv(component, 0:30)
rdplot(df)
```

In some instances, it may be useful to calculate **all** subcommunity (or metacommunity) measures. In which case, a `metacommunity` object may be passed directly to `subdiv()` or `metadiv()`:

```r
# To calculate all subcommunity diversity measures
subdiv(meta, 0:2)

# To calculate all metacommunity diversity measures
metadiv(meta, 0:2)
```

Alternatively, if computational efficiency is not an issue, a single measure of diversity may be calculated directly by calling a wrapper function:
```r
norm_sub_alpha(meta,0:2)
```
A complete list of these functions is shown below:

* `raw_sub_alpha()` : estimate of naive-community metacommunity diversity  
* `norm_sub_alpha()` : similarity-sensitive diversity of subcommunity *j* in isolation  
* `raw_sub_rho()` : redundancy of subcommunity *j*  
* `norm_sub_rho()` : representativeness of subcommunity *j*  
* `raw_sub_beta()` : distinctiveness of subcommunity *j*  
* `norm_sub_beta()` : estimate of effective number of distinct subcommunities  
* `raw_sub_gamma()` : contribution per individual toward metacommunity diversity  
* `raw_meta_alpha()` : naive-community metacommunity diversity  
* `norm_meta_alpha()` : average similarity-sensitive diversity of subcommunities  
* `raw_meta_rho()` : average redundancy of subcommunities  
* `norm_meta_rho()` : average representativeness of subcommunities  
* `raw_meta_beta()` : average distinctiveness of subcommunities  
* `norm_meta_beta()` : effective number of distinct subcommunities  
* `meta_gamma()` : metacommunity similarity-sensitive diversity  


## General tools
* `qD_single()` : the Hill number / naive-type diversity of order *q* of a single population  
* `qD()` : the Hill number / naive-type diversity of a series of independent populations for a series of orders  
* `qDZ_single()` : the similarity-sensitive diversity of order *q* of a single population 
* `qDZ()` : the similarity-sensitive diversity of a series of independent populations for a series of orders  
* `similarity_shimatani()`
* `similarity_phylo()`
