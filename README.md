# rdiversity  

[![Build Status](https://travis-ci.org/boydorr/rdiversity.svg?branch=master)](https://travis-ci.org/boydorr/rdiversity)
[![Coverage Status](https://coveralls.io/repos/github/boydorr/rdiversity/badge.svg?branch=master)](https://coveralls.io/github/boydorr/rdiversity?branch=master)

`rdiversity` is a package for R based around a framework of similarity-sensitive diversity measures. It calculates the diversity of a population and its constituent subcommunities inclusive of similarity between individuals. Similarity may be defined as taxonomic, phenotypic, genetic, phylogenetic, functional, and so on. 

## Installation

To install the latest development version of rdiversity, simply run the following from an R console:

```r
install.packages("devtools")
devtools::install_github("boydorr/rdiversity")
```

## Getting started


Before calculating diversity a 'supercommunity' object must be created. This object contains all the information needed to calculate diversity.

```r
# Example population
pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))

# Create supercommunity object
super <- supercommunity(pop)
```

The `supercommunity()` function takes two arguments, `partition` and `similarity`, and creates an object containing:  

* `@type_abundance` : the abundance of types within a population,  
* `@similarity` : the pair-wise similarity of types within a population,  
* `@ordinariness` : the ordinariness of types within a population,  
* `@subcommunity_weights` :  the relative weights of subcommunities within a population,  
* `@type_weights` : the relative weights of types within a population, and  
* `@.Data` : the contents of the partition argument, which is usually the *raw* abundance of types within a population.
    

## Calculating diversity
First we need to calculate the low-level diversity component seperately, by passing a supercommunity object to the appropriate function; `raw.alpha()`, `normalised.alpha()`, `raw.beta()`, `normalised.beta()`, `raw.rho()`, `normalised.rho()`, or `raw.gamma()`. 

```r
# First, calculate the normalised subcommunity alpha component
component <- normalised.alpha(super)
```

Afterwhich, `subdiv()` or `superdiv()` are used to calculate subcommunity or supercommunity diversity, respectively (since both subcommunity and supercommunity diversity measures are transformations of the same low-level components, this is computationally more efficient).

```r
# Then, calculate species richness
subdiv(component, 0)

# or the average species richness across the whole population
superdiv(component, 0)

# We can also generate a diversity profile by calculating multiple q-values simultaneously
df <- subdiv(component, 0:30)
rdplot(df)
```

Alternatively, to calculate **all** subcommunity (or supercommunity) measures, a supercommunity object may be passed directly:

```r
# To calculate all subcommunity diversity measures
subdiv(super, 0:2)

# To calculate all supercommunity diversity measures
superdiv(super, 0:2)
```


Alternatively, a single measure of diversity may be calculated by calling a wrapper function:
```r
normalised.subcommunity.alpha(super,0:2)
```
A complete list of these functions is shown below:

* `raw.subcommunity.alpha()` : estimate of naive-community supercommunity diversity  
* `normalised.subcommunity.alpha()` : similarity-sensitive diversity of subcommunity *j* in isolation  
* `raw.subcommunity.rho()` : redundancy of subcommunity *j*  
* `normalised.subcommunity.rho()` : representativeness of subcommunity *j*  
* `raw.subcommunity.beta()` : distinctiveness of subcommunity *j*  
* `normalised.subcommunity.beta()` : estimate of effective number of distinct subcommunities  
* `raw.subcommunity.gamma()` : contribution per individual toward supercommunity diversity  
* `raw.supercommunity.alpha()` : naive-community supercommunity diversity  
* `normalised.supercommunity.alpha()` : average similarity-sensitive diversity of subcommunities  
* `raw.supercommunity.rho()` : average redundancy of subcommunities  
* `normalised.supercommunity.rho()` : average representativeness of subcommunities  
* `raw.supercommunity.beta()` : average distinctiveness of subcommunities  
* `normalised.supercommunity.beta()` : effective number of distinct subcommunities  
* `raw.supercommunity.gamma()` : supercommunity similarity-sensitive diversity  


## General tools
* `similarity_matrix()` :  
* `similarity_shimatani()` :  
* `similarity_phylo()` : 
* `qD.single()` : the Hill number / naive-type diversity of order *q* of a single population  
* `qD()` : the Hill number / naive-type diversity of a series of independent populations for a series of orders  
* `qDZ.single()` : the similarity-sensitive diversity of order *q* of a single population  
* `qDZ()` : the similarity-sensitive diversity of a series of independent populations for a series of orders  







