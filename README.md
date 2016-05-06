# A minimal rdiversity example
  
`rdiversity` is a package for R based around a framework of similarity-sensitive diversity measures. It calculates the diversity of a population and its constituent subcommunities inclusive of similarity between individuals; taxonomic, phenotypic, genetic, phylogenetic, etc. 

### Installation

To install the newest version of rdiversity from github, simply run the following from an R console:
```r
install.packages("devtools")
devtools::install_github("boydorr/rdiversity")
```

## Getting started


Before calculating diversity a supercommunity object must be created:

```r
# Example population
pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))

# Create supercommunity object
super <- supercommunity(pop)
str(super)
```

The `supercommunity()` function creates an object containing:  

* `@type_abundance` : the abundance of types within a population,  
* `@similarity` : the pair-wise similarity of types within a population,  
* `@ordinariness` : the ordinariness of types within a population,  
* `@subcommunity_weights` :  the relative weights of subcommunities within a population,  
* `@type_weighst` : the relative weights of types within a population,  
* `@.Data` : the *raw* abundance of types within a population; which may be  
    + count data, in which case `@type_abundance` is then normalised to sum to 1;  
    + relative abundance data, such that `@type_abundance` is numerically identical; or  
    + count/relative abundance data of present-day samples in a phylogeny, where `@type_abundance` is then calculated for all historic species within the evolutionary history of that phylogeny. 
    
The supercommunity object contains all the information needed to calculate diversity.
    
##Calculating diversity

The functions within `rdiversity` can be accessed in two main ways. 

###1. Two part method (slightly more computationally efficient)
For large datasets (or simply to run faster code), it makes sense to calculate diversity in two steps. 

Before explaining these steps, it is important to note that both subcommunity and supercommunity  alpha diversity are transformations of the same low-level alpha component. The same is true for normalised alpha, beta, normalised beta, rho, normalised rho, and gamma.

It makes sense then, to calculate the low-level diversity component seperately, by passing a supercommunity object to the appropriate function; `alpha()`, `alphabar()`, `beta()`, `betabar()`, `rho()`, `rhobar()`, or `gamma()`. Afterwhich `subdiv()` or `superdiv()` can be called to calculate subcommunity or supercommunity diversity, respectively.

```r
# First, calculate the normalised subcommunity alpha component
a <- alphabar(super)

# Then, calculate normalised subcommunity alpha 
subdiv(a, 0:2)

# or normalised supercommunity alpha
superdiv(a, 0:2)
```

Alternatively, to calculate all subcommunity (or supercommunity) measures a supercommunity object is passed directly:

```r
# To calculate all subcommunity diversity measures
subdiv(super)

# To calculate all supercommunity diversity measures
superdiv(super)
```

###2. Direct method
Simply, to calculate normalised subcommunity alpha diversity, a supercommunity object is passed to a wrapper function with a suitable *q* value (or range of values):
```r
subcommunity.alpha.bar(super,0)
```
And to calculate normalised supercommunity alpha:
```r 
supercommunity.alpha.bar(super,0:2)
```
A complete list of these functions is shown below:

* `subcommunity.alpha()` : estimate of naive-community supercommunity diversity  
* `subcommunity.alpha.bar()` : similarity-sensitive diversity of subcommunity *j* in isolation  
* `subcommunity.rho()` : redundancy of subcommunity *j*  
* `subcommunity.rho.bar()` : representativeness of subcommunity *j*  
* `subcommunity.beta()` : distinctiveness of subcommunity *j*  
* `subcommunity.beta.bar()` : estimate of effective number of distinct subcommunities  
* `subcommunity.gamma()` : contribution per individual toward supercommunity diversity  
* `supercommunity.A()` : naive-community supercommunity diversity  
* `supercommunity.A.bar()` : average similarity-sensitive diversity of subcommunities  
* `supercommunity.R()` : average redundancy of subcommunities  
* `supercommunity.R.bar()` : average representativeness of subcommunities  
* `supercommunity.B()` : average distinctiveness of subcommunities  
* `supercommunity.B.bar()` : effective number of distinct subcommunities  
* `supercommunity.G()` : supercommunity similarity-sensitive diversity  


##General tools
* `similarity_matrix()` :  
* `similarity_shimatani()` :  
* `similarity_phylo()` : 
* `qD.single()` : the Hill number / naive-type diversity of order *q* of a single population  
* `qD()` : the Hill number / naive-type diversity of a series of independent populations for a series of orders  
* `qDZ.single()` : the similarity-sensitive diversity of order *q* of a single population  
* `qDZ()` : the similarity-sensitive diversity of a series of independent populations for a series of orders  







