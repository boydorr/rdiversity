# A minimal rdiversity example
  
`rdiversity` is a package for R based around a framework of similarity-sensitive diversity measures. It calculates the diversity of a population and its constituent subcommunities inclusive of similarity between individuals; taxonomic, phenotypic, genetic, phylogenetic, etc. 

### Installation

To install the newest version of rdiversity from github, simply run the following from an R console:
```r
install.packages("devtools")
devtools::install_github("boydorr/rdiversity")
```

### Getting started

The functions within `rdiversity` may be accessed in two main ways. 

Before calculating diversity a supercommunity object must be created

```r
# Example population
pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))

# Create supercommunity object
super <- supercommunity(pop)
str(super)
```

supercommunity() creates an object containing:  

* `super@type_abundance` : the abundance of types within a population,  
* `super@similarity` : the pair-wise similarity of types within a population,  
* `super@ordinariness` : the ordinariness of types within a population,  
* `super@subcommunity_weights` :  the relative weights of subcommunities within a population,  
* `super@type_weighst` : the relative weights of types within a population,  
* `super@.Data` : the *raw* abundance of types within a population; which may be  
    + count data, in which case `@type_abundance` is then normalised to sum to 1;  
    + relative abundance data, such that `@type_abundance` is numerically identical; or  
    + count/relative abundance data of present-day samples in a phylogeny, where `@type_abundance` is then calculated for all historic species within the evolutionary history of that phylogeny.  

At this point, we may select which measure we wish to calculate. Calculating first (for computational reasons) the low-level component of diversity, which may then be transformed into subcommunity or supercommunity diversity. 

```r
# Calculate normalised subcommunity alpha diversity (takes the power mean)
a <- alphabar(super)

# Calculate normalised subcommunity alpha 
subdiv(a, 0:2)

# Calculate normalised supercommunity alpha
superdiv(a, 0:2)
```

Low-level components of diversity are:
* `alpha()` :  
* `alphabar()` :  
* `beta()` :  
* `betabar()` :  
* `rho()` :  
* `rhobar()` :  
* `gamma()` :  

Alternatively, subcommunity or supercommunity diversity may be calculated directly using the following wrappers:

**Diversity framework**  
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


**General tools**  
* `similarity_matrix()` :  
* `similarity_shimatani()` :  
* `qD.single()` : the Hill number / naive-type diversity of order *q* of a single population  
* `qD()` : the Hill number / naive-type diversity of a series of independent populations for a series of orders  
* `qDZ.single()` : the similarity-sensitive diversity of order *q* of a single population  
* `qDZ()` : the similarity-sensitive diversity of a series of independent populations for a series of orders  







