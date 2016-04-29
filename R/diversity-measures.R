#' Similarity-sensitive raw subcommunity.alpha
#' 
#' \code{subcommunity.alpha()} is used to calculate an estimate of 
#' naive-community supercommunity diversity.
#' 
#' \code{subcommunity.alpha()} calculates the subcommunity alpha diversity of 
#' a series of columns representing independent subcommunity counts relative 
#' to the supercommunity as a whole (by default the sum of the subcommunities). 
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate subcommunity alpha diversity (takes the power mean)
#' a <- alpha(super)
#' subdiv(a, 0:2)
#' 
subcommunity.alpha <- function(super, qs) 
  subdiv(alpha(super), qs)


#' Similarity-sensitive normalised subcommunity.alpha
#' 
#' \code{subcommunity.alpha.bar()} is used to calculate the 
#' similarity-sensitive diversity of subcommunity \emph{j} in isolation.
#' 
#' \code{subcommunity.alpha.bar()} calculates the normalised subcommunity 
#' alpha diversity of a series of columns representing independent subcommunity 
#' counts relative to the supercommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised subcommunity alpha diversity (takes the power mean)
#' a <- alphabar(super)
#' subdiv(a, 0:2)
#' 
subcommunity.alpha.bar <- function(super, qs) 
  subdiv(alphabar(super), qs)


#' Similarity-sensitive raw subcommunity.beta diversity
#' 
#' \code{subcommunity.beta()} is used to calculate the 
#' distinctiveness of subcommunity \emph{j}.
#' 
#' \code{subcommunity.beta()} calculates the subcommunity beta diversity of a 
#' series of columns representing independent subcommunity counts relative to 
#' the supercommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate subcommunity beta diversity (takes the relative entropy)
#' a <- beta(super)
#' subdiv(a, 0:2)
#' 
subcommunity.beta <- function(super, qs) 
  subdiv(beta(super), qs)


#' Similarity-sensitive normalised subcommunity.beta diversity
#' 
#' \code{subcommunity.beta.bar()} is used to calculate an 
#' estimate of the effective number of distinct subcommunities.
#' 
#' \code{subcommunity.beta.bar()} calculates the normalised subcommunity beta 
#' diversity of a series of columns representing independent subcommunity 
#' counts relative to the supercommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised subcommunity beta diversity (takes the relative 
#' # entropy)
#' a <- betabar(super)
#' subdiv(a, 0:2)
#' 
subcommunity.beta.bar <- function(super, qs) 
  subdiv(betabar(super), qs)


#' Similarity-sensitive raw subcommunity.gamma diversity
#' 
#' \code{subcommunity.gamma()} is used to calculate the 
#' contribution per individual toward supercommunity diversity.
#' 
#' \code{subcommunity.gamma()} calculates the subcommunity gamma diversity of 
#' a series of columns representing independent subcommunity counts relative 
#' to the supercommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate subcommunity gamma diversity (takes the power mean)
#' a <- gamma(super)
#' subdiv(a, 0:2)
#' 
subcommunity.gamma <- function(super, qs) 
  subdiv(gamma(super), qs)


#' Similarity-sensitive raw subcommunity.rho diversity
#' 
#' \code{subcommunity.rho()} is used to calculate the redundancy of 
#' subcommunity \emph{j}.
#' 
#' \code{subcommunity.rho()}, the inverse of \code{subcommunity.beta.bar}, 
#' calculates the subcommunity rho diversity of a series of columns  
#' representing independent subcommunity counts relative to the
#' supercommunity as a whole (by default the sum of the subcommunities). This 
#' measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate subcommunity rho diversity (takes the power mean)
#' a <- rho(super)
#' subdiv(a, 0:2)
#' 
subcommunity.rho <- function(super, qs) 
  subdiv(rho(super), qs)


#' Similarity-sensitive normalised subcommunity.rho diversity
#' 
#' \code{subcommunity.rho.bar()} is used to calculate the 
#' representativeness of subcommunity \emph{j}.
#' 
#' \code{subcommunity.rho.bar()}, the inverse of \code{subcommunity.beta}), 
#' calculates the normalised subcommunity rho diversity of a series of columns 
#' representing independent subcommunities counts relative to a total 
#' supercommunity (by default the sum of the subcommunities), for a series of 
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate noramlised subcommunity rho diversity (takes the power mean)
#' a <- rhobar(super)
#' subdiv(a, 0:2)
#' 
subcommunity.rho.bar <- function(super, qs) 
  subdiv(rhobar(super), qs)


#' Similarity-sensitive raw supercommunity.A diversity
#' 
#' \code{supercommunity.A()} is used to calculate the 
#' naive-community supercommunity diversity.
#' 
#' \code{supercommunity.A()} calculates the total supercommunity alpha  
#' diversity of a series of columns representing independent subcommunity 
#' counts relative to the supercommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate supercommunity alpha diversity (takes the power mean)
#' a <- alpha(super)
#' superdiv(a, 0:2)
#' 
supercommunity.A <- function(super, qs) 
  superdiv(alpha(super), qs)


#' Similarity-sensitive normalised supercommunity.A diversity
#' 
#' \code{supercommunity.A.bar()} is used to calculate the 
#' average similarity-sensitive diversity of subcommunities.
#' 
#' \code{supercommunity.A.bar()} calculates the total normalised supercommunity  
#' alpha diversity of a series of columns representing independent subcommunity 
#' counts relative to the supercommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised supercommunity alpha diversity (takes the power mean)
#' a <- alphabar(super)
#' superdiv(a, 0:2)
#' 
supercommunity.A.bar <- function(super, qs) 
  superdiv(alphabar(super), qs)


#' Similarity-sensitive raw supercommunity.B diversity
#' 
#' \code{supercommunity.B()} is used to calculate the 
#' average distinctiveness of subcommunities.
#' 
#' \code{supercommunity.B()} calculates the total supercommunity beta diversity 
#' of a series of columns representing independent subcommunity counts relative 
#' to the supercommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate supercommunity beta diversity (takes the relative entropy)
#' a <- beta(super)
#' superdiv(a, 0:2)
#' 
supercommunity.B <- function(super, qs) 
  superdiv(beta(super), qs)


#' Similarity-sensitive normalised supercommunity.B diversity
#' 
#' \code{supercommunity.B.bar()} is used to calculate the 
#' effective number of distinct subcommunities.
#' 
#' \code{supercommunity.B.bar()} calculates the total normalised supercommunity 
#' beta diversity of a series of columns representing independent subcommunity 
#' counts relative to the supercommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised supercommunity beta diversity (takes the relative 
#' # entropy)
#' a <- betabar(super)
#' superdiv(a, 0:2)
#' 
supercommunity.B.bar <- function(super, qs) 
  superdiv(betabar(super), qs)


#' Similarity-sensitive raw supercommunity.R diversity
#' 
#' \code{supercommunity.R()} is used to calculate the 
#' average redundancy of subcommunities.
#' 
#' \code{supercommunity.R()} calculates the total supercommunity rho diversity 
#' of a series of columns representing independent subcommunity counts relative 
#' to the supercommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate supercommunity rho diversity (takes the power mean)
#' a <- rho(super)
#' superdiv(a, 0:2)
#' 
supercommunity.R <- function(super, qs) 
  superdiv(rho(super), qs)


#' Similarity-sensitive normalised supercommunity.R diversity
#' 
#' \code{supercommunity.R.bar()} is used to calculate the 
#' average representativeness of subcommunities.
#' 
#' \code{supercommunity.R.bar()} calculates the total noramlised 
#' supercommunity rho diversity of a series of columns representing independent 
#' subcommunity counts relative to the supercommunity as a whole (by default 
#' the sum of the subcommunities). This measure may be calculated for a series 
#' of orders, repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate noramlised supercommunity rho diversity (takes the power mean)
#' a <- rhobar(super)
#' superdiv(a, 0:2)
#' 
supercommunity.R.bar <- function(super, qs) 
  superdiv(rhobar(super), qs)


#' Similarity-sensitive raw supercommunity.G diversity
#' 
#' \code{supercommunity.G()} is used to calculate the 
#' supercommunity similarity-sensitive diversity.
#' 
#' \code{supercommunity.G()} calculates the total supercommunity gamma diversity 
#' of a series of columns representing independent subcommunity counts relative 
#' to the supercommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param super object of class \code{supercommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a two-dimensional \code{matrix} of diversities, with columns  
#' representing subcommunities and rows representing values of \emph{q}.
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate supercommunity gamma diversity (takes the power mean)
#' a <- gamma(super)
#' superdiv(a, 0:2)
#' 
supercommunity.G <- function(super, qs) 
  superdiv(gamma(super), qs)


