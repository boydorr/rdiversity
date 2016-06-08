#' Similarity-sensitive raw subcommunity alpha
#' 
#' \code{raw.subcommunity.alpha()} is used to calculate an estimate of 
#' naive-community supercommunity diversity.
#' 
#' \code{raw.subcommunity.alpha()} calculates the subcommunity alpha diversity of 
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
#' # Calculate raw subcommunity alpha diversity
#' a <- raw.alpha(super)
#' subdiv(a, 0:2)
#' 
raw.subcommunity.alpha <- function(super, qs) 
  subdiv(raw.alpha(super), qs)


#' Similarity-sensitive normalised subcommunity alpha
#' 
#' \code{normalised.subcommunity.alpha()} is used to calculate the 
#' similarity-sensitive diversity of subcommunity \emph{j} in isolation.
#' 
#' \code{normalised.subcommunity.alpha()} calculates the normalised subcommunity 
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
#' # Calculate normalised subcommunity alpha diversity
#' a <- normalised.alpha(super)
#' subdiv(a, 0:2)
#' normalised.subcommunity.alpha(super, 0:2)
#' 
normalised.subcommunity.alpha <- function(super, qs) 
  subdiv(normalised.alpha(super), qs)


#' Similarity-sensitive raw subcommunity beta diversity
#' 
#' \code{raw.subcommunity.beta()} is used to calculate the 
#' distinctiveness of subcommunity \emph{j}.
#' 
#' \code{raw.subcommunity.beta()} calculates the subcommunity beta diversity of a 
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
#' # Calculate raw subcommunity beta diversity
#' b <- beta(super)
#' subdiv(b, 0:2)
#' raw.subcommunity.beta(super, 0:2)
#' 
subcommunity.beta <- function(super, qs) 
  subdiv(raw.beta(super), qs)


#' Similarity-sensitive normalised subcommunity beta diversity
#' 
#' \code{normalised.subcommunity.beta()} is used to calculate an 
#' estimate of the effective number of distinct subcommunities.
#' 
#' \code{normalised.subcommunity.beta()} calculates the normalised subcommunity beta 
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
#' # Calculate normalised subcommunity beta diversity
#' b <- normalised.beta(super)
#' subdiv(b, 0:2)
#' normalised.subcommunity.beta(super, 0:2)
#' 
normalised.subcommunity.beta <- function(super, qs) 
  subdiv(normalised.beta(super), qs)


#' Similarity-sensitive raw subcommunity rho diversity
#' 
#' \code{raw.subcommunity.rho()} is used to calculate the redundancy of 
#' subcommunity \emph{j}.
#' 
#' \code{raw.subcommunity.rho()}, the inverse of \code{raw.subcommunity.beta()}, 
#' calculates the raw subcommunity rho diversity of a series of columns  
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
#' # Calculate raw subcommunity rho diversity
#' r <- raw.rho(super)
#' subdiv(r, 0:2)
#' raw.subcommunity.rho(super, 0:2)
#' 
raw.subcommunity.rho <- function(super, qs) 
  subdiv(raw.rho(super), qs)


#' Similarity-sensitive normalised subcommunity rho diversity
#' 
#' \code{normalised.subcommunity.rho()} is used to calculate the 
#' representativeness of subcommunity \emph{j}.
#' 
#' \code{normalised.subcommunity.rho()}, the inverse of \code{subcommunity.beta}), 
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
#' # Calculate normalised subcommunity rho diversity
#' r <- normalised.rho(super)
#' subdiv(r, 0:2)
#' normalised.subcommunity.rho(super, 0:2)
#' 
normalised.subcommunity.rho <- function(super, qs) 
  subdiv(normalised.rho(super), qs)


#' Similarity-sensitive subcommunity gamma diversity
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
#' # Calculate subcommunity gamma diversity
#' g <- gamma(super)
#' subdiv(g, 0:2)
#' subcommunity.gamma(super, 0:2)
#' 
subcommunity.gamma <- function(super, qs) 
  subdiv(raw.gamma(super), qs)
raw.subcommunity.gamma <- subcommunity.gamma


#' Similarity-sensitive raw supercommunity alpha diversity
#' 
#' \code{raw.supercommunity.alpha()} is used to calculate the 
#' naive-community supercommunity diversity.
#' 
#' \code{raw.supercommunity.alpha()} calculates the total supercommunity alpha  
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
#' # Calculate raw supercommunity alpha diversity
#' a <- raw.alpha(super)
#' superdiv(a, 0:2)
#' raw.supercommunity.alpha(super, 0:2)
#' 
raw.supercommunity.alpha <- function(super, qs) 
  superdiv(raw.alpha(super), qs)


#' Similarity-sensitive normalised supercommunity alpha diversity
#' 
#' \code{normalised.supercommunity.alpha()} is used to calculate the 
#' average similarity-sensitive diversity of subcommunities.
#' 
#' \code{normalised.supercommunity.alpha()} calculates the total
#' normalised supercommunity alpha diversity of a series of columns
#' representing independent subcommunity counts relative to the
#' supercommunity as a whole (by default the sum of the
#' subcommunities). This measure may be calculated for a series of
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
#' # Calculate normalised supercommunity alpha diversity
#' a <- normalised.alpha(super)
#' superdiv(a, 0:2)
#' normalised.supercommunity.alpha(super, 0:2)
#' 
normalised.supercommunity.alpha <- function(super, qs) 
  superdiv(normalised.alpha(super), qs)


#' Similarity-sensitive raw supercommunity beta diversity
#' 
#' \code{raw.supercommunity.beta()} is used to calculate the 
#' average distinctiveness of subcommunities.
#' 
#' \code{raw.supercommunity.beta()} calculates the total
#' supercommunity beta diversity of a series of columns representing
#' independent subcommunity counts relative to the supercommunity as a
#' whole (by default the sum of the subcommunities). This measure may
#' be calculated for a series of orders, repesented as a vector of
#' \code{qs}.
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
#' # Calculate raw supercommunity beta diversity
#' b <- beta(super)
#' superdiv(b, 0:2)
#' raw.supercommunity.beta(super, 0:2)
#' 
raw.supercommunity.beta <- function(super, qs) 
  superdiv(raw.beta(super), qs)


#' Similarity-sensitive normalised supercommunity beta diversity
#' 
#' \code{normalised.supercommunity.beta()} is used to calculate the 
#' effective number of distinct subcommunities.
#' 
#' \code{normalised.supercommunity.beta()} calculates the total
#' normalised supercommunity beta diversity of a series of columns
#' representing independent subcommunity counts relative to the
#' supercommunity as a whole (by default the sum of the
#' subcommunities). This measure may be calculated for a series of
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
#' # Calculate normalised supercommunity beta diversity
#' b <- normalised.beta(super)
#' superdiv(b, 0:2)
#' normalised.supercommunity.beta(super, 0:2)
#' 
normalised.supercommunity.beta <- function(super, qs) 
  superdiv(normalised.beta(super), qs)


#' Similarity-sensitive raw supercommunity rho diversity
#' 
#' \code{raw.supercommunity.rho()} is used to calculate the 
#' average redundancy of subcommunities.
#' 
#' \code{raw.supercommunity.rho()} calculates the total supercommunity
#' rho diversity of a series of columns representing independent
#' subcommunity counts relative to the supercommunity as a whole (by
#' default the sum of the subcommunities). This measure may be
#' calculated for a series of orders, repesented as a vector of
#' \code{qs}.
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
#' # Calculate supercommunity rho diversity
#' r <- raw.rho(super)
#' superdiv(r, 0:2)
#' raw.supercommunity.rho(super, 0:2)
#' 
raw.supercommunity.rho <- function(super, qs) 
  superdiv(raw.rho(super), qs)


#' Similarity-sensitive normalised supercommunity rho diversity
#' 
#' \code{normalised.supercommunity.rho()} is used to calculate the 
#' average representativeness of subcommunities.
#' 
#' \code{normalised.supercommunity.rho()} calculates the total normalised 
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
#' # Calculate normalised supercommunity rho diversity
#' r <- normalised.rho(super)
#' superdiv(r, 0:2)
#' normalised.supercommunity.rho(super, 0:2)
#' 
normalised.supercommunity.rho <- function(super, qs) 
  superdiv(normalised.rho(super), qs)


#' Similarity-sensitive supercommunity gamma diversity
#' 
#' \code{supercommunity.gamma()} is used to calculate the 
#' supercommunity similarity-sensitive diversity.
#' 
#' \code{supercommunity.gamma()} calculates the total supercommunity gamma diversity 
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
#' # Calculate supercommunity gamma diversity
#' g <- gamma(super)
#' superdiv(g, 0:2)
#' supercommunity.gamma(super, 0:2)
#' 
supercommunity.gamma <- function(super, qs) 
  superdiv(raw.gamma(super), qs)
raw.supercommunity.gamma <- supercommunity.gamma
