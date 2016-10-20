#' Similarity-sensitive raw subcommunity alpha
#' 
#' \code{raw.subcommunity.alpha()} is used to calculate an estimate of 
#' naive-community metacommunity diversity.
#' 
#' \code{raw.subcommunity.alpha()} calculates the subcommunity alpha diversity of 
#' a series of columns representing independent subcommunity counts relative 
#' to the metacommunity as a whole (by default the sum of the subcommunities). 
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
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
#' counts relative to the metacommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
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
#' the metacommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate raw subcommunity beta diversity
#' b <- raw.beta(super)
#' subdiv(b, 0:2)
#' raw.subcommunity.beta(super, 0:2)
#' 
raw.subcommunity.beta <- function(super, qs) 
  subdiv(raw.beta(super), qs)


#' Similarity-sensitive normalised subcommunity beta diversity
#' 
#' \code{normalised.subcommunity.beta()} is used to calculate an 
#' estimate of the effective number of distinct subcommunities.
#' 
#' \code{normalised.subcommunity.beta()} calculates the normalised subcommunity beta 
#' diversity of a series of columns representing independent subcommunity 
#' counts relative to the metacommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
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
#' metacommunity as a whole (by default the sum of the subcommunities). This 
#' measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
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
#' metacommunity (by default the sum of the subcommunities), for a series of 
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
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
#' contribution per individual toward metacommunity diversity.
#' 
#' \code{subcommunity.gamma()} calculates the subcommunity gamma diversity of 
#' a series of columns representing independent subcommunity counts relative 
#' to the metacommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate subcommunity gamma diversity
#' g <- gamma(super)
#' subdiv(g, 0:2)
#' subcommunity.gamma(super, 0:2)
#' 
subcommunity.gamma <- function(super, qs) 
  subdiv(raw.gamma(super), qs)
raw.subcommunity.gamma <- subcommunity.gamma


#' Similarity-sensitive raw metacommunity alpha diversity
#' 
#' \code{raw.metacommunity.alpha()} is used to calculate the 
#' naive-community metacommunity diversity.
#' 
#' \code{raw.metacommunity.alpha()} calculates the total metacommunity alpha  
#' diversity of a series of columns representing independent subcommunity 
#' counts relative to the metacommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate raw metacommunity alpha diversity
#' a <- raw.alpha(super)
#' metadiv(a, 0:2)
#' raw.metacommunity.alpha(super, 0:2)
#' 
raw.metacommunity.alpha <- function(super, qs) 
  metadiv(raw.alpha(super), qs)


#' Similarity-sensitive normalised metacommunity alpha diversity
#' 
#' \code{normalised.metacommunity.alpha()} is used to calculate the 
#' average similarity-sensitive diversity of subcommunities.
#' 
#' \code{normalised.metacommunity.alpha()} calculates the total
#' normalised metacommunity alpha diversity of a series of columns
#' representing independent subcommunity counts relative to the
#' metacommunity as a whole (by default the sum of the
#' subcommunities). This measure may be calculated for a series of
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate normalised metacommunity alpha diversity
#' a <- normalised.alpha(super)
#' metadiv(a, 0:2)
#' normalised.metacommunity.alpha(super, 0:2)
#' 
normalised.metacommunity.alpha <- function(super, qs) 
  metadiv(normalised.alpha(super), qs)


#' Similarity-sensitive raw metacommunity beta diversity
#' 
#' \code{raw.metacommunity.beta()} is used to calculate the 
#' average distinctiveness of subcommunities.
#' 
#' \code{raw.metacommunity.beta()} calculates the total
#' metacommunity beta diversity of a series of columns representing
#' independent subcommunity counts relative to the metacommunity as a
#' whole (by default the sum of the subcommunities). This measure may
#' be calculated for a series of orders, repesented as a vector of
#' \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate raw metacommunity beta diversity
#' b <- raw.beta(super)
#' metadiv(b, 0:2)
#' raw.metacommunity.beta(super, 0:2)
#' 
raw.metacommunity.beta <- function(super, qs) 
  metadiv(raw.beta(super), qs)


#' Similarity-sensitive normalised metacommunity beta diversity
#' 
#' \code{normalised.metacommunity.beta()} is used to calculate the 
#' effective number of distinct subcommunities.
#' 
#' \code{normalised.metacommunity.beta()} calculates the total
#' normalised metacommunity beta diversity of a series of columns
#' representing independent subcommunity counts relative to the
#' metacommunity as a whole (by default the sum of the
#' subcommunities). This measure may be calculated for a series of
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate normalised metacommunity beta diversity
#' b <- normalised.beta(super)
#' metadiv(b, 0:2)
#' normalised.metacommunity.beta(super, 0:2)
#' 
normalised.metacommunity.beta <- function(super, qs) 
  metadiv(normalised.beta(super), qs)


#' Similarity-sensitive raw metacommunity rho diversity
#' 
#' \code{raw.metacommunity.rho()} is used to calculate the 
#' average redundancy of subcommunities.
#' 
#' \code{raw.metacommunity.rho()} calculates the total metacommunity
#' rho diversity of a series of columns representing independent
#' subcommunity counts relative to the metacommunity as a whole (by
#' default the sum of the subcommunities). This measure may be
#' calculated for a series of orders, repesented as a vector of
#' \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate metacommunity rho diversity
#' r <- raw.rho(super)
#' metadiv(r, 0:2)
#' raw.metacommunity.rho(super, 0:2)
#' 
raw.metacommunity.rho <- function(super, qs) 
  metadiv(raw.rho(super), qs)


#' Similarity-sensitive normalised metacommunity rho diversity
#' 
#' \code{normalised.metacommunity.rho()} is used to calculate the 
#' average representativeness of subcommunities.
#' 
#' \code{normalised.metacommunity.rho()} calculates the total normalised 
#' metacommunity rho diversity of a series of columns representing independent 
#' subcommunity counts relative to the metacommunity as a whole (by default 
#' the sum of the subcommunities). This measure may be calculated for a series 
#' of orders, repesented as a vector of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate normalised metacommunity rho diversity
#' r <- normalised.rho(super)
#' metadiv(r, 0:2)
#' normalised.metacommunity.rho(super, 0:2)
#' 
normalised.metacommunity.rho <- function(super, qs) 
  metadiv(normalised.rho(super), qs)


#' Similarity-sensitive metacommunity gamma diversity
#' 
#' \code{metacommunity.gamma()} is used to calculate the 
#' metacommunity similarity-sensitive diversity.
#' 
#' \code{metacommunity.gamma()} calculates the total metacommunity gamma diversity 
#' of a series of columns representing independent subcommunity counts relative 
#' to the metacommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param super object of class \code{metacommunity}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity, community, or metacommunity), and \code{measure} 
#' (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- metacommunity(pop)
#' 
#' # Calculate metacommunity gamma diversity
#' g <- gamma(super)
#' metadiv(g, 0:2)
#' metacommunity.gamma(super, 0:2)
#' 
metacommunity.gamma <- function(super, qs) 
  metadiv(raw.gamma(super), qs)
raw.metacommunity.gamma <- metacommunity.gamma
