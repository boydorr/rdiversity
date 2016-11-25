#' Similarity-sensitive raw subcommunity alpha
#' 
#' \code{raw_subcommunity_alpha()} is used to calculate an estimate of 
#' naive-community metacommunity diversity.
#' 
#' \code{raw_subcommunity_alpha()} calculates the subcommunity alpha diversity of 
#' a series of columns representing independent subcommunity counts relative 
#' to the metacommunity as a whole (by default the sum of the subcommunities). 
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw subcommunity alpha diversity
#' raw_subcommunity_alpha(meta, 0:2)
#' 
raw_subcommunity_alpha <- function(meta, qs) 
  subdiv(raw_alpha(meta), qs)


#' Similarity-sensitive normalised subcommunity alpha
#' 
#' \code{normalised_subcommunity_alpha()} is used to calculate the 
#' similarity-sensitive diversity of subcommunity \emph{j} in isolation.
#' 
#' \code{normalised_subcommunity_alpha()} calculates the normalised subcommunity 
#' alpha diversity of a series of columns representing independent subcommunity 
#' counts relative to the metacommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised subcommunity alpha diversity
#' normalised_subcommunity_alpha(meta, 0:2)
#' 
normalised_subcommunity_alpha <- function(meta, qs) 
  subdiv(normalised_alpha(meta), qs)


#' Similarity-sensitive raw subcommunity beta diversity
#' 
#' \code{raw_subcommunity_beta()} is used to calculate the 
#' distinctiveness of subcommunity \emph{j}.
#' 
#' \code{raw_subcommunity_beta()} calculates the subcommunity beta diversity of a 
#' series of columns representing independent subcommunity counts relative to 
#' the metacommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw subcommunity beta diversity
#' raw_subcommunity_beta(meta, 0:2)
#' 
raw_subcommunity_beta <- function(meta, qs) 
  subdiv(raw_beta(meta), qs)


#' Similarity-sensitive normalised subcommunity beta diversity
#' 
#' \code{normalised_subcommunity_beta()} is used to calculate an 
#' estimate of the effective number of distinct subcommunities.
#' 
#' \code{normalised_subcommunity_beta()} calculates the normalised subcommunity beta 
#' diversity of a series of columns representing independent subcommunity 
#' counts relative to the metacommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised subcommunity beta diversity
#' normalised_subcommunity_beta(meta, 0:2)
#' 
normalised_subcommunity_beta <- function(meta, qs) 
  subdiv(normalised_beta(meta), qs)


#' Similarity-sensitive raw subcommunity rho diversity
#' 
#' \code{raw_subcommunity_rho()} is used to calculate the redundancy of 
#' subcommunity \emph{j}.
#' 
#' \code{raw_subcommunity_rho()}, the inverse of \code{raw_subcommunity_beta()}, 
#' calculates the raw subcommunity rho diversity of a series of columns  
#' representing independent subcommunity counts relative to the
#' metacommunity as a whole (by default the sum of the subcommunities). This 
#' measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw subcommunity rho diversity
#' raw_subcommunity_rho(meta, 0:2)
#' 
raw_subcommunity_rho <- function(meta, qs) 
  subdiv(raw_rho(meta), qs)


#' Similarity-sensitive normalised subcommunity rho diversity
#' 
#' \code{normalised_subcommunity_rho()} is used to calculate the 
#' representativeness of subcommunity \emph{j}.
#' 
#' \code{normalised_subcommunity_rho()}, the inverse of \code{subcommunity.beta}), 
#' calculates the normalised subcommunity rho diversity of a series of columns 
#' representing independent subcommunities counts relative to a total 
#' metacommunity (by default the sum of the subcommunities), for a series of 
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised subcommunity rho diversity
#' normalised_subcommunity_rho(meta, 0:2)
#' 
normalised_subcommunity_rho <- function(meta, qs) 
  subdiv(normalised_rho(meta), qs)


#' Similarity-sensitive subcommunity gamma diversity
#' 
#' \code{subcommunity_gamma()} is used to calculate the 
#' contribution per individual toward metacommunity diversity.
#' 
#' \code{subcommunity_gamma()} calculates the subcommunity gamma diversity of 
#' a series of columns representing independent subcommunity counts relative 
#' to the metacommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a  
#' vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate subcommunity gamma diversity
#' subcommunity_gamma(meta, 0:2)
#' 
subcommunity_gamma <- function(meta, qs) 
  subdiv(raw_gamma(meta), qs)


#' Similarity-sensitive raw metacommunity alpha diversity
#' 
#' \code{raw_metacommunity_alpha()} is used to calculate the 
#' naive-community metacommunity diversity.
#' 
#' \code{raw_metacommunity_alpha()} calculates the total metacommunity alpha  
#' diversity of a series of columns representing independent subcommunity 
#' counts relative to the metacommunity as a whole (by default the sum of the 
#' subcommunities). This measure may be calculated for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw metacommunity alpha diversity
#' raw_metacommunity_alpha(meta, 0:2)
#' 
raw_metacommunity_alpha <- function(meta, qs) 
  metadiv(raw_alpha(meta), qs)


#' Similarity-sensitive normalised metacommunity alpha diversity
#' 
#' \code{normalised_metacommunity_alpha()} is used to calculate the 
#' average similarity-sensitive diversity of subcommunities.
#' 
#' \code{normalised_metacommunity_alpha()} calculates the total
#' normalised metacommunity alpha diversity of a series of columns
#' representing independent subcommunity counts relative to the
#' metacommunity as a whole (by default the sum of the
#' subcommunities). This measure may be calculated for a series of
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised metacommunity alpha diversity
#' normalised_metacommunity_alpha(meta, 0:2)
#' 
normalised_metacommunity_alpha <- function(meta, qs) 
  metadiv(normalised_alpha(meta), qs)


#' Similarity-sensitive raw metacommunity beta diversity
#' 
#' \code{raw_metacommunity_beta()} is used to calculate the 
#' average distinctiveness of subcommunities.
#' 
#' \code{raw_metacommunity_beta()} calculates the total
#' metacommunity beta diversity of a series of columns representing
#' independent subcommunity counts relative to the metacommunity as a
#' whole (by default the sum of the subcommunities). This measure may
#' be calculated for a series of orders, repesented as a vector of
#' \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw metacommunity beta diversity
#' raw_metacommunity_beta(meta, 0:2)
#' 
raw_metacommunity_beta <- function(meta, qs) 
  metadiv(raw_beta(meta), qs)


#' Similarity-sensitive normalised metacommunity beta diversity
#' 
#' \code{normalised_metacommunity_beta()} is used to calculate the 
#' effective number of distinct subcommunities.
#' 
#' \code{normalised_metacommunity_beta()} calculates the total
#' normalised metacommunity beta diversity of a series of columns
#' representing independent subcommunity counts relative to the
#' metacommunity as a whole (by default the sum of the
#' subcommunities). This measure may be calculated for a series of
#' orders, repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised metacommunity beta diversity
#' normalised_metacommunity_beta(meta, 0:2)
#' 
normalised_metacommunity_beta <- function(meta, qs) 
  metadiv(normalised_beta(meta), qs)


#' Similarity-sensitive raw metacommunity rho diversity
#' 
#' \code{raw_metacommunity_rho()} is used to calculate the 
#' average redundancy of subcommunities.
#' 
#' \code{raw_metacommunity_rho()} calculates the total metacommunity
#' rho diversity of a series of columns representing independent
#' subcommunity counts relative to the metacommunity as a whole (by
#' default the sum of the subcommunities). This measure may be
#' calculated for a series of orders, repesented as a vector of
#' \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate metacommunity rho diversity
#' raw_metacommunity_rho(meta, 0:2)
#' 
raw_metacommunity_rho <- function(meta, qs) 
  metadiv(raw_rho(meta), qs)


#' Similarity-sensitive normalised metacommunity rho diversity
#' 
#' \code{normalised_metacommunity_rho()} is used to calculate the 
#' average representativeness of subcommunities.
#' 
#' \code{normalised_metacommunity_rho()} calculates the total normalised 
#' metacommunity rho diversity of a series of columns representing independent 
#' subcommunity counts relative to the metacommunity as a whole (by default 
#' the sum of the subcommunities). This measure may be calculated for a series 
#' of orders, repesented as a vector of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised metacommunity rho diversity
#' r <- normalised_rho(meta)
#' metadiv(r, 0:2)
#' normalised_metacommunity_rho(meta, 0:2)
#' 
normalised_metacommunity_rho <- function(meta, qs) 
  metadiv(normalised_rho(meta), qs)


#' Similarity-sensitive metacommunity gamma diversity
#' 
#' \code{metacommunity_gamma()} is used to calculate the 
#' metacommunity similarity-sensitive diversity.
#' 
#' \code{metacommunity_gamma()} calculates the total metacommunity gamma diversity 
#' of a series of columns representing independent subcommunity counts relative 
#' to the metacommunity as a whole (by default the sum of the subcommunities).  
#' This measure may be calculated for a series of orders, repesented as a vector 
#' of \code{qs}.
#' 
#' @param meta object of class \code{metacommunity}
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate metacommunity gamma diversity
#' g <- raw_gamma(meta)
#' metadiv(g, 0:2)
#' metacommunity_gamma(meta, 0:2)
#' 
metacommunity_gamma <- function(meta, qs) 
  metadiv(raw_gamma(meta), qs)
