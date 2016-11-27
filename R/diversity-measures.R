#' Similarity-sensitive raw subcommunity alpha
#'
#' \code{raw_sub_alpha()} is used to calculate an estimate of
#' naive-community metacommunity diversity.
#'
#' \code{raw_sub_alpha()} calculates the subcommunity alpha diversity of
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
#' raw_sub_alpha(meta, 0:2)
#'
raw_sub_alpha <- function(meta, qs)
  subdiv(raw_alpha(meta), qs)


#' Similarity-sensitive normalised subcommunity alpha
#'
#' \code{norm_sub_alpha()} is used to calculate the
#' similarity-sensitive diversity of subcommunity \emph{j} in isolation.
#'
#' \code{norm_sub_alpha()} calculates the normalised subcommunity
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
#' norm_sub_alpha(meta, 0:2)
#'
norm_sub_alpha <- function(meta, qs)
  subdiv(norm_alpha(meta), qs)


#' Similarity-sensitive raw subcommunity beta diversity
#'
#' \code{raw_sub_beta()} is used to calculate the
#' distinctiveness of subcommunity \emph{j}.
#'
#' \code{raw_sub_beta()} calculates the subcommunity beta diversity of a
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
#' raw_sub_beta(meta, 0:2)
#'
raw_sub_beta <- function(meta, qs)
  subdiv(raw_beta(meta), qs)


#' Similarity-sensitive normalised subcommunity beta diversity
#'
#' \code{norm_sub_beta()} is used to calculate an
#' estimate of the effective number of distinct subcommunities.
#'
#' \code{norm_sub_beta()} calculates the normalised subcommunity beta
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
#' norm_sub_beta(meta, 0:2)
#'
norm_sub_beta <- function(meta, qs)
  subdiv(norm_beta(meta), qs)


#' Similarity-sensitive raw subcommunity rho diversity
#'
#' \code{raw_sub_rho()} is used to calculate the redundancy of
#' subcommunity \emph{j}.
#'
#' \code{raw_sub_rho()}, the inverse of \code{raw_sub_beta()},
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
#' raw_sub_rho(meta, 0:2)
#'
raw_sub_rho <- function(meta, qs)
  subdiv(raw_rho(meta), qs)


#' Similarity-sensitive normalised subcommunity rho diversity
#'
#' \code{norm_sub_rho()} is used to calculate the
#' representativeness of subcommunity \emph{j}.
#'
#' \code{norm_sub_rho()}, the inverse of \code{norm_sub_beta}),
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
#' norm_sub_rho(meta, 0:2)
#'
norm_sub_rho <- function(meta, qs)
  subdiv(norm_rho(meta), qs)


#' Similarity-sensitive subcommunity gamma diversity
#'
#' \code{sub_gamma()} is used to calculate the
#' contribution per individual toward metacommunity diversity.
#'
#' \code{sub_gamma()} calculates the subcommunity gamma diversity of
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
#' sub_gamma(meta, 0:2)
#'
sub_gamma <- function(meta, qs)
  subdiv(raw_gamma(meta), qs)


#' Similarity-sensitive raw metacommunity alpha diversity
#'
#' \code{raw_meta_alpha()} is used to calculate the
#' naive-community metacommunity diversity.
#'
#' \code{raw_meta_alpha()} calculates the total metacommunity alpha
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
#' raw_meta_alpha(meta, 0:2)
#'
raw_meta_alpha <- function(meta, qs)
  metadiv(raw_alpha(meta), qs)


#' Similarity-sensitive normalised metacommunity alpha diversity
#'
#' \code{norm_meta_alpha()} is used to calculate the
#' average similarity-sensitive diversity of subcommunities.
#'
#' \code{norm_meta_alpha()} calculates the total
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
#' norm_meta_alpha(meta, 0:2)
#'
norm_meta_alpha <- function(meta, qs)
  metadiv(norm_alpha(meta), qs)


#' Similarity-sensitive raw metacommunity beta diversity
#'
#' \code{raw_meta_beta()} is used to calculate the
#' average distinctiveness of subcommunities.
#'
#' \code{raw_meta_beta()} calculates the total
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
#' raw_meta_beta(meta, 0:2)
#'
raw_meta_beta <- function(meta, qs)
  metadiv(raw_beta(meta), qs)


#' Similarity-sensitive normalised metacommunity beta diversity
#'
#' \code{norm_meta_beta()} is used to calculate the
#' effective number of distinct subcommunities.
#'
#' \code{norm_meta_beta()} calculates the total
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
#' norm_meta_beta(meta, 0:2)
#'
norm_meta_beta <- function(meta, qs)
  metadiv(norm_beta(meta), qs)


#' Similarity-sensitive raw metacommunity rho diversity
#'
#' \code{raw_meta_rho()} is used to calculate the
#' average redundancy of subcommunities.
#'
#' \code{raw_meta_rho()} calculates the total metacommunity
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
#' raw_meta_rho(meta, 0:2)
#'
raw_meta_rho <- function(meta, qs)
  metadiv(raw_rho(meta), qs)


#' Similarity-sensitive normalised metacommunity rho diversity
#'
#' \code{norm_meta_rho()} is used to calculate the
#' average representativeness of subcommunities.
#'
#' \code{norm_meta_rho()} calculates the total normalised
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
#' norm_meta_rho(meta, 0:2)
#'
norm_meta_rho <- function(meta, qs)
  metadiv(norm_rho(meta), qs)


#' Similarity-sensitive metacommunity gamma diversity
#'
#' \code{meta_gamma()} is used to calculate the
#' metacommunity similarity-sensitive diversity.
#'
#' \code{meta_gamma()} calculates the total metacommunity gamma diversity
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
#' meta_gamma(meta, 0:2)
#'
meta_gamma <- function(meta, qs)
  metadiv(raw_gamma(meta), qs)
