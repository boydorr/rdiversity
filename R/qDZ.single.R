qDZ.single <-
function(proportions, q,
                       Z = diag(nrow(proportions)),
                       Zp = Z %*% proportions)
  1 / power.mean(values = Zp, order = q - 1, weights = proportions)
