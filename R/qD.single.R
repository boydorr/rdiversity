qD.single <-
function(proportions, q)
  1 / power.mean(values = proportions, order = q - 1, weights = proportions)
