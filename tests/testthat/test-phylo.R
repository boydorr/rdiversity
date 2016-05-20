context('Testing the phylogenetic diversity measures')

test_that("Answers match up with Leinster-Cobbold Appendix A", {
  # Ultrametric
  phy <- ape::read.tree(text="(A:2,B:2)R:1;")
  pds <- c(0.6, 0.4)
  x <- as.rdphylo(pds, phy)
  super <- supercommunity(pds, phy)
  
  expect_equivalent(sum(phy$edge.length) + phy$root.edge, 
                    unlist(superdiv(gamma(super),0)$diversity * x@Tbar))
  expect_equivalent(c(super@type_abundance), 
                    c(0.2, 0.4, (1/3)*0.4, (2/3)*0.4))
  expect_equivalent(super@similarity, 
                    rbind(rep(1,4),c(1,1,0,0),rep(1,4),c(0,0,1,1)))
  
  # Non-ultrametric
  phy2 <- ape::read.tree(text="(A:1,B:2)R:1;")
  pds <- c(0.6, 0.4)
  x2 <- as.rdphylo(pds, phy2)
  super2 <- supercommunity(pds, phy2)
  
  expect_equivalent(sum(phy2$edge.length) + phy2$root.edge, 
                    unlist(superdiv(gamma(super2),0)$diversity * x2@Tbar[1]))
  expect_equivalent(c(super2@type_abundance), 
                    c(0.25, 0.25, (1/2.4)*0.4, (2/2.4)*0.4))
  expect_equivalent(super2@similarity, 
                    rbind(c(1.2, 1.2, 0.8, 0.8), c(1.2, 1.2, 0, 0),
                          c(1.2, 1.2, 0.8, 0.8), c(0, 0, 0.8, 0.8)))
})

