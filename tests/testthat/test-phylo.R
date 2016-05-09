context('Testing the phylogenetic diversity measures')

test_that("Answers match up with Leinster-Cobbold Appendix A", {
  # Ultrametric
  phy <- ape::read.tree(text="(A:2,B:2)R:1;")
  pds <- c(0.6, 0.4)
  x <- as.rdphylo(phy, pds)
  super <- supercommunity(phy, pds.abundance = pds)
  
  expect_equivalent(sum(phy$edge.length) + phy$root.edge, 
                    c(superdiv(gamma(super),0) * x@parameters$Tbar[1]))
  expect_equivalent(c(super@type_abundance), 
                    c(0.4, (2/3)*0.4, 0.2, (1/3)*0.4))
  expect_equivalent(super@similarity, 
                    rbind(c(1,0,1,0),c(0,1,0,1),rep(1,4),rep(1,4)))
  
  # Non-ultrametric
  phy2 <- ape::read.tree(text="(A:1,B:2)R:1;")
  pds <- c(0.6, 0.4)
  x2 <- as.rdphylo(phy2,pds)
  super2 <- supercommunity(phy2, pds.abundance = pds)
  
  expect_equivalent(sum(phy2$edge.length) + phy2$root.edge, 
                    c(superdiv(gamma(super2),0) * x2@parameters$Tbar[1]))
  expect_equivalent(c(super2@type_abundance), 
                    c(0.25, (2/2.4)*0.4, 0.25, (1/2.4)*0.4))
  expect_equivalent(super2@similarity, 
                    rbind(c(1.2, 0, 1.2, 0), c(0, 0.8, 0, 0.8),
                          c(1.2, 0.8, 1.2, 0.8), c(1.2, 0.8, 1.2, 0.8)))
})

