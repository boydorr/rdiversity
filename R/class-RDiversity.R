# Define S4 Class
rdiv <- setClass("rdiv",
                       contains = 'data.frame',
                       slots = c(measure = "character",
                                 tag = "formula",
                                 level = "character"))


# Constructor function
rdiv <- function(measure, tag, level) 
  new("RDiversity", 
      measure = measure,
      tag = tag,
      level = level)


# 
is.RDiversity <-
  function (x) 
  {
    inherits(x, "RDiversity")
  }


# #show() is the S4 analogy of print() in S3
# setMethod("show", "rdiv",
#           function(object) {
#             print(object)
#           }
# )


# 
# b<-RDiversity(res,'alpha')
# show(b)
# b
