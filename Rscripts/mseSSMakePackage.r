# Build help files from code using roxy
require(roxyPackage)
roxy.package(
  pck.source.dir  = getwd(),
  pck.version     = "0.0.1",
  #R.libs          = file.path("C:","Program Files","R","R-2.14.2","library"),
  R.libs          = getwd(),
  repo.root       = file.path("..","Help"),
  pck.description = data.frame(
    Package     = "mseSS",
    Title       = "Management Strategy Evaluation Framework for Stock Synthesis",
    Author      = "Chris Grandin <chris.grandin@dfo-mpo.gc.ca>",
    AuthorsR    = "c(person(given  = \"Chris\",
                    family = \"Grandin\",
                    email  = \"chris.grandin@dfo-mpo.gc.ca\",
                    role   = c(\"aut\",\"cre\")))",
    Maintainer  = "Chris Grandin <chris.grandin@dfo-mpo.gc.ca>",
    Depends     = "R (>= 3.0.1)",
    Description = "Functions for running and interpreting Fisheries Management Strategy Evaluations using Stock Synthesis",
    License     = "GPL (>=3)",
    Encoding    = "UTF-8",
    LazyLoad    = "yes",
    URL         = "http://code.google.com/p/msess/",
    stringsAsFactors = FALSE))

