.onLoad  <- function(library, section) {
   require("EvalEst")
   warning("Package dse2 is deprecated. Please use packages dse and EvalEst instead.
      Some function previously in dse2 are now in dse, but many are in EvalEst.")
   invisible(TRUE)
   }
