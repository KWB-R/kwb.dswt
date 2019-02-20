# validate_HQ_relationships ----------------------------------------------------

#' Validate H-Q-Relationships
#' 
#' Validate HQ relationships for DN = 150 and DN = 300
#' 
#' @export
#' 
validate_HQ_relationships <- function()
{
  for (DN in c(150, 300)) {
    cat(sprintf("DN = %d\n", DN))
    validate_HQ_relationship(DN)
    hq <- H_Q_Table(DN)
    hq$calc <- round(H_to_Q(hq$H_m, DN), 3)
    print(hq)
  }
}

# validate_HQ_relationship -----------------------------------------------------

#' validate_HQ_relationship
#' 
#' Validate HQ relationship by plotting Q versus H from manufacturer table
#'   as points and the regression line given by the derived formula
#' @keywords internal
validate_HQ_relationship <- function(DN)
{
  hq <- H_Q_Table(DN)
  
  graphics::plot(
    hq$H_m, hq$Q_L_s, xlab = "H in m", ylab = "Q in L/s", 
    main = paste("DN", DN, sep=" = ")
  )
  
  h <- seq(0, max(hq$Q_L_s), by = 0.01)
  graphics::lines(h, H_to_Q(h, DN = DN), col = "blue")
}
