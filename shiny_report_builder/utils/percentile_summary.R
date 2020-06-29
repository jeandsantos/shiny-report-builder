percentile_summary <- function(x, y, q=c(0.001,0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99,0.999)){
  #' Returns the values of at specific percentiles
  out <- rep(NA, length(q))
  
  for (i in 1:length(q)){
    
    out[i] <- percentile_val(x, y, q[i])
    
  }
  
  names(out) <- as.character(q)
  return(out)
}
