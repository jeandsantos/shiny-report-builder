# Load/install required package(s)
if (!require(pracma)==TRUE) {install.packages("pracma"); require(pracma, quietly = TRUE)} else {require(pracma, quietly = TRUE)}
if (!require(docstring)==TRUE) {install.packages("docstring"); require(docstring, quietly = TRUE)} else {require(docstring, quietly = TRUE)}
if (!require(dplyr)==TRUE) {install.packages("dplyr"); require(dplyr, quietly = TRUE)} else {require(dplyr, quietly = TRUE)}
if (!require(tibble)==TRUE) {install.packages("tibble"); require(tibble, quietly = TRUE)} else {require(tibble, quietly = TRUE)}
if (!require(ggplot2)==TRUE) {install.packages("ggplot2"); require(ggplot2, quietly = TRUE)} else {require(ggplot2, quietly = TRUE)}

percentile_idx <- function(x, q=0.5) {
  #' Returns the index of the value below or equal to a percentile (q)
  out <- max(which(x <=q))
  names(out) <- as.character(q)
  return(out)
}

percentile_val <- function(x, y, q=0.5){
  #' Returns the value of the vector at a specific percentile (q)
  out <- NA
  idx = percentile_idx(y, q)
  out <- x[idx]
  names(out) <- as.character(q)
  
  return(out)
}

percentile_summary <- function(x, y, q=c(0.001,0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99,0.999)){
  #' Returns the values of at specific percentiles
  out <- rep(NA, length(q))
  
  for (i in 1:length(q)){
    
    out[i] <- percentile_val(x, y, q[i])
    
  }
  
  names(out) <- as.character(q)
  return(out)
}

cum_integral <- function(x, y){
  #' Returns the cumulative integral
  #' Requires the pracma package
  
  out <- pracma::cumtrapz(x_vals, y_test)
  row.names(out) <- as.character(x_vals)
  return(out)
  
}

norm_cum_integral <- function(x, y){
  #' Returns the normalized cumulative integral (area under the curve = 1)
  ci_vec <- cum_integral(x, y)
  
  out <- ci_vec/max(ci_vec)
  return(out)
}

param_names <- function(n_peaks){
  #' Returns vector for names based on the number of peaks
  n_peaks <- as.integer(n_peaks)
  
  params_names <- c(paste0(rep("rel_area",n_peaks),1:n_peaks), paste0(rep("std_dev",n_peaks),1:n_peaks), paste0(rep("peak",n_peaks),1:n_peaks))
  return(params_names)
}

lower_ranges <- function(x, y, n_peaks, rel_area=0, std_dev=10, peak="auto", q_peak=0.01, vec_names=param_names){
  #' Lower Ranges
  #' 
  #' Defines the lower boundaries of the optimization algorithm for values of peak relative area, peak standard deviation and peak center (mean)
  
  vec_rel_area <- rep(rel_area, n_peaks)
  vec_std_dev <- rep(std_dev, n_peaks)  
  
  if (peak=="auto"){
    vec_norm_cum_int <- norm_cum_integral(x, y)
    x_quantiles <- percentile_summary(x, vec_norm_cum_int)
    vec_peak <- rep(x_quantiles[as.character(q_peak)], n_peaks)
    if (any(is.na(vec_peak))) { vec_peak <- rep(min(x_quantiles), n_peaks) }
  } else {
    vec_peak <- rep(as.numeric(peak), n_peaks)
  }
  
  out <- c(vec_rel_area, vec_std_dev, vec_peak)
  names(out) <- vec_names(n_peaks)
  
  return(out)
}

upper_ranges <- function(x, y, n_peaks, rel_area="auto", std_dev=200, peak="auto", q_peak=0.99, vec_names=param_names){
  #' Upper Ranges
  #' 
  #' Defines the upper boundaries of the optimization algorithm for values of peak relative area, peak standard deviation and peak center (mean)
  
  vec_rel_area <- rep(rel_area, n_peaks)
  vec_std_dev <- rep(std_dev, n_peaks)  
  
  if (peak=="auto"){
    vec_norm_cum_int <- norm_cum_integral(x, y)
    x_quantiles <- percentile_summary(x, vec_norm_cum_int)
    vec_peak <- rep(x_quantiles[as.character(q_peak)], n_peaks)
    if (any(is.na(vec_peak))) { vec_peak <- rep(max(x_quantiles), n_peaks) }
    
  } else {
    vec_peak <- rep(as.numeric(peak), n_peaks)
  }
  
  if (rel_area=="auto"){
    tot_area <- pracma::trapz(x, y)
    vec_rel_area <- rep(tot_area, n_peaks)
  } else {
    vec_rel_area <- rep(as.numeric(rel_area), n_peaks)
  }
  
  out <- c(vec_rel_area, vec_std_dev, vec_peak)
  names(out) <- vec_names(n_peaks)
  
  return(out)
}

optim_init_params <- function(x, y, n_peaks, rel_area="auto", std_dev="auto", peak="auto", vec_names=param_names, q_min=0.01, q_max=0.99, verbose=FALSE){
  #' Initial Parameters
  #' 
  #' Defines the initial parameters for the optimization algorithm for values of peak relative area, peak standard deviation and peak center
  
  vec_norm_cum_int <- norm_cum_integral(x, y)
  x_quantiles <- percentile_summary(x, vec_norm_cum_int)
  
  # Define values for peak centers
  if (peak=="auto"){
    
    x_min <- x_quantiles[as.character(q_min)]
    x_max <- x_quantiles[as.character(q_max)]
    
    if (any(is.na(c(x_min, x_max)))){
      x_min <- min(x, na.rm = TRUE)
      x_max <- max(x, na.rm = TRUE)
    }
    
    dist <- (x_max - x_min)/n_peaks
    vec_peak <- seq(x_min, x_max, length.out = n_peaks)
    
  } else {
    vec_peak <- peaks
  }
  
  # Define values for relative area
  if (rel_area=="auto"){
    tot_area <- pracma::trapz(x, y)
    vec_rel_area <- rep(tot_area/n_peaks, n_peaks)
  } else {
    vec_rel_area <- rep(as.numeric(rel_area), n_peaks)
  }
  
  # Define values for standard deviation    
  if (std_dev=="auto"){
    vec_std_dev <- rep(dist, n_peaks)  
  } else {
    vec_std_dev <- rep(as.numeric(std_dev), n_peaks)
  }
  
  out <- c(vec_rel_area, vec_std_dev, vec_peak)
  names(out) <- vec_names(n_peaks)
  
  return(out)
  
  # if (verbose){ print(out) } 
  
}

run_optim <- function(target, x_vals, n_peaks = 5, seed=1, loss = loss_n_peak, method = "L-BFGS-B", maxit=5000){
  
  set.seed(seed)
  lower_boundaries <- lower_ranges(x = x_vals, y = y_test, n_peaks = n_peaks, q_peak = 0.01)
  upper_boundaries <- upper_ranges(x = x_vals, y = y_test, n_peaks = n_peaks, q_peak = 0.99)
  # init_params <- optim_init_params(x = x_vals, y = y_test, n_peaks = n_peaks)
  
  initiate_params <- c(runif(n_peaks,0, 1), runif(n_peaks, 5, 100), runif(n_peaks, 5, 1000))
  names(init_params) <- c(paste0(rep("rel_area",n_peaks),1:n_peaks), 
                          paste0(rep("std_dev",n_peaks),1:n_peaks), 
                          paste0(rep("peak",n_peaks),1:n_peaks))
  
  optim_results <- optim(par = init_params,
                         fn = loss_n_peak,
                         target = target,
                         x_vals = x_vals,
                         n_peaks = n_peaks,
                         lower = lower_boundaries,
                         upper = upper_boundaries,
                         method = method,
                         control = list(maxit = maxit)
  )
  
  optim_out <- list(solution = optim_results$par,
                    score = optim_results$value)
  
  return(optim_out)
}

fit_peak <- function(x_vals, center, sdt_dev, area){ 
  #' Fit Peak
  #' 
  #' Generates the y values of a peak based on its parameters: center, standard deviation and area
  
  return(area*dnorm(x = x_vals, mean = center, sd = sdt_dev)) 
}

plot_dist_cta <- function(x_vals, y_val){
  
  cum_tot_area <- cumtrapz(x_vals, y_val)
  norm_cum_tot_area <- cum_tot_area/max(cum_tot_area)
  
  plot(x_vals, y_val, col="blue", type="l", ylab="PDF")
  axis(2, ylim=c(0,max(y_val)), col="blue")
  
  par(new=T)
  plot(x_vals, norm_cum_tot_area, type="l", col="red", axes=F, ylab="", lty=2)
  axis(4, ylim=c(0,max(norm_cum_tot_area)), col="red")
  
}

process_solution <- function(sol_vec, names_vec=NULL){
  
  if (is.null(names_vec)) {
    sol_names <- names(sol_vec)
  } else {
    sol_names <- names_vec
  } 
  
  rel_area <- sol_vec[grep("^rel_area", x = sol_names)]
  non_zero_peaks <- rel_area != 0
  rel_area <- rel_area[non_zero_peaks]
  
  std_dev <- sol_vec[grep("^std_dev", x = sol_names)][non_zero_peaks]
  peak <- sol_vec[grep("^peak", x = sol_names)][non_zero_peaks]
  
  n_peaks<- length(rel_area)
  
  vec_list <- list()
  x_list <- list()
  elems_matrix <- matrix(rep(NA,n_peaks*length(x_vals)), ncol = n_peaks)
  
  for (i in 1:n_peaks){
    
    vec <- rel_area[i]*dnorm(x_vals, peak[i], std_dev[i])
    vec_list[[i]] <- vec
    x_list[[i]] <- x_vals
    elems_matrix[,i] <- vec
    
  }
  
  solution_df <- tibble::tibble(peak, rel_area, std_dev, curve = vec_list, x_vals = x_list) %>% 
    dplyr::arrange(peak) %>% 
    dplyr::mutate(ID = paste0("Peak_",1:n_peaks), 
                  perc_total_area = (rel_area/sum(rel_area, na.rm = TRUE))*100) %>% 
    dplyr::select(ID, peak, everything())
  
  return(solution_df)
}


plot_deconvoluted <- function(solution_df, x_vals, y_vals, unnest_cols = c("x_vals", "curve"), alpha=0.5, x_lab="x", y_lab="PDF", col_lab="Peak", title="", ...){
  #' Plot Deconvoluted Peaks
  #'
  #' Plots deconvoluted peaks together with original data
  
  sol_df <- solution_df %>% tidyr::unnest(cols = unnest_cols) 
  y_pred <- rep(0, length(solution_df$curve[[1]]))
  
  for (i in 1:nrow(solution_df)){
    y_pred <- solution_df$curve[[i]] + y_pred
  }
  
  df_transp <- data.frame(x_vals, y_vals, y_pred)
  
  g <- ggplot() +
    # geom_vline(data = sol_df, mapping = aes(xintercept = peak, col=factor(ID)), alpha=0.5) +
    geom_line(data = sol_df, mapping = aes(x=x_vals, y=curve, col=factor(ID)), size=0.75) + # , fill=factor(ID)
    geom_line(data = df_transp, mapping = aes(x=x_vals, y=y_vals), col="black", alpha=0.5, size=0.75) +
    geom_line(data = df_transp, mapping = aes(x=x_vals, y=y_pred), col="grey50", alpha=0.5, size=0.75) +
    theme_bw() +
    labs(x=x_lab, y=y_lab, col=col_lab, title=title) +
    scale_color_brewer(type = "qual", palette = "Set1") + 
    theme(...)
  
  return(g)
}


loss_n_peak <- function(y_pred_params, x_target, y_target, n_peaks, verbosity=0){
  #' Loss function for n peak optimization
  #'
  #' Returns the sum of squared error between the target curve and sum of the deconvoluted peaks

  elems_matrix <- matrix(data = rep(0, n_peaks*length(y_target)), ncol = n_peaks)
  
  for (i in 1:n_peaks){
    elems_matrix[,i] <- fit_peak(x_vals = x_target, center=y_pred_params[(2*n_peaks)+i], sdt_dev=y_pred_params[n_peaks+i], area =y_pred_params[i])
    if (verbosity>=2){
      print(paste0("Peak ",i, " - area: ", round(y_pred_params[i],4)," | ", "std dev: ", round(y_pred_params[n_peaks+i],2)," | ", "center: ", round(y_pred_params[(2*n_peaks)+i],2))) 
    }
  }
  
  elems_total <- apply(elems_matrix, MARGIN = 1, sum, na.rm=FALSE)
  err <- sum((y_target - elems_total)^2)
  if (verbosity>=1){ print(paste("SSE:",round(err, 10))) }
  return( err )
  
}


loss_n_peak_exp <- function(y_pred_params, x_target, y_target, n_peaks, verbosity=0){
  #' Loss function for n peak optimization
  #'
  #' Returns the sum of squared error between the target curve and sum of the deconvoluted peaks
  #' Adds a penalty to solutions whose total area is higher than the target total area

  tot_area_y_target <- pracma::trapz(x_target, y_target)
  elems_matrix <- matrix(data = rep(NA, n_peaks*length(y_target)), ncol = n_peaks)
  
  for (i in 1:n_peaks){
    elems_matrix[,i] <- fit_peak(x_vals = x_target, center=y_pred_params[(2*n_peaks)+i], sdt_dev=y_pred_params[n_peaks+i], area =y_pred_params[i])
    if (verbosity){
      print(paste0("Peak ",i, " - area: ", round(y_pred_params[i],4)," | ", "std dev: ", round(y_pred_params[n_peaks+i],2)," | ", "center: ", round(y_pred_params[(2*n_peaks)+i],2))) 
    }
  }  
  
  elems_total <- apply(elems_matrix, MARGIN = 1, sum, na.rm=FALSE)
  tot_area_vec <- pracma::trapz(x_target, elems_total)
  
  err <- sum((y_target - elems_total)^2)
  
  if (tot_area_vec > tot_area_y_target){ 
    return( err + 10^(tot_area_vec/tot_area_y_target) ) 
  } else { return(err) }
}















