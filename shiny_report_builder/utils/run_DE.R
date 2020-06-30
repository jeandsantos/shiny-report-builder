run_DE <- function(x_vals, target, n_peaks = 3, seed=1, loss = loss_n_peak_exp, maxit=1000, pop_size=100, p_crossover=0.5, strategy=3, verbosity=0){
  
  set.seed(seed)
  lower_boundaries <- lower_ranges(x = x_vals, y = target, n_peaks = n_peaks, q_peak = 0.01)
  upper_boundaries <- upper_ranges(x = x_vals, y = target, n_peaks = n_peaks, q_peak = 0.99)
  
  optim_results <- DEoptim::DEoptim(fn = loss,
                                    y_target = y_test, 
                                    x_target = bins,
                                    n_peaks = n_peaks,
                                    lower = lower_boundaries,
                                    upper = upper_boundaries,
                                    control = DEoptim.control(NP = pop_size, itermax = maxit, CR = p_crossover, trace = verbosity*100, strategy=as.integer(strategy)))
  
  optim_out <- list(solution = optim_results$optim$bestmem,
                    score = optim_results$optim$bestval)
  
  return(optim_out)
}