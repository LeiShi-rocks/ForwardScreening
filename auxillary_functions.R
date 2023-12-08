# ======= Generate a factorial design =========
# levels : if integer, number of factors; if vector, level specification
# trt_group_size : vector, size of the treatment groups in inverse lexicographical order

factor.design <- function(levels, trt_group_size, interactions, centering = 0){
  # sanity check
  if(length(levels) == 1){
    #print("1")
    K <- as.integer(levels) # number of factors
    Levels <- rep(2, K)
  }
  else {
    #print("0")
    if(length(levels) > 1){
      if(!prod(levels) == length(trt_group_size)){
        stop("Number of levels and number of treatment groups do not match!")
      }
      Levels <- levels
      K <- length(levels)
    }
    else{
      stop("Incompatible input!")
    }
  }
  
  # generate a balanced factorial design: inverse Lexicographical order
  balanced_design <- which(array(0, dim = Levels) == 0, arr.ind = TRUE) - 1
  colnames(balanced_design) <- paste0("F", 1:K)
  if (centering == 0){
    final_balanced_design <- as.data.frame(balanced_design)
  }
  else if (centering == 1/2){
    final_balanced_design <- 2*as.data.frame(balanced_design) - 1
  }
  
  
  
  if (!missing(interactions) && interactions > 1){
    form <- as.formula(paste("~.^", interactions))
    final_balanced_design <- model.matrix(form, data = final_balanced_design)
    final_balanced_design <- final_balanced_design[,-1] # rm the intercepts
  }
  
  # generate the full design
  final_balanced_design <- as.matrix(final_balanced_design)
  full_design <- final_balanced_design[rep(seq_len(nrow(balanced_design)), trt_group_size),]
  
  
  # convert to a data frame
  if(K>1){
    data.frame(full_design)
    #full_design
  }
  else{
    #full_design
    data.frame(F1 = full_design)
  }
}






# ======= Generate a finite population ========
# generate a finite population from a super population
# num_pop: number of units
# num_trt_group: number of trt groups
# opts: 
##  dist: vector of length <num_trt_group>, distribution(norm, unif, binom, t) 
##  [parameters associated with the above distributions]
##  ctrMatFlag: constrast matrix flag, binary, indicating whether we are applying a contrast matrix
##  effects: if ctrMatFlag is TRUE, there must be a vector of length 2^K indicating what are the full set of specified factorial effects

finite.pop <- function(num_pop, num_trt_group, opts){
  pop <- c()
  
  # determine the working mean of the subgroups, if we are using factorial contrasts
  if (!is.null(opts$ctrMatFlag)){
    # if we are using ctrMat, then construct ctrMat first and transfrom to the subgroup means
    ctrMat <- 2*factor.design(as.integer(num_factors), rep(1, num_trt_group)) - 1
    form.int <- as.formula(paste("~.^", num_factors))
    ctrMat <- model.matrix(form.int, data = data.frame(ctrMat))
    working.mean <- ctrMat %*% opts$effects
  }
  
  for (trt in 1:num_trt_group){
    if (missing(opts)){
      pop_curr <- rnorm(num_pop, 0, 1)
    }
    else {
      if (is.null(opts$dist)){
        pop_curr <- rnorm(num_pop, 0, 1)
      }
      else {
        pop_curr <- switch(EXPR = opts$dist[trt],
                           norm = rnorm(num_pop, opts$avg[trt], opts$std[trt]),
                           unif = runif(num_pop, opts$min[trt], opts$max[trt]),
                           binom = rbinom(num_pop, opts$size[trt], opts$prob[trt]),
                           t = rt(num_pop, opts$df[trt]),
                           exp = (rexp(num_pop, opts$rate[trt]) - 1/opts$rate[trt]) + opts$mu[trt]
        )
      }
    }
  
    if(is.null(pop_curr)){
      stop("No such distribution defined!")
    }
    # make adjustment to the population
    if (!is.null(opts$ctrMatFlag)){
      pop_curr <- pop_curr - mean(pop_curr) + working.mean[trt]
    }
    pop <- cbind(pop, pop_curr)
  }
  colnames(pop) <- paste0("Trt", 1:num_trt_group)
  as.data.frame(pop)
}


# ====== Generate binary factorial experiment data ======
# INPUT:
# factoral.data.opts: 

# OUTPUT:


factorial.data <- function(num_factors, trt_group_size, pop = NULL, factorial.data.opts = list(),
                           finite.pop.init = list()){
  # sanity check: 
  ## 2^num_factors == length(trt_group_size)
  num_trt_group <- length(trt_group_size)
  if ( (2^num_factors) != num_trt_group){
    stop("Incompatible number of factors and trt group sizes!")
  }
  
  # parameter initialization
  ## global parameters
  num_pop <- sum(trt_group_size)
  num_trt_group <- length(trt_group_size)
  
  ##for finite.pop()
  finite.pop.opts <- setOpts(finite.pop.init, "finite.pop.opts", list())
  
  # generate factorial designs
  factorial.design <- factor.design(num_factors, trt_group_size) 
  
  # generate a finite population
  if (is.null(pop)){
    pop_1 <- finite.pop(num_pop, num_trt_group, finite.pop.opts)
  }
  else{
    pop_1 <- pop
  }
  
  subgroup.effect <- colMeans(pop_1)
  
  # add intervention (permutation)
  ind_perm <- sample(1:num_pop)
  obs <- pop_1[cbind(ind_perm, rep(1:num_trt_group, trt_group_size))]
  
  factorial_data <- data.frame(y = obs, factorial.design)
  
  # compute the factorial effects
  design_0 <- 2*factor.design(num_factors, rep(1, num_trt_group)) - 1

  form.int <- as.formula(paste("~.^", num_factors))
  design_0 <- model.matrix(form.int, data = data.frame(design_0))
  design_0 <- design_0[,-1]

  
  pop.fit.data <- data.frame(subgroup.effect, design_0)
  pop.fit <- lm(subgroup.effect ~ ., data = pop.fit.data)
  factorial.effect <- pop.fit$coefficients
  
  # output results
  list(
    factorial_data = factorial_data,
    factorial_pop  = pop_1,
    subgroup.effect = subgroup.effect,
    factorial.effect = factorial.effect,
    pop.fit = pop.fit
  )
}





# ======= Generate trials =======
trial.run <- function(arg_list, verbose = FALSE, print.flag = FALSE){
  # local version of environmental parameters
  levels <- arg_list$levels
  trt_group_size <-arg_list$trt_group_size
  num_pop <- sum(trt_group_size)
  num_trt_group <- length(trt_group_size)
  niter <- arg_list$niter
  model <- arg_list$model
  correction_type <- arg_list$correction_type
  alpha <- arg_list$alpha
  opts <- arg_list$opts
  interactions <- arg_list$interactions
  
  if(is.null(arg_list$wt)){
    wt <- rep(1, num_pop)
  }
  else{
    wt <- arg_list$wt
  }
  
  # Exclusive local parameters
  # results of replication
  form <- as.formula(paste("obs~", paste(model, collapse = "+")))
  q.tail <- qnorm(alpha/2, lower.tail = FALSE)  
  res.summary <- data.frame(
    x = 1:niter,
    tau.A.est = rep(0, niter),
    tau.A.var.ols = rep(0, niter),
    tau.A.var.hc0 = rep(0, niter)
  )
  res.logging <- vector(mode = "list", length = length(model))
  names(res.logging) <- model
  for (term in model){
    res.logging[[term]] <- list(estimate = rep(0, niter), 
                                ols.std = rep(0, niter), 
                                robust.std = rep(0, niter), 
                                coverage.ols = rep(0, niter),
                                coverage.robust = rep(0, niter)
    )
  }
  
  pop_1 <- finite.pop(num_pop, num_trt_group, opts)
  subgroup.effect <- colMeans(pop_1)
  
  design_0 <- 2*factor.design(levels, rep(1, num_trt_group)) - 1
  design_1 <- 2*factor.design(levels, trt_group_size) - 1
  if (is.integer(interactions)){
    form.int <- as.formula(paste("~.^", interactions))
    design_0 <- model.matrix(form.int, data = data.frame(design_0))
    design_1 <- model.matrix(form.int, data = data.frame(design_1))
  }
  
  pop.fit.data <- data.frame(subgroup.effect, design_0)
  pop.fit <- lm(subgroup.effect ~ ., data = pop.fit.data)
  factor.effect <- pop.fit$coefficients
  
  # true variance for pooled estimates
  for (iter in 1:niter){
    if(print.flag == TRUE){
      cat("Running Trial:", iter, "\n")
    }
    ind_perm <- sample(1:num_pop)
    obs <- pop_1[cbind(ind_perm, rep(1:num_trt_group, trt_group_size))]
    obs.fit.data <- data.frame(obs, design_1)
    obs.fit <- lm(form, data = obs.fit.data, weights = wt)
    obs.hc0 <- hccm(obs.fit, type=correction_type)
    
    # OLS estimate for tau_A
    # res.summary$tau.A.est[iter] <- obs.fit$coefficient[2]
    # res.summary$tau.A.var.ols[iter] <- summary(obs.fit)$coefficients["F1", "Std. Error"]    
    # res.summary$tau.A.var.hc0[iter] <- sqrt(obs.hc0["F1","F1"])
    for (term in model){
      estimate.term <- obs.fit$coefficient[term]
      ols.std.term <- summary(obs.fit)$coefficients[term, "Std. Error"]
      robust.std.term <- sqrt(obs.hc0[term, term])
      effect.term <- factor.effect[term]
      res.logging[[term]]$estimate[iter] <- estimate.term
      res.logging[[term]]$ols.std[iter] <- ols.std.term 
      res.logging[[term]]$robust.std[iter] <- robust.std.term
      res.logging[[term]]$coverage.ols[iter] <- ((estimate.term - q.tail*ols.std.term)<effect.term) &
        (effect.term < (estimate.term + q.tail*ols.std.term))
      res.logging[[term]]$coverage.robust[iter] <- ((estimate.term - q.tail*robust.std.term)<effect.term) &
        (effect.term < (estimate.term + q.tail*robust.std.term))
    }
  }
  
  # check coverage
  # if(plot.flag){
  #   plot.data <- res.summary %>% gather(var.method, var.est, -x, -tau.A.est)
  #   ggplot(plot.data, aes(x=x, y=tau.A.est, col=var.method)) +
  #     geom_line() + 
  #     geom_errorbar(aes(ymin = tau.A.est - 1.96*var.est, ymax= tau.A.est + 1.96*var.est), lty=2) +
  #    facet_wrap(~var.method)
  # }
  
  # coverage <- with(res.summary, 
  #     sum(((tau.A.est - 1.96*tau.A.var.hc0)<factor.effect[2]) &
  #       (factor.effect[2] < (tau.A.est + 1.96*tau.A.var.hc0)))/niter,
  #)
  
  if (!verbose){
    res.logging
  }
  else{
    list(res.logging = res.logging, 
         subgroup.effect = subgroup.effect, 
         factor.effect = factor.effect, 
         pop_1 = pop_1)
  }
}


# ====== Heredity ======

# decide the pre-screened model for next layer based on the current selected model
heredity.proceed <- function(num_factors,  working_model.old, working_layer.old, criterion = "full"){
  
  working_model.new <- NULL
  working_model.new.code <- c()
  working_layer.new <- working_layer.old + 1
  # Generate binary decimal design codebook
  binary.codebook <- array(list(data.frame(code =c())), num_factors)
  binary.design <- factor.design(as.integer(num_factors), rep(1, 2^num_factors) ,num_factors)
  # print(binary.design)
  for (ind_col in 1:(2^num_factors - 1)){
    effect.string <- paste0(binary.design[,ind_col], collapse = "")
    effect.code <- data.frame(code = strtoi(effect.string, base = 2))
    effect.level <- num_factors - log2(sum(as.integer(intToBits(effect.code))))
    #print(effect.level)
    print(effect.code)
    rownames(effect.code) <- colnames(binary.design)[ind_col]
    binary.codebook[[effect.level]] <- rbind(binary.codebook[[effect.level]], effect.code)
  }
  # print(binary.codebook)
  # for sanity check: <working_model.old> must only contain interactions of order <working_layer.old>
  # print(working_model.old)
  # print(rownames(binary.codebook))
  # print(rownames(binary.codebook[[working_layer.old]]))
  sanity.check <- setdiff(working_model.old, rownames(binary.codebook[[working_layer.old]]))
  if (length(sanity.check) != 0){
    stop("Wrong input! Input model is incompatible with the input level.")
  }
  if (working_layer.old == num_factors){
    stop("Has reached the last layer. No need to forward the model!")
  }
  
  
  if (criterion == "full"){
    working_model.new <- rownames(binary.codebook[[working_layer.new]])
  }
  else if (criterion == "weak"){
    working_model.old.code <- binary.codebook[[working_layer.old]][working_model.old,]
    main.effect.code <- binary.codebook[[1]]$code
    ind <- 0
    for (i in 1:length(working_model.old.code)){
      for (j in 1:length(main.effect.code)){
        ind <- ind + 1
        working_model.new.code[ind] <- bitwAnd(working_model.old.code[i], main.effect.code[j])
      }
    }
    working_model.new.code <- unique(working_model.new.code)
    working_model.new <- rownames(binary.codebook[[working_layer.new]] %>% filter(code %in% working_model.new.code))
  }
  else if (criterion == "strong"){
    if(0){
      # generate a candidate model: working_model.pre
      working_model.old.code <- binary.codebook[[working_layer.old]][working_model.old,]
      main.effect.code <- binary.codebook[[1]]$code
      ind <- 0
      for (i in 1:length(working_model.old.code)){
        for (j in 1:length(main.effect.code)){
          ind <- ind + 1
          working_model.pre.code[ind] <- bitwAnd(working_model.old.code[i], main.effect.code[j])
        }
      }
      working_model.pre.code <- unique(working_model.pre.code)
      working_model.pre <- rownames(binary.codebook[[working_layer.new]] %>% filter(code %in% working_model.pre.code))
    }
    
    # generate a candidate model by pruning next layer:
    working_model.prune.code <- c()
    working_model.old.code <- binary.codebook[[working_layer.old]][working_model.old,]
    main.effect.code <- binary.codebook[[1]]$code
    working_model.old.comp.code <- binary.codebook[[working_layer.old]] %>% filter(!(code %in% working_model.old.code))
    working_model.old.comp.code <- working_model.old.comp.code$code
    #print(length(working_model.old.comp.code))
    if(length(working_model.old.comp.code) >= 1){
      ind <- 0
      for (i in 1:length(working_model.old.comp.code)){
        for (j in 1:length(main.effect.code)){
          ind <- ind + 1
          #print(ind)
          #print(working_model.old.comp.code[i])
          #print(main.effect.code[j])
          working_model.prune.code[ind] <- bitwAnd(working_model.old.comp.code[i],     main.effect.code[j])
        }
      }
      working_model.prune.code <- unique(working_model.prune.code)
      working_model.prune <- rownames(binary.codebook[[working_layer.new]] %>% filter(code %in% working_model.prune.code))
    }
    
    working_model.new <- rownames(binary.codebook[[working_layer.new]] %>% filter(!(code %in% working_model.prune.code)))
    
  }
  
  
  list(codebook = binary.codebook, 
       design = binary.design, 
       working_model = working_model.new)
  
}



heredity.proceed.new <- function(num_factors,  working_model.old, 
                                 working_layer.old, criterion = "full"){
  
  working_model.new <- NULL
  working_model.new.list <- list()
  working_layer.new <- working_layer.old + 1
  # Generate binary decimal design codebook
  binary.codebook <- array(list(data.frame(code =c())), num_factors)
  binary.design <- factor.design(as.integer(num_factors), 
                                 rep(1, 2^num_factors),
                                 working_layer.new)
  
  binary.codebook <- colnames(binary.design)
  
  start.old.ind <- cumsum(choose(num_factors, 0:working_layer.old))[working_layer.old]
  end.old.ind <- cumsum(choose(num_factors, 0:working_layer.old))[working_layer.new] - 1
  working_model.old.full <- binary.codebook[start.old.ind:end.old.ind]

  # print(working_model.old.full)
  # print(working_model.old)
  
  sanity.check <- setdiff(working_model.old, 
                          working_model.old.full)
  # print(length(sanity.check))
  if (length(sanity.check) != 0){
    stop("Wrong input! Input model is incompatible with the input level.")
  }
  if (working_layer.old == num_factors){
    stop("Has reached the last layer. No need to forward the model!")
  }
  
  start.new.ind <- cumsum(choose(num_factors, 0:working_layer.new))[working_layer.new]
  end.new.ind <- cumsum(choose(num_factors, 0:working_layer.new))[working_layer.new + 1] - 1
  working_model.new.full <- binary.codebook[start.new.ind:end.new.ind]
  main.effect <- paste0("F", 1:num_factors)
  
  if (criterion == "full"){
    working_model.new <- working_model.new.full
  }
  else if (criterion == "weak"){
    ind <- 0
    for (i in 1:length(working_model.old)){
      for (j in 1:length(main.effect)){
        check.effect <- union(unlist(strsplit(working_model.old[i], "[.]")), 
                              main.effect[j])
        if (length(check.effect) == working_layer.new){
          ind <- ind + 1
          working_model.new[ind] <- 
            paste0("F", 
                   sort(as.numeric(unlist(strsplit(gsub("F", "", check.effect), split  = "\\.")))), 
                   collapse = ".")
        }
      }
    }
    working_model.new <- unique(working_model.new)
  }
  else if (criterion == "strong"){
    # generate a candidate model by pruning next layer:
    working_model.new.prune <- c()
    working_model.old.comp <- setdiff(working_model.old.full, working_model.old)
    # print(working_model.old)
    
    ind <- 0
    for (i in 1:length(working_model.old.comp)){
      for (j in 1:length(main.effect)){
        check.effect <- union(unlist(strsplit(working_model.old.comp[i], "[.]")), 
                              main.effect[j])
        if (length(check.effect) == working_layer.new){
          ind <- ind + 1
          working_model.new.prune[ind] <- 
            paste0("F", 
                   sort(as.numeric(unlist(strsplit(gsub("F", "", check.effect), split  = "\\.")))), 
                   collapse = ".")
        }
      }
    }
    
    working_model.new.prune <- unique(working_model.new.prune)
    
    #print("full new: ")
    #print(working_model.new.full)
    #print("prune new: ")
    #print(working_model.new.prune)
    #print("old comp: ")
    #print(working_model.old.comp)
    
    
    working_model.new <- setdiff(working_model.new.full, working_model.new.prune)
  }
  else{
    stop("No such criterion defined!")
  }
  
  
  list(design = binary.design, 
       working_model = working_model.new)
}







# ====== Layer-wise Model Selection Methods =======

# dataIn frame containing
## (y, F1, ..., FK): data; requires the data obeys the standard form
# pre_selected_model: some full model we use to run linear regression; not just a layer of working model! 
# wt: weights
# method: using which model selection method; by default Bonferroni
# level: select up to which level; by default 1
# criterion: based on what heredity assumptions {strong, weak, full (BY DEFAULT)}
# model.selection.opts:
## For Bonferroni:
##   - correction.type: hc0, hc1, ...
##   - alpha: significance level
##   - test_model: the model used for test
##   - robust.flag: whether to use a robust variance estimation or not

## For LASSO:
##   - test_model: same as above


model.selection <- function(dataIn, pre_selected_model, wt = NULL, 
                            method = "Bonferroni", model.selection.opts = list()){
  # If pre_selected_model and test_model is not the same, define test_model
  # print(pre_selected_model)
  test_model <- setOpts(model.selection.opts, "test_model", pre_selected_model)
  
  if (is.null(wt)){
    wt <- rep(1, nrow(dataIn))
  }
  
  if (method == "Bonferroni"){
    # parameters: 
    # alpha: significance level
    # robust.flag: whether to use robust variance estimation or not
    # correction_type: hc0, hc1, ...
    alpha <- ifelse(is.null(model.selection.opts$alpha), 0.05, model.selection.opts$alpha)
    robust.flag <- ifelse(is.null(model.selection.opts$robust.flag), TRUE, model.selection.opts$robust.flag)
    correction.type <- ifelse(is.null(model.selection.opts$correction.type), "hc0", model.selection.opts$correction.type)
    
    form <- as.formula(paste("y~", paste(pre_selected_model, collapse = "+")))
    fit.model <- lm(form, data = dataIn, weights = wt)
    point.estimates <- fit.model$coefficients[test_model]
    
    if (robust.flag){
      if (correction.type == "hc0"){
        group.var <- dataIn %>% group_by(across(c(-y))) %>% 
          summarise(num=n(), group.var = var(y)) %>%
          mutate(est.var = group.var/num)
        std.est <- sqrt(sum(group.var$est.var) / (nrow(group.var)^2))
      }
      if (correction.type == "hc1" || correction.type == "hc2"){
        group.var <- dataIn %>% group_by(across(c(-y))) %>% 
          summarise(num=n(), group.var = var(y)) %>%
          mutate(est.var = group.var/num)
        correction.factor <- (nrow(group.var) / (nrow(group.var) - 1 - length(pre_selected_model)))
        std.est <- sqrt(correction.factor * sum(group.var$est.var) / (nrow(group.var)^2))
      }
      # std.est <- sqrt(diag(hccm(fit.model, type=correction.type)))
      std.est <- rep(std.est, 1 + length(pre_selected_model))
      names(std.est) <- rownames(summary(fit.model)$coefficients)
    }
    else{
      std.est <- summary(fit.model)$coefficients[, "Std. Error"]
    }
    std.est <- std.est[test_model]
    
    q.tail.bfr <- qnorm(alpha/(2*length(test_model)), lower.tail = FALSE)
    print(paste0("test: ", length(test_model), " q.tail: ", q.tail.bfr))
    
    working_flag <- rep(TRUE, length(test_model))
    
    t_val.store <- rep(0, length(test_model))
    
    for (ind in 1:length(test_model)){
      term <- test_model[ind]
      # print(point.estimates[term])
      # print(std.est)
      t_val <- abs(point.estimates[term]) / (std.est[term])
      # print(t_val)
      t_val.store[ind] <- t_val
      if (t_val <= q.tail.bfr){
        working_flag[ind] <- FALSE
      }
    }
    # plot(t_val.store)
    post_working_model <- test_model[working_flag]
  }
  else if(method == "LASSO"){
    # print("LASSO")
    form <- as.formula(paste("y~", paste(pre_selected_model, collapse = "+")))
    # print(length(form))
    # X <- as.matrix(model.matrix(form, data = dataIn))[, -1]
    # print(pre_selected_model)
    if(length(pre_selected_model) <= 1){
      post_working_model <- intersect(pre_selected_model, test_model)
      fit.model <- NULL
      warning("Cannot run lasso since model is too small!")
    }
    else{
      best_lambda_choice = setOpts(model.selection.opts, "best_lambda_choice", "lambda.1se")
      X <- as.matrix(dataIn[, pre_selected_model]) 
      y <- as.matrix(dataIn$y)
      
      ncv = setOpts(model.selection.opts, "ncv", 50)
      print(paste("ncv:", ncv))
      best_lambda_array = rep(0, ncv)
      for (cv_trial in 1:ncv){
        cv_model <- cv.glmnet(X, y, alpha = 1, weights = wt)
        best_lambda_array[cv_trial] <- cv_model[[best_lambda_choice]]
      }
      best_lambda = mean(best_lambda_array)
      
      # best_lambda
      #-- plot(cv_model)
      # best_lambda = 0.01
      fit.model <- glmnet(X, y, alpha = 1, lambda = best_lambda, weights = wt)
      coeff <- coef(fit.model)
      # a special data structure called "dgCMatrix", which is designed for sparse matrix.
      post_working_model <- intersect(test_model, 
                                      coeff@Dimnames[[1]][1+coeff@i[abs(coeff@x) > 1e-4]])
    }
  }
  else{
    stop("No such model selection methods are defined yet!")
  }
  
  list(post_working_model = post_working_model, 
       fit.model = fit.model)
}






# ====== Forward Selection =======

# factorial_data frame containing
## (y, F1, ..., FK): factorial data; requires the data obeys the standard form
# wt: weights
# alpha.vec: a vector of length 'level' that contains significance levels in each layer of the forward selection
# method: using which model selection method; by default Bonferroni
# level: select up to which level; by default 1
# criterion: based on what heredity assumptions {strong, weak, full (BY DEFAULT)}
# forward.select.opts:
## model.selection.opts


forward.select <- function(factorial_data,  alpha.vec, wt = NULL, level = 1L, 
                           forward.select.opts = list(),
                           heredity.proceed.init = list(), 
                           model.selection.init = list()){
  # parameter initialization
  ## basic setup
  working_level <- 1
  num_factors <- ncol(factorial_data) - 1 
  num_pop <- nrow(factorial_data)
  
  ## we will be using model.selection(), so need to initialize the parameters
  method <- setOpts(model.selection.init, "method", "Bonferroni")
  model.selection.opts <- setOpts(model.selection.init, "model.selection.opts", list())

  
  ## initialize heredity.proceed
  criterion <- setOpts(heredity.proceed.init, "criterion", "full")
  
  ## storing the running info for each layer
  fit.model <- array(list(), level)
  
  # print(method)
  # print(criterion)
  
  ## if weight is null, set it as inverse propensity score
  if (is.null(wt)){
    wt <- factorial_data %>% group_by(across(c(-y))) %>% 
      mutate(num=n()) %>% ungroup() %>% 
      mutate(inv_num = num_pop/num) %>% 
      dplyr::select(inv_num)
    wt <- wt$inv_num
  }
  
  ## preprocessing of the dataset for regression
  data.full <- data.frame(2 * factorial_data[, -1] - 1)
  data.full <- model.matrix(as.formula(paste0("~.^", level)), data = data.full)
  data.full <- data.frame(data.full[, -1])
  data.full <- cbind(factorial_data["y"], data.full)
  
  # because the first column is a vector of outcomes
  working_model <- paste0("F", 1:num_factors)
  selected_model <- NULL
  pre_selected_model <- c(selected_model, working_model)
  
  # Select main effects
  # model.selection.opts <- setOpts(model.selection.init, "model.selection.opts", list())
  model.selection.opts$alpha <- alpha.vec[1]
  model.selection.opts$test_model <- working_model
  
  
  
  
  raw_model <- model.selection(data.full, pre_selected_model, 
                               wt, method, model.selection.opts)
  # print(wt)
  working_model <- raw_model$post_working_model
  selected_model <- c(selected_model, working_model)
  fit.model[[1]] <- raw_model$fit.model
  # print(selected_model)
  
  
  # Select interactions
  if (level >= 2 && length(working_model) >= 1){
    for (lv in 2:level){
      # working model for layer <lv>
      working_model <- heredity.proceed.new(num_factors, working_model, working_level, criterion)
      working_model <- working_model$working_model
      working_level <- working_level + 1
      
      if (length(working_model) == 0) {
        break
      }
      
      # updating <model.selection.opts>
      model.selection.opts$alpha <- alpha.vec[lv]
      model.selection.opts$test_model <- working_model
      
      # getting <pre_selected_model>
      pre_selected_model <- c(selected_model, working_model)
      # print(pre_selected_model)
      raw_model <- model.selection(data.full, pre_selected_model, 
                                   wt, method, model.selection.opts)
      
      # update working_model and selected_model at layer <lv>
      working_model <- raw_model$post_working_model
      selected_model <- c(selected_model, working_model)
      fit.model[[lv]] <- raw_model$fit.model
      
      # check whether working_model is empty
      if (length(working_model) == 0) {
        break
      }
    }
  }
  
  
  # return: selected_model; fit.model
  fit.model.flag <- TRUE
  if (fit.model.flag){
    list(
      selected_model = selected_model,
      fit.model = fit.model
    )
  }
  else{
    list(selected_model = selected_model)
  }
}




# ====== Factor combination selection and inference ======
# Select the best factor combinations
factor.comb <- function(point.estimates, covariance.est, num.ties, 
                        threshold, threshold.CL, threshold.CR,
                        alpha = 0.05,
                        factor.comb.opts = list()){
  candidate.estimates <- point.estimates
  point.estimates.ordered <- sort(point.estimates, decreasing = TRUE)
  point.estimates.rank    <- rank(-point.estimates)
  selected.ties <- array(list(), num.ties)
  curr.order <- 1
  indices <- NULL
  for (ind.tie in 1:num.ties){
    upper.threshold <- point.estimates.ordered[curr.order] + threshold * threshold.CR[ind.tie]
    lower.threshold <- point.estimates.ordered[curr.order] - threshold * threshold.CL[ind.tie]
    indices <- which(candidate.estimates <= upper.threshold & candidate.estimates >= lower.threshold)
    candidate.estimates[indices] <- Inf
    selected.ties[[ind.tie]]$indices <- indices
    selected.ties[[ind.tie]]$estimates <- point.estimates[indices]
    selected.ties[[ind.tie]]$size <- length(indices)
    effect.est <- mean(point.estimates[indices])
    selected.ties[[ind.tie]]$effect <- effect.est
    one.vec <- rep(1, length(indices))
    selected.Sigma <- covariance.est[indices, indices]
    effect.var <- as.numeric(t(one.vec) %*% selected.Sigma %*% one.vec) / length(indices)^2
    selected.ties[[ind.tie]]$variance <- effect.var
    q.tail <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = F)
    selected.ties[[ind.tie]]$CI <- c(effect.est - q.tail * sqrt(effect.var), 
                                     effect.est + q.tail * sqrt(effect.var))
    
    curr.order <- curr.order + length(indices)
    # sanity check 
    if (curr.order > length(point.estimates)){
      warning("No more tie sets!")
      break
    }
  }
  
  # output 
  list(
    selected.ties = selected.ties
  )
}


# Select the ties using a smooth bootstrap
factor.comb.bootstrap <- function(){
  
}


# Data driven tuning scheme 

  
# ===== Generate factorial combination selection trials ====

# Run trials for factor level combination selection
comb.select.trial <- function(factorial.data.init, alpha.vec, level, 
                              design.run, design.core,
                              Niter = 100, num.ties = 1, random.seed = 2022,
                              comb.select.trial.opts = list()){
  # options(warn = 2)
  # Initialization
  # set.seed(random.seed)
  
  ## factorial data
  num_factors <- setOpts(factorial.data.init, "num_factors", 5)
  trt_group_size <- setOpts(factorial.data.init, "trt_group_size", rep(100, 2^5))
  num_trt_group = 2^num_factors
  pop <- setOpts(factorial.data.init, "pop", NULL)
  factorial.data.opts <- setOpts(factorial.data.init, "factorial.data.opts", list())
  finite.pop.init <- setOpts(factorial.data.init, "finite.pop.init", list())

  ## Thresholds
  delta <- 0.25 
  threshold <- 100^(-delta) 
  threshold.CL <- rep(1.0, num.ties)
  threshold.CR <- rep(1.0, num.ties)
  verbose <- TRUE
  
  ## simulation records
  simulation.res <- list(
    model.select.res = array(list(), Niter),
    comb.select.res = array(list(), Niter),
    brutal.select.res = array(list(), Niter),
    est.effect.res = array(list(), Niter),
    var.effect.res = array(list(), Niter),
    group.var.res  = array(list(), Niter),
    group.avg.res  = array(list(), Niter)
  )
  
  ## method flags 
  method.flag <- data.frame(
    flag = c(1, 1, 1, 1, 1, 1, 1, 1),
    correction = c("full", "weak", "strong", "full", "weak", "strong", "naive", "naive"),
    method = c(rep("Bonferroni", 3), rep("LASSO", 3), "Bonferroni", "LASSO"),
    row.names = c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL")
  )
  
  # Iteration
  for (iter in 1:Niter){
    # first six methods
    factorial_data_raw <- factorial.data(num_factors, trt_group_size, pop = pop,
                                     factorial.data.opts, finite.pop.init)
    factorial_data <- factorial_data_raw$factorial_data
    
    # processing data
    group.var <- factorial_data %>% group_by(across(c(-y))) %>% 
      summarise(num=n(), group.var = var(y), est.avg = mean(y)) %>%
      arrange(across(num_factors:1)) %>%
      mutate(est.var = group.var/num)
    
    simulation.res$group.var.res[[iter]] = group.var$est.var
    simulation.res$group.avg.res[[iter]] = group.var$est.avg
    
    wt <- factorial_data %>% group_by(across(c(-y))) %>% 
      mutate(num=n()) %>% ungroup() %>% 
      mutate(inv_num = num_pop/num) %>% 
      dplyr::select(inv_num)
    wt <- wt$inv_num

    
    data.full <- data.frame(2 * factorial_data[, -1] - 1)
    data.full <- model.matrix(as.formula(paste0("~.^", level)), data = data.full)
    data.full <- data.frame(data.full[, -1])
    data.full <- cbind(factorial_data["y"], data.full)
    
    
    simulation.res$model.select.res[[iter]] <- list()
    simulation.res$comb.select.res[[iter]]  <- list()
    simulation.res$est.effect.res[[iter]] <- list()
    simulation.res$var.effect.res[[iter]] <- list()
    
    
    for (method.ind in 1:8){
      running.tag  <- row.names(method.flag)[method.ind]
      running.flag   <- method.flag$flag[method.ind]
      running.correction <- method.flag$correction[method.ind]
      running.method <- method.flag$method[method.ind]
      
      if (verbose){
        print(paste0("Running: Iteration = ", iter, 
                     ", Method = ", running.tag, 
                     ", y = "     , factorial_data$y[1]))
      }
      
      if (running.flag && method.ind <= 6){
        # model.selection.init <- list()
        
        # forward.select.opts
        forward.select.opts <- list()
        
        # heredity.proceed.init <- list()
        heredity.proceed.init <- list(
          criterion = running.correction
        )
        
        # model.selection.init
        model.selection.opts <- list(
          correction.type = "hc0",
          robust.flag = TRUE,
          ncv = 1
        )
        model.selection.init <- list(
          method = running.method,
          model.selection.opts = model.selection.opts
        )
        
        selected.model.output <- forward.select(factorial_data, alpha.vec, wt = NULL, 
                                                level, forward.select.opts, 
                                                heredity.proceed.init, model.selection.init)
        
        selected_model = selected.model.output$selected_model
        simulation.res$model.select.res[[iter]][[running.tag]] <- selected_model
        # print(selected_model)
        # print(paste("y~", paste(selected_model, collapse = "+")))
        form <- as.formula(paste("y~", paste(selected_model, collapse = "+")))
        fit.model <- lm(form, data = data.full, weights = wt)
        simulation.res$est.effect.res[[iter]][[running.tag]] <- fit.model$coefficients
        selected_design_core = cbind(1, design.core[, selected_model])
        # colnames(selected_design_core)[1] = "(Intercept)"
        selected_design_core = as.matrix(selected_design_core)
        
        simulation.res$var.effect.res[[iter]][[running.tag]] = 
          t(selected_design_core) %*% (selected_design_core * group.var$est.var)/num_trt_group^2
        
        
        # factor level combination selection
        if (num.ties!=0){
          post.design.run <- dplyr::select(design.run, selected_model)
          post.design.core <- dplyr::select(design.core, selected_model)
          post.factorial.data <- cbind(y = factorial_data$y, post.design.run)
          lm.post <- lm(y~., data = post.factorial.data)
          sigma.post <- hccm(lm.post, "hc0")
          
          Y.hat.post <- as.matrix(cbind(1, post.design.core)) %*% lm.post$coefficients 
          var.post <- as.matrix(cbind(1, post.design.core)) %*% sigma.post %*% 
            t(as.matrix(cbind(1, post.design.core)))
          
          factor.selection.post <- factor.comb(Y.hat.post, var.post, num.ties, threshold, threshold.CL, threshold.CR)
          
          simulation.res$comb.select.res[[iter]][[running.tag]] <- factor.selection.post$selected.ties          
        }
      }
      
      if (running.flag && method.ind == 7){ # Naive Bonferroni
        # model selection
        # print("yes")
        
        
        running.factorial_data <- cbind(y = factorial_data$y, design.run)
        pre_selected_model <- names(subset(running.factorial_data, select = -y))
        select.model.output <- model.selection(running.factorial_data, 
                                               pre_selected_model, 
                                               wt = wt,
                                               method = "Bonferroni")
        selected_model <- select.model.output$post_working_model
        simulation.res$model.select.res[[iter]][[running.tag]] <- selected_model
        
        # selected_model = selected.model.output$selected_model
        simulation.res$model.select.res[[iter]][[running.tag]] <- selected_model
        
        form <- as.formula(paste("y~", paste(selected_model, collapse = "+")))
        fit.model <- lm(form, data = data.full, weights = wt)
        simulation.res$est.effect.res[[iter]][[running.tag]] <- fit.model$coefficients
        
        
        selected_design_core = cbind(1, design.core[, selected_model])
        # colnames(selected_design_core)[1] = "(Intercept)"
        selected_design_core = as.matrix(selected_design_core)
        
        simulation.res$var.effect.res[[iter]][[running.tag]] = 
          t(selected_design_core) %*% (selected_design_core * group.var$est.var)/num_trt_group^2
        
        
        
        # factor level combination selection
        if (num.ties!=0){
          post.design.run <- dplyr::select(design.run, selected_model)
          post.design.core <- dplyr::select(design.core, selected_model)
          post.factorial.data <- cbind(y = factorial_data$y, post.design.run)
          lm.post <- lm(y~., data = post.factorial.data)
          sigma.post <- hccm(lm.post, "hc0")
          
          Y.hat.post <- as.matrix(cbind(1, post.design.core)) %*% lm.post$coefficients 
          var.post <- as.matrix(cbind(1, post.design.core)) %*% sigma.post %*% t(as.matrix(cbind(1, post.design.core)))
          
          factor.selection.post <- factor.comb(Y.hat.post, var.post, num.ties, 
                                               threshold, threshold.CL, threshold.CR)
          
          simulation.res$comb.select.res[[iter]][[running.tag]] <- factor.selection.post$selected.ties          
        }
      }
      
      if (running.flag && method.ind == 8){ # Naive LASSO
        # model selection
        # print("yes")
        wt <- factorial_data %>% group_by(across(c(-y))) %>% 
          mutate(num=n()) %>% ungroup() %>% 
          mutate(inv_num = num_pop/num) %>% 
          dplyr::select(inv_num)
        wt <- wt$inv_num
        
        running.factorial_data <- cbind(y = factorial_data$y, design.run)
        pre_selected_model <- names(subset(running.factorial_data, select = -y))
        select.model.output <- model.selection(running.factorial_data, 
                                               pre_selected_model, 
                                               wt = wt,
                                               method = "LASSO", 
                                               model.selection.opts = list(ncv = 1))
        selected_model <- select.model.output$post_working_model
        simulation.res$model.select.res[[iter]][[running.tag]] <- selected_model
        
        # selected_model = selected.model.output$selected_model
        simulation.res$model.select.res[[iter]][[running.tag]] <- selected_model
        
        form <- as.formula(paste("y~", paste(selected_model, collapse = "+")))
        fit.model <- lm(form, data = data.full, weights = wt)
        simulation.res$est.effect.res[[iter]][[running.tag]] <- fit.model$coefficients
        selected_design_core = cbind(1, design.core[, selected_model])
        #colnames(selected_design_core)[1] = "(Intercept)"
        selected_design_core = as.matrix(selected_design_core)
        simulation.res$var.effect.res[[iter]][[running.tag]] = 
          t(selected_design_core) %*% (selected_design_core * group.var$est.var)/num_trt_group^2
        
        # factor level combination selection
        if (num.ties != 0){
          post.design.run <- dplyr::select(design.run, selected_model)
          post.design.core <- dplyr::select(design.core, selected_model)
          post.factorial.data <- cbind(y = factorial_data$y, post.design.run)
          lm.post <- lm(y~., data = post.factorial.data)
          sigma.post <- hccm(lm.post, "hc0")
          
          Y.hat.post <- as.matrix(cbind(1, post.design.core)) %*% lm.post$coefficients 
          var.post <- as.matrix(cbind(1, post.design.core)) %*% sigma.post %*% t(as.matrix(cbind(1, post.design.core)))
          
          factor.selection.post <- factor.comb(Y.hat.post, var.post, num.ties, 
                                               threshold, threshold.CL, threshold.CR)
          
          simulation.res$comb.select.res[[iter]][[running.tag]] <- factor.selection.post$selected.ties          
        }
      }
    }
    
    
    
    # Brutal factor level combination selection
    if (num.ties != 0){
      group.estimates <- factorial_data %>% 
        group_by_at(rev(names(factorial_data)[-1])) %>%
        summarise(avg.est = mean(y), var.est = var(y), num = n()) 
      # print(head(group.estimates))
      point.est <- group.estimates$avg.est
      covariance.est <- diag(group.estimates$var.est / group.estimates$num)
      
      factor.selection.brutal <- factor.comb(point.est, covariance.est,
                                             num.ties, threshold, 
                                             threshold.CL, threshold.CR)
      
      simulation.res$brutal.select.res[[iter]] <- factor.selection.brutal
    }
  }
  simulation.res
}
  
    
  
# ====== Post processing =======

# check coverage

check.coverage <- function(res.logging){
  model <- names(res.logging)
  coverage.percent <- matrix(NA, nrow = 2, ncol = length(model))
  rownames(coverage.percent) <- c("ols", "robust")
  colnames(coverage.percent) <- model
  for (term in model){
    res <- res.logging[[term]]
    coverage.percent["ols", term] <- sum(res$coverage.ols) / length(res$coverage.ols)
    coverage.percent["robust", term] <- sum(res$coverage.robust) / length(res$coverage.robust)
  }
  coverage.percent
}


# check asymptotic normality:

check.normal <- function(res.logging){
  model <- names(res.logging)
  niter <- length(res.logging[[1]]$estimate)
  point.estimate <- matrix(NA, nrow = niter, ncol = length(model))
  for (index in 1:length(model)){
    point.estimate[, index] <- res.logging[[index]]$estimate
  }
  colnames(point.estimate) <- model
  point.estimate <- data.frame(point.estimate)
  
  # wide to long format
  plot.data <- point.estimate %>% gather(key = effect, value = value)
  
  # plot a collection of histograms
  plot.estimate <- ggplot(plot.data, aes(x = value)) + 
    facet_wrap(vars(effect), scales = "free") +
    geom_histogram(bins = 20) +
    theme_gray()
  
  plot.estimate
}


# visualize point estimators:

check.points <- function(res.logging){
  model <- names(res.logging)
  niter <- length(res.logging[[1]]$estimate)
  point.estimate <- matrix(NA, nrow = niter, ncol = length(model))
  for (index in 1:length(model)){
    point.estimate[, index] <- res.logging[[index]]$estimate
  }
  colnames(point.estimate) <- model
  point.estimate <- data.frame(point.estimate)
  
  # wide to long format
  plot.data <- point.estimate %>% gather(key = effect, value = value)
  
  # plot a collection of histograms
  plot.estimate <- ggplot(plot.data, aes(x = effect, y = value, fill = effect)) + 
    geom_boxplot()
  
  plot.estimate
}

# check model selection results

check.model.selection <- function(model.select.res, correct.model = NULL){
  Niter <- length(model.select.res)
  method.flag <- data.frame(
    flag = c(1, 1, 1, 1, 1, 1, 1, 1),
    correction = c("full", "weak", "strong", "full", "weak", "strong", "naive", "naive"),
    method = c(rep("Bonferroni", 3), rep("LASSO", 3), "Bonferroni", "LASSO"),
    row.names = c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL")
  )
  model.selection.report <- matrix(0, nrow = 8, ncol = 4)
  # print(model.selection.report)
  rownames(model.selection.report) <- c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL")
  colnames(model.selection.report) <- c("Perfect", "Over", "Under", "NoneofAbove")
  for (method.ind in 1:8){
    
    running.tag  <- row.names(method.flag)[method.ind]
    running.flag   <- method.flag$flag[method.ind]
    running.correction <- method.flag$correction[method.ind]
    running.method <- method.flag$method[method.ind]
    
    for (iter in 1:Niter){
      running.model <- model.select.res[[iter]][[running.tag]]
      if (all(running.model %in% correct.model) &&
          length(running.model) == length(correct.model)){
        model.selection.report[running.tag, "Perfect"] <- 
          model.selection.report[running.tag, "Perfect"] + 1
      }
      else if (all(correct.model %in% running.model) &&
               length(correct.model) < length(running.model)){
        model.selection.report[running.tag, "Over"] <- 
          model.selection.report[running.tag, "Over"] + 1
      }
      else if (all(running.model %in% correct.model) &&
               length(running.model) < length(correct.model)){
        model.selection.report[running.tag, "Under"] <- 
          model.selection.report[running.tag, "Under"] + 1
      }
      else{
        model.selection.report[running.tag, "NoneofAbove"] <- 
          model.selection.report[running.tag, "NoneofAbove"] + 1
      }
    }
  }
  
  model.selection.report
}

# check target contrast coverage

check.target.coverage <- function(simulation.res,
                                  design.core, target_weight = 1, true_effect){
  model.select.res = simulation.res$model.select.res
  est.effect.res = simulation.res$est.effect.res
  var.effect.res = simulation.res$var.effect.res
  group.var.res  = simulation.res$group.var.res
  group.avg.res  = simulation.res$group.avg.res
  
  Niter <- length(model.select.res)
  method.flag <- data.frame(
    flag = c(1, 1, 1, 1, 1, 1, 1, 1),
    correction = c("full", "weak", "strong", "full", "weak", "strong", "naive", "naive"),
    method = c(rep("Bonferroni", 3), rep("LASSO", 3), "Bonferroni", "LASSO"),
    row.names = c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL")
  )
  target.coverage.report <- matrix(0, nrow = 9, ncol = 4)
  # print(target.coverage.report)
  rownames(target.coverage.report) <- c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL", "NS")
  colnames(target.coverage.report) <- c("Bias", "Std", "Coverage", "Rejection")
  
  for (method.ind in 1:8){
    
    running.tag  <- row.names(method.flag)[method.ind]
    running.flag   <- method.flag$flag[method.ind]
    running.correction <- method.flag$correction[method.ind]
    running.method <- method.flag$method[method.ind]
    
    for (iter in 1:Niter){
      running_model  =  model.select.res[[iter]][[running.tag]]
      running_effect =  est.effect.res[[iter]][[running.tag]]
      running_var    =  var.effect.res[[iter]][[running.tag]]
      
      running_design_core = cbind(1, design.core[, running_model])
      # colnames(running_design_core)[1] = "(Intercept)"
      running_design_core = as.matrix(running_design_core)
      
      target_f = t(running_design_core) %*% target_weight
      
      effect.est.r = sum(target_f * running_effect)
      std.est.r = sqrt(t(target_f) %*% running_var %*% target_f)
      
      target.coverage.report[method.ind, 1] = 
        target.coverage.report[method.ind, 1] + (effect.est.r - true_effect)/Niter
      target.coverage.report[method.ind, 2] = 
        target.coverage.report[method.ind, 2] + std.est.r/Niter
      target.coverage.report[method.ind, 3] = 
        target.coverage.report[method.ind, 3] + 
        (abs(effect.est.r - true_effect) < std.est.r*qnorm(0.975))/Niter
      target.coverage.report[method.ind, 4] = 
        target.coverage.report[method.ind, 4] + 
        (abs(effect.est.r) > std.est.r*qnorm(0.975))/Niter
    }
  }
  
  for (iter in 1:Niter){
    effect.est.wls = sum(group.avg.res[[iter]] * target_weight)
    std.est.wls = sqrt(sum(t(target_weight) * group.var.res[[iter]] * target_weight))
    target.coverage.report[9, 1] = 
      target.coverage.report[9, 1] + (effect.est.wls - true_effect)/Niter
    target.coverage.report[9, 2] = 
      target.coverage.report[9, 2] + std.est.wls/Niter
    target.coverage.report[9, 3] = 
      target.coverage.report[9, 3] + 
      (abs(effect.est.wls - true_effect) < std.est.wls*qnorm(0.975))/Niter
    target.coverage.report[9, 4] = 
      target.coverage.report[9, 4] + 
      (abs(effect.est.wls) > std.est.wls*qnorm(0.975))/Niter
  }
  target.coverage.report
}




check.comb.selection <- function(comb.select.res, brutal.select.res, tie.ind,
                                 correct.comb = NULL){
  Niter <- length(comb.select.res)
  method.flag <- data.frame(
    flag = c(1, 1, 1, 1, 1, 1, 1, 1),
    correction = c("full", "weak", "strong", "full", "weak", "strong", "naive", "naive"),
    method = c(rep("Bonferroni", 3), rep("LASSO", 3), "Bonferroni", "LASSO"),
    row.names = c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL")
  )
  comb.selection.report <- matrix(0, nrow = 9, ncol = 3)
  # print(model.selection.report)
  rownames(comb.selection.report) <- c("FB", "WB", "SB", "FL", "WL", "SL", "NB", "NL", "AVG")
  colnames(comb.selection.report) <- c("Selection", "Coverage", "CIlength")
  for (method.ind in 1:8){
    
    running.tag  <- row.names(method.flag)[method.ind]
    running.flag   <- method.flag$flag[method.ind]
    running.correction <- method.flag$correction[method.ind]
    running.method <- method.flag$method[method.ind]
    
    for (iter in 1:Niter){
      running.comb <- comb.select.res[[iter]][[running.tag]][[tie.ind]]$indices
      if (all(running.comb %in% correct.comb$indices) &&
          length(running.comb) == length(correct.comb$indices)){
        comb.selection.report[running.tag, "Selection"] <- 
          comb.selection.report[running.tag, "Selection"] + 1
      }
      
      running.CI <- c(comb.select.res[[iter]][[running.tag]][[tie.ind]]$effect -
                        1.96 * sqrt(comb.select.res[[iter]][[running.tag]][[tie.ind]]$variance),
                      comb.select.res[[iter]][[running.tag]][[tie.ind]]$effect +
                        1.96 * sqrt(comb.select.res[[iter]][[running.tag]][[tie.ind]]$variance))

      if (correct.comb$effect > running.CI[1] && 
          correct.comb$effect < running.CI[2]){
        comb.selection.report[running.tag, "Coverage"] <- 
          comb.selection.report[running.tag, "Coverage"] + 1
      }
      
      comb.selection.report[running.tag, "CIlength"] <- 
        comb.selection.report[running.tag, "CIlength"] + (running.CI[2] - running.CI[1])/Niter

    }
    
  }
  
  for (iter in 1:Niter){
    running.comb <- brutal.select.res[[iter]]$selected.ties[[tie.ind]]$indices
    if (all(running.comb %in% correct.comb$indices) &&
        length(running.comb) == length(correct.comb$indices)){
      comb.selection.report["AVG", "Selection"] <- 
        comb.selection.report["AVG", "Selection"] + 1
    }
    
    running.CI <- c(brutal.select.res[[iter]]$selected.ties[[tie.ind]]$effect -
                      1.96 * sqrt(brutal.select.res[[iter]]$selected.ties[[tie.ind]]$variance),
                    brutal.select.res[[iter]]$selected.ties[[tie.ind]]$effect + 
                      1.96 * sqrt(brutal.select.res[[iter]]$selected.ties[[tie.ind]]$variance))
    if (correct.comb$effect > running.CI[1] && 
        correct.comb$effect < running.CI[2]){
      comb.selection.report["AVG", "Coverage"] <- 
        comb.selection.report["AVG", "Coverage"] + 1
    }
    
    comb.selection.report["AVG", "CIlength"] <- 
      comb.selection.report["AVG", "CIlength"] + (running.CI[2] - running.CI[1])/Niter
  }
  
  comb.selection.report
}



# ====== Utilities ======
setOpts <- function(list.in, field, defaults){
  if (is.null(list.in[[field]])){
    out <- defaults
  }
  else{
    out <- list.in[[field]]
  }
  out
}
