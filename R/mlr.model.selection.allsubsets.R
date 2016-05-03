mlr.model.selection.allsubsets <- function(full.model.fx
                                           ,data
                                           ,n.iv.min = 1
                                           ,n.iv.max = 5
                                           ,max.models = 1000
                                           ,selection.method = c("random.selection")#, "low.order.first", "high.order.first")
                                           #,return.formula = F
                                           ,calculate.cp = T
                                           ) {
  
  fx.terms<-terms(full.model.fx)
  
  response<-all.vars(full.model.fx)[attributes(fx.terms)$response]
  iv.names<-attributes(terms(full.model.fx))$term.labels
  
  #Cp stuff
  if (calculate.cp) {
    #cp.r.squared.model - used later
    #cp.parameter.count.model - used later
    
    cp.r.squared.all.vars <- summary(lm(full.model.fx, data = data))$r.squared
    cp.parameter.count.all.vars <- length(iv.names)+1
    cp.sample.size <- nrow(data)
  }
  
  
  
  
  
  retval <- data.frame()
  
  for (i in iv.names) {
    retval[[i]] <- logical(0)
  }
  
  #retval$model.f      <- numeric(0)
  #retval$model.f.df1  <- numeric(0)
  #retval$model.f.df2  <- numeric(0)
  #retval$model.f.p    <- numeric(0)
  retval$model.n.iv   <- numeric(0)
  retval$model.rsq    <- numeric(0)
  retval$model.adjrsq <- numeric(0)
  retval$model.cp     <- numeric(0)
  #retval$model.aic    <- numeric(0)
  #retval$model.bic    <- numeric(0)
  #retval$model.loglik <- numeric(0)
  
  # if (return.formula) {
  #   retval$formula <- character(0)
  # }
  
  base.model.entry <- function(iv.names, selected.iv.names) {
    retval <- list()
    
    for (i in iv.names) {
      retval[[i]] <- F
    }

    for (i in selected.iv.names) {
      retval[[i]] <- T
    }
    
    #retval$model.f      <- numeric(0)
    #retval$model.f.df1  <- numeric(0)
    #retval$model.f.df2  <- numeric(0)
    #retval$model.f.p    <- numeric(0)
    retval$model.n.iv   <- length(selected.iv.names)
    retval$model.rsq    <- 0
    retval$model.adjrsq <- 0
    retval$model.cp     <- length(iv.names)*10
    #retval$model.aic    <- numeric(0)
    #retval$model.bic    <- numeric(0)
    #retval$model.loglik <- numeric(0)
    
    retval
  }
  
  base.model.formula <- function(response, selected.iv.names) {
    ret <- paste(response,"~ 1")
    
    for ( i in selected.iv.names) {
      ret <- paste(ret,"+",i)
    }
    
    #print(ret)
    
    eval(parse(text=ret))
  }
  
  if (n.iv.max > length(iv.names)) {
    n.iv.max <- length(iv.names)
  }
  
  if (n.iv.min > length(iv.names)) {
    n.iv.min <- length(iv.names)
  }
  
  model.list <- data.frame()
  for (i in n.iv.min:n.iv.max) {
    model.list[[paste("v",i,sep="")]] <- character(0)
  }

  for (i in n.iv.max:n.iv.min) {
    
    this.addition <- as.data.frame(t(combn(iv.names, i)))
    
    if (i < n.iv.max) {
      for (i in 1:(n.iv.max-i)) {
        this.addition <- cbind(this.addition, rep(NA, nrow(this.addition)))
      }
    }
    
    names(this.addition) <- names(model.list)
    
    model.list <- rbind(model.list, this.addition)
  }
  
  
  if (nrow(model.list) > max.models) {
    if (selection.method[1] == "random.selection") {
      model.list <- model.list[sample(1:nrow(model.list), max.models, replace = F ),]
    }
  }

  
  for (i in 1:nrow(model.list)) {
    selected.iv.names <- na.omit(as.character(unlist(model.list[i,])))

    #print(selected.iv.names)
    
    this.model <- base.model.entry(iv.names, selected.iv.names)

    #print(this.model)
    this.model.fx <- base.model.formula(response, selected.iv.names)

    #print(this.model.fx)
    
    #evaluate the model
    this.model.out <- summary(lm(this.model.fx, data = data))
    
    #print(this.model.out)
    
    
    
    #this.model$model.f      <- numeric(0)
    #this.model$model.f.df1  <- numeric(0)
    #this.model$model.f.df2  <- numeric(0)
    #this.model$model.f.p    <- numeric(0)
    #this.model$model.n.iv   <- length(selected.iv.names)
    #this.model$model.rsq    <- 0
    #this.model$model.adjrsq <- 0
    
    #this.model$model.aic    <- numeric(0)
    #this.model$model.bic    <- numeric(0)
    #this.model$model.loglik <- numeric(0)
    
    this.model$model.rsq <- this.model.out$r.squared
    this.model$model.adjrsq <- this.model.out$adj.r.squared

    if (calculate.cp) {
    
      cp.r.squared.model <- this.model$model.rsq
      cp.parameter.count.model <- this.model$model.n.iv
      
      #cp.r.squared.all.vars <- summary(lm(full.model.fx, data = data))$r.squared
      #cp.parameter.count.all.vars <- length(iv.names)+1
      #cp.sample.size <- nrow(data)
      
      this.model$model.cp     <- mlr.mallow.cp.simple(r.squared.model = cp.r.squared.model,
                                                      parameter.count.model = cp.parameter.count.model,
                                                      r.squared.all.vars = cp.r.squared.all.vars,
                                                      parameter.count.all.vars = cp.parameter.count.all.vars,
                                                      sample.size = cp.sample.size)
    
    }
    
        
    # if (return.formula) {
    #   this.model$formula <- this.model.fx
    # }
    
    retval <- rbind(retval, this.model)    
  }
  
  ri <- data.frame(Intercept = rep(T, nrow(retval)))
  
  cbind(ri,retval)
}
