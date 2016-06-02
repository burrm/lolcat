#Based on description at: http://pareonline.net/pdf/v15n12.pdf
transform.box.cox <- function(x
                              ,lambda
                              ,correct.min = 0
                              ,lower.specification = NA
                              ,target = NA
                              ,upper.specification = NA
                              ,inverse = F
                              ) {

  spec <- c(lower.specification, target, upper.specification)
  
  
  if (!inverse) {
    if (lambda != 0) {
      ret <- ((x + correct.min)^lambda -1) / lambda
      spec <- ((spec + correct.min)^lambda -1)/lambda
    } else {
      ret <- log(x+correct.min)
      spec <- log(spec + correct.min)
    }
  } else {
    if (lambda != 0) {
      ret <-  (lambda * x +1)^(1/lambda) - correct.min 
      spec <- (lambda * spec + 1)^(1/lambda) - correct.min
    } else {
      ret <- exp(x) - correct.min
      spec <- exp(spec) - correct.min
    }
  }
  
  if(any(!is.na(spec))) {
    list(x = ret
         ,lower.specification = spec[1]
         ,target = spec[2]
         ,upper.specification = spec[3]
         )
  } else {
    ret
  }
}