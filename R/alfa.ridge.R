alfa.ridge <- function(y, x, a = seq(-1, 1, by = 0.1), h = TRUE, model = "gaussian", lambda = NULL,
                       xnew = NULL) {

  res <- list()
  for ( i in 1:length(a) ) {
    xa <- Compositional::alfa(x, a[i], h = h)$aff ## apply the alpha-transformation
    mod <- glmnet::glmnet(xa, y, alpha = 0, family = model, lambda = lambda)
    est <- NULL
    if ( !is.null(xnew) ) {
      xnew <- Compositional::alfa(xnew, a[i], h = h)$aff ## apply the alpha-transformation
      est <- predict(mod, xnew, s = mod$lambda, type = "response")
    }
    res[[ i ]] <- list(mod = mod, est = est)
  }
  names(res) <- paste("alpha=", a, sep = "")
  res
  
}
