#' Residual density plot
#'
#' ols_plot_resid_dense plots a residual density plot.
#' 
#' This function is an adjunct to olsrr package which adds density plot functionality.
#'
#' @param model an lm model 
#'   scores.
#' @return A ggplot density plot.
#' @examples
#' DF <- data.frame(x=rnorm(30,10,2),
#'					Treat=as.factor(c(rep("A",10),rep("B","10"),rep("C","10"))),
#'					Block=as.factor(rep(c(1,2),15)))
#' model=lm(x~Block+Treat,DF)
#' ols_plot_resid_dense(model)

ols_plot_resid_dense <- 
function (model)
{
	if (!inherits(model, "lm")) stop("use only with 'lm' objects")
    d <- tibble(x = resid(model))
    p <- ggplot(d, aes(x = x)) + geom_density() + 
    xlab("Residuals") + ggtitle("Residual Density Plot")
	print(p)
}

