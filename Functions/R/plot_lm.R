#' Residual plots
#'
#' plot_lm plots graphs useful in diagnosing linear models.
#'
#' This is (sort of) a copy of the stats plot.lm function,
#' but with ggplot output. It's a work in progress...
#'
#' @param plots
#' @param id.n
#' @param labels.id
#' @param caption
#' @param arrange
#' @param ...
#' @param x an lm model object

plot_lm <-

  function(
  x,
  plots=c(1L:4L),
  id.n=3,
  labels.id = names(residuals(x)),
  caption=NULL,
  arrange=T,
  ...
){
	suppressPackageStartupMessages(require(tidyverse))
	suppressPackageStartupMessages(require(gridExtra))
	dropInf <- function(x, h) {
			if (any(isInf <- h >= 1)) {
					warning(gettextf("not plotting observations with leverage one:\\n  %s",
							paste(which(isInf), collapse = ", ")), call. = FALSE,
							domain = NA)
					x[isInf] <- NaN
			}
			x
	}
	if (!inherits(x, "lm")) stop("use only with '(g)lm' objects")
	show <- rep(F,6)
	show[plots] <- T
    r <- residuals(x)
    yh <- predict(x)
    n <- length(r)
    s <-  sqrt(deviance(x)/df.residual(x))
    hii <- (infl <- influence(x, do.coef = FALSE))$hat
    ylab23 <- "Standardized residuals"
    r.w <- r
    rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)

    l.fit <- "Fitted values"

    if (is.null(id.n))
        id.n <- 0
    else {
        id.n <- as.integer(id.n)
        if (id.n < 0L || id.n > n)
            stop(gettextf("'id.n' must be in {1,..,%d}", n),
                domain = NA)
    }
    if (id.n > 0L) {
        if (is.null(labels.id))
            labels.id <- paste(1L:n)
		show.r <- show.rs <- labels.id
		show.r[sort.list(abs(r), decreasing = TRUE)[(id.n+1):length(show.rs)]] <- NA
		show.rs[sort.list(abs(rs), decreasing = TRUE)[(id.n+1):length(show.rs)]] <- NA
    }

	gl <- list()

	# plot histogram
	if (show[1]) {
      h <- hist(r, plot = FALSE)
      xfit <- seq(min(r),max(r),length=80)
	  yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) * diff(h$mids[1:2]) * n

      d2 <- tibble(x = xfit, y = yfit)
      d <- tibble(x = r)
      g <- ggplot(d, aes(x = x))
	  g <- g + labs(x="Residuals",title="Residual Histogram")
	  g <- g + geom_histogram(bins = 6, color = "black", fill = "#ADD8E6")
	  g <- g + geom_line(data = d2, aes(x = x, y = y), color = "#0000A0", size = 1.2)
      gl$histogram <- g
	}
	# end plot

	# plot normal q-q
	if (show[2]) {
      y_q <- quantile(r[!is.na(r)], c(0.25, 0.75))
      x_q <- qnorm(c(0.25, 0.75))
      slope <- diff(y_q)/diff(x_q)
      int <- y_q[1L] - slope * x_q[1L]
	  d <- merge(ggplot_build(ggplot(tibble(x = r), aes(sample = x)) + stat_qq())$data[[1]],tibble(y = r,label=show.rs))
      g <- ggplot(d, aes(x = x, y=y,label=label))
	  g <- g + labs(x= "Theoretical Quantiles", y="Sample Quantiles", title = "Normal Q-Q Plot")
	  g <- g + geom_point(colour="blue")
      g <- g + geom_abline(slope = slope, intercept = int, color = "red")
  	  g <- g + geom_label(na.rm=T)
      gl$norm_qq <- g
	}
	# end plot

	# plot residual fit
	if(show[3]) {
	  d <- tibble(predicted=yh,resid=r,label=show.r)
      g <- ggplot(d, aes(x = predicted, y = resid,label=label))
      g <- g + labs(x="Fitted Value", y="Residual", title="Residual vs Fitted Values")
	  g <- g + geom_point(shape = 1,colour = "blue")
	  g <- g + geom_hline(yintercept = 0,colour = "red")
  	  g <- g + geom_label(na.rm=T)
	  gl$resid_fit <- g
	}
	# end plot

    # plot Scale-Location
	if(show[4]) {
      sqrtabsr <- sqrt(abs(rs))
      ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
      yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))
      yhn0 <-  yh
	  d <- tibble(x=yhn0,y=sqrtabsr,label=show.rs)
 	  g <- ggplot(d, aes(x,y,label=label))
	  g <- g + labs(x=l.fit,y=yl,title="Scale-Location Plot")
	  g <- g + geom_point() + geom_smooth(...)
 	  g <- g + geom_label(na.rm=T)
	  gl$scale_loc <- g
	}
    # end plot

	# plot density
	if(show[5]) {
  	  d <- tibble(x = r)
      g <- ggplot(d, aes(x = x))
      g <- g + labs(x="Residuals",title="Residual Density Plot")
	  g <- g + geom_density()
	  gl$density <- g
	}
	if(arrange) {
	  return(grid.arrange(grobs=gl,se=F))
	} else {
	  return(gl)
	}
}

