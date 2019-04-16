ols_plot_resid_stand_fit <- 
function(x,id.n=3,labels.id = names(residuals(x)), sub.caption=NULL,...) {
    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            warning(gettextf("not plotting observations with leverage one:\\n  %s",
                paste(which(isInf), collapse = ", ")), call. = FALSE,
                domain = NA)
            x[isInf] <- NaN
        }
        x
    }
    if (!inherits(x, "lm")) stop("use only with 'lm' objects")
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
		show.rs <- labels.id	
		show.rs[sort.list(abs(rs), decreasing = TRUE)[(id.n+1):length(show.rs)]] <- NA
    }
    if (is.null(sub.caption)) {
        cal <- x$call
        if (!is.na(m.f <- match("formula", names(cal)))) {
            cal <- cal[c(1, m.f)]
            names(cal)[2L] <- ""
        }
        cc <- deparse(cal, 80)
        nc <- nchar(cc[1L], "c")
        abbr <- length(cc) > 1 || nc > 75
        sub.caption <- if (abbr)
            paste(substr(cc[1L], 1L, min(75L, nc)), "...")
        else cc[1L]
    }

    sqrtabsr <- sqrt(abs(rs))
    ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
    yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))
    yhn0 <-  yh
	
    ##plot
	g <- ggplot(tibble(x=yhn0,y=sqrtabsr,label=show.rs), aes(x,y,label=label))
	g <- g + labs(x=l.fit,y=yl,title="Scale-Location Plot",caption=sub.caption)
	g <- g + geom_point() + geom_smooth(...) #geom_smooth(se=F,method = 'loess', formula = 'y ~ x') 
 	g <- g + geom_label(na.rm=T)
	#geom_text(nudge_y=(max(sqrtabsr) - min(sqrtabsr))/length(sqrtabsr),na.rm=T)
	g
    ##end plot
}
