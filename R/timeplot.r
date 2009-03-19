plot.rtv = function (x, y, ...)
{	if (missing (y) ) timeplot.ecdf (x, ...)
	else timeplot.line (x, y, ...)
}

timeplot.multicycle = function (x, y)
{	op = par (mfrow=c (2, 2) )
	if (missing (y) )
	{	timeplot.ecdf (x, main="overall")
		timeplot.ecdf (x, cu="year", main="yearly")
		timeplot.ecdf (x, cu="week", main="weekly")
		timeplot.ecdf (x, cu="day", main="daily")
	}
	else
	{	timeplot.line (x, y, main="overall")
		timeplot.line (x, y, cu="year", main="yearly")
		timeplot.line (x, y, cu="week", main="weekly")
		timeplot.line (x, y, cu="day", main="daily")
	}
	par (op)
}

timeplot.ecdf = function (x, cu=NULL, ...)
{	if (is.null (cu) )
	{	plot (ecdf (x), axes=FALSE, ...)
		timebox (1:2, x, ...)
	}
	else
	{	x = rtv.cp (x, cu)
		plot (ecdf (x), ...)
	}
}

timeplot.dot = function (x, ...)
{	g = rep (0, length (x) )
	timeplot.group (g, x, ...)
}

timeplot.group = function (g, x, cu=NULL, jitter=TRUE, ...)
{	g = as.factor (g)
	gg = levels (g)
	ng = length (gg)
	nx = length (x)
	u = as.integer (g)
	v = if (jitter) u + runif (nx, -0.35, 0.35) else u
	if (! is.null (cu) ) x = rtv.cp (x, cu)
	plot.default (v, x, xlab="", axes=FALSE, ...)
	box ()
	if (ng > 1) axis (1, 1:ng, gg)
	if (is.null (cu) ) timeaxis (2, x, ...)
	else axis (2)
}

timeplot.line = function (x, y, cu=NULL, sort=TRUE, ...)
{	if (! is.null (cu) ) x = rtv.cp (x, cu)
	if (sort)
	{	i = order (x)
		x = x [i]
		y = y [i]
	}
	if (is.null (cu) )
	{	plot.default (x, y, type="l", axes=FALSE, ...)
		timebox (1:2, x, ...)
	}
	else
		plot.default (x, y, type="l", ...)
}

timebox = function (side, x, ...)
{	box ()
	timeaxis (side [1], x, ...)
	if (length (side) > 1)
		axis (side [2])
}

timeaxis = function (side, x, n=5, date=getOption ("rtv.plot.date"), ...)
{	x = timeseq (range (x, na.rm=TRUE), n=n, origin=attr (x, "origin"), unit=attr (x, "unit") )
	axis (side, x, timestring (x, date, ...) )
}

