plot.rtv = function (x, y, ..., at, cycle)
{	if (is.drtv (x) ) x = crtv (x)
	if (!missing (cycle) )
	{	x = as.numeric (crtv (x, x$origin, cycle) )
		x = x - floor (x)
	}
	i = order (x)
	x = x [i]
	if (missing (y) ) .rtv.ecdf (x, at=at, ...)
	else
	{	y = y [i]
		if (is.crtv (x) ) .rtv.lines (x, y, at=at, ...)
		else plot (x, y, type="l", ...)
	}
}

timeaxis = function (side, x, ...,  at, n=7)
{	if (missing (at) )
	{	k = range (x)
		at = seq (k [1], k [2], n)
	}
	else attributes (at) = attributes (x)
	axis (side, at, drtvf (at, ...) )
}
	
.rtv.ecdf = function (x, at, ...)
{	n = length (x) - 1
	F = (0:n) / n
	plot.default (x, F, type="l", axes=FALSE, ...)
	box ()
	timeaxis (1, x, at=at)
	axis (2)
}

.rtv.lines = function (x, y, at, ...)
{	plot.default (x, y, type="l", axes=FALSE, ...)
	box ()
	timeaxis (1, x, at=at)
	axis (2)
}

