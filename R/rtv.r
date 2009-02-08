rtv = function (...) drtv (...)

is.rtv = function (x) inherits (x, "rtv")
as.rtv = function (x) if (is.rtv (x) ) x else drtv (x)

as.Date.rtv = function (x, ...) as.Date (as.POSIXlt (x) )
as.POSIXct.rtv = function (x, ...) as.POSIXct (as.POSIXlt (x) )
as.POSIXlt.rtv = function (x, ...)
{	x = drtv (x)
	tstr = paste (x$year, "-", x$month, "-", x$day, " ",
		x$hour, ":", x$minute, ":", x$second, sep="")
	strptime (tstr, "%Y-%m-%d %H:%M:%OS", tz="GMT")
}

as.double.rtv = function (x, std = FALSE, ...)
{	if (std) x = crtv (x)
	else x = as.crtv (x)
	class (x) = NULL
	as.numeric (x)
}

length.rtv = function (x, ...)
{	if (inherits (x, "drtv") )
		length (x [[1]])
	else
	{	class (x) = NULL
		length (x)
	}
}

sort.rtv = function (x, ...) timesweep (sort, x, ...)
sample.rtv = function (x, ...) timesweep (sample, x, ...)
mean.rtv = function (x, ...) timesweep (mean, x, ...)
min.rtv = function (x, ...) timesweep (min, x, ...)
max.rtv = function (x, ...) timesweep (max, x, ...)
diff.rtv = function (x, ...) diff (as.numeric (x), ...)

range.rtv = function (x, diff=FALSE, ...)
{	y = timesweep (range, x, ...)
	if (diff) diff (y)
	else y
}

subset.rtv = function (x, v, ...) x [v]

timesweep = function (f, x, ...)
{	z = as.crtv (x)
	y = crtv.default (f (as.numeric (z), ...), attr (z, "origin"), attr (z, "unit") )
	if (inherits (x, "drtv") ) drtv (y)
	else y
}

"+.rtv" = function (x1, x2=NULL)
{	if (is.null (x2) ) x1
	else if (is.rtv (x1) && is.rtv (x2) ) stop ("rtv.object + rtv.object not allowed")
	else if (is.rtv (x1) ) rtv.incr (x1, x2)
	else rtv.incr (x2, x1)
}

"-.rtv" = function (x1, x2=NULL)
{	if (is.null (x2) || is.rtv (x2) ) stop ("- rtv.object not allowed")
	else rtv.incr (x1, -1 * x2)
}

rtv.incr = function (x1, x2, unit=attr (x1, "unit"))
{	if (is.null (unit) ) unit = "day"
	y = crtv (as.numeric (crtv (x1, unit=unit) ) + x2, unit=unit)
	if (is.drtv (x1) ) drtv (y)
	else crtv (y, unit=attr (x1, "unit"), origin=attr (x1, "origin") )
}

timeseq = function (x1, x2=NULL, n, ...)
{	if (is.null (x2) ) x2 = x1 [2]
	x = seq (as.numeric (x1 [1], TRUE), as.numeric (x2 [1], TRUE), length.out=n)
	crtv (crtv (x), ...)
}

c.rtv = function (...)
{	seed = list (...)
	target = numeric ()
	for (i in 1:length (seed) )
	{	obj = seed [[i]]
		obj = if (is.rtv (obj) ) as.numeric (obj, std=TRUE)
		else as.numeric (obj)
		target = c (target, obj)
	}
	target = crtv (target)
	if (is.drtv (seed [[1]]) ) drtv (target)
	else crtv (target, origin=attr (seed [[1]], "origin"), unit=attr (seed [[1]], "unit") )
}

print.rtv = function (x, date=getOption ("rtv.print.date"), ...)
	print.default (format (x, date) )

