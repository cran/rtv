crtv = function (...) UseMethod ("crtv")

crtv.drtv = function (x, origin, unit, ...)
{	if (missing (origin) ) origin=getOption ("rtv.origin")
	if (missing (unit) ) unit=getOption ("rtv.unit")
	origin = drtv (origin)
	if (unit == "month")
	{	y = .implode.month (x) - .implode.month (origin)
		crtv (unclass (y), origin=origin, unit=unit)
	}
	else if (unit == "year")
	{	y = .implode.year (x) - .implode.year (origin)
		crtv (unclass (y), origin=origin, unit=unit)
	}
	else crtv (as.POSIXct (x), origin, unit)
}

crtv.crtv = function (x, origin, unit, ...)
{	if (.oreq (x$origin, origin) && x$unit == unit) x
	else crtv (drtv (x), origin, unit)
}

crtv.Date = function (x, origin, unit, ..., hour=6)
	crtv (drtv (x, hour=hour), origin, unit)

crtv.POSIXlt = function (x, origin, unit, ...)
	crtv (as.POSIXct (x, tz="GMT"), origin, unit)

crtv.POSIXct = function (x, origin, unit, ...)
{	if (missing (origin) ) origin=getOption ("rtv.origin")
	if (missing (unit) ) unit=getOption ("rtv.unit")
	origin = drtv (origin)
	if (unit == "month" || unit == "year")
		crtv (drtv (x), origin, unit)
	else
	{	k = as.POSIXct (origin)
		y = if (unit == "week") .implode.homotime (x, k, 604800)
		else if (unit == "day") .implode.homotime (x, k, 86400)
		else if (unit == "hour") .implode.homotime (x, k, 3600)
		else if (unit == "minute") .implode.homotime (x, k, 60)
		else if (unit == "second") .implode.homotime (x, k, 1)
		else stop ("unknown time unit")
		crtv (unclass (y), origin=origin, unit=unit)
	}
}

crtv.character = function (x, origin, unit, ..., date=TRUE, hour=6, style)
	crtv (drtv (x, date=date, hour=hour, style=style), origin, unit)

crtv.default = function (x, origin, unit, ...)
{	if (missing (origin) ) origin=getOption ("rtv.origin")
	if (missing (unit) ) unit=getOption ("rtv.unit")
	extend (REALv (x), c ("crtv", "rtv"), origin, unit)
}

is.crtv = function (x) inherits (x, "crtv")

rep.crtv = function (x, times, ...) .timesweep (rep, x, times, ...)
mean.crtv = function (x, ...) .timesweep (mean, x, ...)
range.crtv = function (x, ...) .timesweep (range, x, ...)
min.crtv = function (x, ...) .timesweep (min, x, ...)
max.crtv = function (x, ...) .timesweep (max, x, ...)
round.crtv = function (x, ...) .timesweep (round, x, ...)
floor.crtv = function (x, ...) .timesweep (floor, x, ...)
ceiling.crtv = function (x, ...) .timesweep (ceiling, x, ...)

seq.crtv = function (a, b, n, ...)
{	if (is.crtv (a) && is.crtv (b) )
	{	if (.oreq (a$origin, b$origin) && a$unit == b$unit)
		{	x = seq (unclass (a), unclass (b), length=n)
			attributes (x) = attributes (a)
			x
		}
		else stop ("seq.crtv, only allowed if same origin and unit")
	}
	else stop ("seq.crtv requires two crtv objects")
}

"+.crtv" = function (a, b)
{	if (missing (b) ) a
	else if (is.crtv (a) && is.crtv (b) )
	{	x = unclass (a) + unclass (b)
		attributes (x) = attributes (a)
		x
	}
	else if (is.crtv (a) ) .timesweep (`+`, a, b)
	else .timesweep (`+`, b, a)
}

"-.crtv" = function (a, b) if (missing (b) ) -1 * a else a + -1 * b

.implode.homotime = function (x, origin, fac)
	(as.numeric (x) - as.numeric (origin) ) / fac

.implode.year = function (v)
{	dp = .implode.day (v$hour, v$minute, v$second)
	v$year + (v$doy + dp - 1) / ndays.year (v$year) 
}

.implode.month = function (v)
{	dp = .implode.day (v$hour, v$minute, v$second)
	12 * v$year + v$month + (v$day + dp - 1) / ndays.month (v$year, v$month)
}

.implode.day = function (hour, minute, second)
	hour / 24 + minute / 1440 + second / 86400

.timesweep = function (f, x, ...)
{	y = f (unclass (x), ...)
	attributes (y) = attributes (x)
	y
}

.oreq = function (k1, k2) drtvf (k1) == drtvf (k2)




