crtv = function (...)
	UseMethod ("crtv")

crtv.drtv = function (x, ...)
	crtv (paste (x$year, "-", x$month, "-", x$day, " ",
	x$hour, ":", x$minute, ":", x$second, sep=""), FALSE, ...)

crtv.crtv = function (x, origin=NULL, unit=NULL, clone=FALSE, relative=FALSE, ...)
{	if (relative) origin = min (as.POSIXct (x), na.rm = TRUE)
	if (clone)
	{	if (is.null (origin) ) origin = attr (x, "origin")
		if (is.null (unit) ) unit = attr (x, "unit")
	}
	else
	{	if (is.null (origin) ) origin = getOption ("rtv.default.origin")
		if (is.null (unit) ) unit = getOption ("rtv.default.unit")
	}
	if (origin == attr (x, "origin") && unit == attr (x, "unit") ) x
	else crtv (drtv (x), origin=origin, unit=unit)
}

crtv.Date = function (x, ...)
	crtv (as.POSIXct (x), ...)

crtv.POSIXlt = function (x, ...)
	crtv (as.POSIXct (x), ...)

crtv.POSIXct = function (x, relative=FALSE, origin=getOption ("rtv.default.origin"),
	unit=getOption ("rtv.default.unit"), ...)
{	if (relative) origin = min (x, na.rm = TRUE)
	origin = as.POSIXct (origin)
	y = if (unit == "year") implode.year (x) - implode.year (origin)
	else if (unit == "month") implode.month (x) - implode.month (origin)
	else if (unit == "week") implode.homotime (x, origin, 604800)
	else if (unit == "day") implode.homotime (x, origin, 86400)
	else if (unit == "hour") implode.homotime (x, origin, 3600)
	else if (unit == "minute") implode.homotime (x, origin, 60)
	else if (unit == "second") implode.homotime (x, origin, 1)
	else stop ("unknown time unit")
	crtv.default (y, origin=origin, unit=unit)
}

crtv.character = function (x, date=getOption ("rtv.read.date"), informat=timestring.format (date), ...)
	crtv (strptime (x, informat, tz="GMT"), ...)

crtv.default = function (x, origin=getOption ("rtv.default.origin"),
	unit=getOption ("rtv.default.unit"), ...)
	structure (x, class=c ("crtv", "rtv"), "origin"=as.POSIXct (origin), "unit"=unit)

is.crtv = function (x) inherits (x, "crtv")
as.crtv = function (x) if (is.crtv (x) ) x else (crtv (x) )

implode.homotime = function (x, origin, fac)
	(as.numeric (as.POSIXct (x) ) - as.numeric (origin) ) / fac

implode.year = function (x)
{	v = drtv (x, FALSE)
	dp = implode.day (v$hour, v$minute, v$second)
	v$year + (v$doy + dp - 1) / ndays.year (v$year) 
}

implode.month = function (x)
{	v = drtv (x, FALSE)
	dp = implode.day (v$hour, v$minute, v$second)
	12 * v$year + v$month + (v$day + dp - 1) / ndays.month (v$year, v$month)
}

implode.day = function (hour, minute, second)
	hour / 24 + minute / 1440 + second / 86400

round.crtv = function (x, ...) timesweep (round, x, ...)

"[.crtv" = function (x, i)
{	y = "[.numeric_version" (x, i)
	attributes (y) = attributes (x)
	y
}


