drtv = function (...) UseMethod ("drtv")
drtv.drtv = function (x, ...) x

drtv.crtv = function (x, ...)
{	k = x$unit
	if (k == "year") .explode.year (x, ...)
	else if (k == "month") .explode.month (x, ...)
	else if (k == "week") .explode.homotime (x, 604800, ...)
	else if (k == "day") .explode.homotime (x, 86400, ...)
	else if (k == "hour") .explode.homotime (x, 3600, ...)
	else if (k == "minute") .explode.homotime (x, 60, ...)
	else if (k == "second") .explode.homotime (x, 1, ...)
	else stop ("crtv object has invalid unit")
}

drtv.Date = function (x, ..., hour=6)
{	x = drtv (as.POSIXlt (x, tz="GMT"), ...)
	x$hour = rep (hour, length (x) )
	x
}

drtv.POSIXlt = function (x, ...)
{	dow = x$wday
	dow [dow == 0] = 7
	drtv (x$year + 1900, x$mon + 1, x$mday, x$hour, x$min, x$sec,
		dow, x$yday + 1, validate=FALSE, round=FALSE, ...)
}

drtv.POSIXct = function (x, ...) drtv (as.POSIXlt (x, tz="GMT"), ...)

drtv.character = function (x, ..., date=TRUE, hour=6, style)
{	if (missing (style) )
	{	if (date)
		{	y = drtv (strptime (x, getOption ("rtv.styled"), tz="GMT") )
			y$hour = rep (hour, length (y) )
			y
		}
		else drtv (strptime (x, getOption ("rtv.stylex"), tz="GMT") )
	}
	else drtv (strptime (x, style, tz="GMT") )
}

drtv.default = function (year=2000, month=1, day=1, hour=6, minute=0, second=0,
	dow=1, doy=1, ..., validate=TRUE, round=FALSE)
{	x = data.frame (year, month, day, hour, minute, second, dow, doy)
	if (validate) for (i in 1:nrow (x) ) if (any (is.na (x [i,]) ) ) x [i,] = NA
	x = extend (as.list (x), c ("drtv", "rtv") )
	if (round) x = .round.drtv (x)
	if (validate)
	{	x$dow = date2dow (x$year, x$month, x$day)
		x$doy = date2doy (x$year, x$month, x$day)
	}
	x
}

is.drtv = function (x) inherits (x, "drtv")

rep.drtv = function (x, times, ...)
{	for (i in 1:length (unclass (x) ) ) x [[i]] = rep (x [[i]], times)
	x
}

.explode.homotime = function (x, k, ...)
	drtv (as.POSIXct (x$origin) + k * as.numeric (x), ...)

.explode.year = function (x, ...)
{	s1 = .num.split (.implode.year (x$origin) + as.numeric (x) )
	s2 = .num.split (ndays.year (s1 [[1]]) * s1 [[2]] + 1)
	d = doy2date (s1 [[1]], s2 [[1]])
	t = .explode.day (s2 [[2]])
	drtv (s1 [[1]], d$month, d$day, t$hour, t$minute, t$second, ...)
}

.explode.month = function (x, ...)
{	s1 = .num.split (.implode.month (x$origin) + as.numeric (x) - 1)
	year = floor (s1 [[1]] / 12)
	month = s1 [[1]] %% 12 + 1
	s2 = .num.split (ndays.month (year, month) * s1 [[2]] + 1)
	t = .explode.day (s2 [[2]])
	drtv (year, month, s2 [[1]], t$hour, t$minute, t$second, ...)
}

.explode.day = function (x)
{	x = floor (x * 864000000)
	hour = floor (x / 36000000)
	ind = (!is.na (x) & hour > 0)
	x [ind] = x [ind] %% (36000000 * hour [ind])
	minute = floor (x / 600000)
	second = (x - 600000 * minute) / 10000
	list (hour=hour, minute=minute, second=second)
}

.round.drtv = function (x, ...)
{	m = cbind (13, ndays.month (x$month, x$year) + 1, 24, 60)	
	x [[6]] = round (x [[6]], 3)
	ind = (!is.na (x [[6]]) & x [[6]] >= 59.999)
	x [[5]][ind] = x [[5]][ind] + 1
	x [[6]][ind] = 0
	for (i in 5:2)
	{	j = i - 1
		ind = (!is.na (x [[i]]) & x [[i]] >= m [,j])
		x [[j]][ind] = x [[j]][ind] + 1
		x [[i]][ind] = 0
	}
	x
}

.num.split = function (x)
{	xint = floor (x)
	xfrac = x - xint
	list (xint, xfrac)
}



