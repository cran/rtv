is.leap = function (year)
{	x1 = strptime (paste (year, "/02/28/23", sep=""), "%Y/%m/%d/%H", tz="GMT")
	x2 = strptime (paste (year, "/03/01/01", sep=""), "%Y/%m/%d/%H", tz="GMT")
	(difftime (x2, x1, tz="GMT", units="days") > 1)
}

ndays.year = function (year)
{	y = rep (NA, length (year) )
	i = is.leap (year)
	y [i] = 366
	y [!i] = 365
	y
}

ndays.month = function (year, month)
{	d = cbind (year, month)
	y = rep (NA, nrow (d) )
	i = is.leap (d [,1])
	j1 = (d [,2] == 2)
	j2 = (d [,2] == 4 | d [,2] ==  6 | d [,2] == 9 | d [,2] == 11)
	y [i & j1] = 29
	y [!i & j1] = 28
	y [j2] = 30
	y [!(j1 | j2)] = 31
	y
}

date2dow = function (year, month, day)
{	dow = strptime (paste (year, month, day, "6"), "%Y %m %d %H", tz="GMT")$wday
	dow [dow == 0] = 7
	dow
}

date2doy = function (year, month, day)
	.cumdays.month (year, month, FALSE) + day

doy2date = function (year, doy)
{	d = cbind (year, doy)
	n = nrow (d)
	month = day = numeric (n)
	for (i in 1:n)
	{	ndays = cumsum (c (0, ndays.month (d [i, 1], 1:11) ) )
		month [i] = sum (ndays < d [i, 2])
		day [i] = d [i, 2] - ndays [month [i] ]
	}
	list (month=month, day=day)
}

formatmonth = function (x, ...)
{	mn = c ("January", "February", "March", "April",
		"May", "June", "July", "August",
		"September", "October", "November", "December")
	if (is.rtv (x) ) x = drtv (x)$month
	formatdtu (x, mn, ...)
}

formatdow = function (x, ...)
{	dn = c ("Monday", "Tuesday", "Wednesday", "Thursday",
		"Friday", "Saturday", "Sunday")
	if (is.rtv (x) ) x = drtv (x)$dow
	formatdtu (x, dn, ...)
}

formatdtu = function (x, levels, case="title", nchars=3)
{	if (case == "lower") levels = tolower (levels)
	else if (case == "upper") levels = toupper (levels)
	if (!is.na (nchars) ) levels = substr (levels, 1, nchars)
	levels [x]
}

.cumdays.month = function (year, month, inclusive=TRUE)
{	d = cbind (year, month)
	n = nrow (d)
	y = rep (NA, n)
	for (i in 1:n)
		if (!any (is.na (d [i,]) ) )
		{	ndays = if (inclusive) ndays.month (d [i, 1], 1:12)
			else c (0, ndays.month (d [i, 1], 1:11) )
			y [i] = sum (ndays [1:d [i, 2] ])
		}
	y
}

