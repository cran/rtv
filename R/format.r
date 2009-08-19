format.rtv = function (x, ...)
{	if (getOption ("rtv.explicit.format") ) timestring (x, ...)
	else x
}

timestring = function (x,
	date=getOption ("rtv.print.date"),
	outformat=timestring.format (date), ...)
	format (as.POSIXlt (x), outformat)

timestring.format = function (date=TRUE)
{	if (date) getOption ("rtv.default.format.short")
	else getOption ("rtv.default.format.long")
}

monthstring = function (x, ...)
{	mn = c ("January", "February", "March", "April",
		"May", "June", "July", "August",
		"September", "October", "November", "December")
	if (is.rtv (x) ) x = as.drtv (x)$month
	unitstring (x, mn, ...)
}

dowstring = function (x, ...)
{	dn = c ("Monday", "Tuesday", "Wednesday", "Thursday",
		"Friday", "Saturday", "Sunday")
	if (is.rtv (x) ) x = as.drtv (x)$dow
	unitstring (x, dn, ...)
}

unitstring = function (x, levs, case="title", nletters=3)
{	if (case == "lower") levs = tolower (levs)
	else if (case == "upper") levs = toupper (levs)
	if (! is.na (nletters) ) levs = substr (levs, 1, nletters)
	levs [x]
}

