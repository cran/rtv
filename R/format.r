format.rtv = function (x, ...)
{	if (getOption ("rtv.explicit.format") ) explicit.format (x, ...)
	else x
}

explicit.format = function (x, date=TRUE, outformat=default.format (date), ...)
	format (as.POSIXlt (drtv (x) ), outformat)

default.format = function (date=TRUE)
{	if (date) getOption ("rtv.default.format.short")
	else getOption ("rtv.default.format.long")
}

month.string = function (x, ...)
{	mn = c ("January", "February", "March", "April",
		"May", "June", "July", "August",
		"September", "October", "November", "December")
	if (is.rtv (x) ) x = as.drtv (x)$month
	unit.string (x, mn, ...)
}

dow.string = function (x, ...)
{	dn = c ("Monday", "Tuesday", "Wednesday", "Thursday",
		"Friday", "Saturday", "Sunday")
	if (is.rtv (x) ) x = as.drtv (x)$dow
	unit.string (x, dn, ...)
}

unit.string = function (x, levs, case="title", nletters=3)
{	if (case == "lower") levs = tolower (levs)
	else if (case == "upper") levs = toupper (levs)
	if (! is.na (nletters) ) levs = substr (levs, 1, nletters)
	levs [x]
}

timeaxis = function (side, x, n=5, ...)
{	x = timeseq (min (x, na.rm=TRUE), max (x, na.rm=TRUE), n)
	axis (side, x, explicit.format (x, ...) )
}

