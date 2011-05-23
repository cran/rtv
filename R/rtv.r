is.rtv = function (x) inherits (x, "rtv")
as.Date.rtv = function (x, ...) as.Date (as.POSIXlt (x) )
as.POSIXct.rtv = function (x, ...) as.POSIXct (as.POSIXlt (x) )
as.POSIXlt.rtv = function (x, ...)
{	x = drtv (x)
	strptime (paste (x$year, x$month, x$day, x$hour, x$minute, x$second),
		"%Y %m %d %H %M %OS", tz="GMT")
}

drtvs = function (x, ...) drtv.character (x, ..., hour=0)
drtvx = function (x, ...) drtv.character (x, ..., date=FALSE)
crtvs = function (x, ...) crtv.character (x, ..., hour=0)
crtvx = function (x, ...) crtv.character (x, ..., date=FALSE)
crtvc = function (x, origin=x$origin, unit=x$unit)
	crtv.crtv (x, origin, unit)

c.drtv = function (x, ...)
{	for (obj in list (...) )
	{	obj = drtv (obj)
		for (i in 1:length (x) )
			x [i] = c (x [i], obj [i])
	}
	x
}

c.crtv = function (x, ...)
{	q = attributes (x)
	for (obj in list (...) )
		x = c (unclass (x), unclass (crtv (obj, origin=x$origin, unit=x$unit) ) )
	attributes (x) = q
	x
}

print.rtv = function (x, ...)
{	if (getOption ("rtv.format") ) print (drtvf (x, ...) ) 
	else if (is.drtv (x) ) print (data.frame (unclass (x) ) ) 
	else
	{	print (as.numeric (x) )
		cat ('(origin=\"', drtvf (x$origin), '\", unit=\"', x$unit, '\")\n', sep='')
	}
}

drtvf = function (x, ...) format (drtv (x), ...)

format.drtv = function (x, date=getOption ("rtv.date"), style, ...)
{	style = .rtv.style (date, style)
	format (as.POSIXlt (x), style)
}

as.character.drtv = function (x, ...) format (x, ...)

.rtv.style = function (date, style)
{	if (missing (style) )
	{	if (date) getOption ("rtv.styled")
		else getOption ("rtv.stylex")
	}
	else style
}

length.drtv = function (x, ...) length (x [[1]])
length.crtv = function (x, ...) length (unclass (x) )

crtvcp = function (x)
{	y = as.numeric (x)
	y - floor (y)
}

sort.rtv = function (x, ...)
{	i = order (x, ...)
	x [i]
}

sample.rtv = function (x, ...)
{	i = sample (1:length (x), ...)
	x [i]
}

order.drtv = function (x, by.unit=getOption ("rtv.unit"), by.cp=FALSE, ...)
	order (crtv (x, unit=by.unit) )

order.crtv = function (x, by.unit=x$unit, by.cp=FALSE, ...)
{	y = crtv (x, x$origin, by.unit)
	if (by.cp) y = crtvcp (y)
	order (y)
}

"[.drtv" = function (x, i)
{	for (j in 1:8)	x [[j]] = x [[j]][i]
	x
}

"[.crtv" = function (x, i)
{	y = unclass (x) [i]
	attributes (y) = attributes (x)
	y
}


