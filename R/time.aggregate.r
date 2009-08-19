aggregate.rtv = function (x, ...) time.aggregate (x, ...)
aggregate.time.frame = function (x, ...) time.aggregate (x, ...)

time.aggregate = function (x, unit, fs=NULL, labs=fs, period=TRUE, exhaustive=FALSE, ...)
{	p = whichperiod (when (x), unit, ...)
	y = split (x, p)
	n = length (y)
	if (n == 0) NULL
	else if (is.null (fs) ) y
	else
	{	d = time.aggregate.apply (y [[1]], fs)
		if (n > 1)
		{	for (i in 2:n)
				d = rbind (d, time.aggregate.apply (y [[i]], fs) )
		}
		names (d) = labs
		if (period)
		{	d = cbind (names (y), d)
			names (d) [1] = "period"
		}
		d
	}
}

time.aggregate.apply = function (x, fs)
{	d = list ()
	for (i in 1:length (fs) )
	{	if (is.rtv (x) ) d [[i]] = eval (parse (text=fs [i]) ) (x)
		else d [[i]] = eval (parse (text=fs [i]), x)
	}
	names (d) = NA
	as.data.frame (d)
}

whichperiod = function (x, unit, format=FALSE, natural=FALSE, ...)
{	y = floor (crtv (x, unit=unit, clone=TRUE) )
	if (format) timestring (y, ...)
	else if (natural) as.integer (as.factor (y) )
	else y
}

