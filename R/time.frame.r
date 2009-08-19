time = function (...) UseMethod ("time")

time.frame = function (...)
{	d = data.frame (...)
	class (d) = c ("time.frame", class (d) )
	d
}

as.data.frame.rtv = function (x, ...)
{	if (is.drtv (x) )
	{	x = crtv (x)
		warning ("drtv object converted to crtv object for data.frame")
	}
	class (x) = c ("AsIs", class (x) )
	y = list (x)
	attr (y, "row.names") = 1:length (x)
	y
}

when = function (x)
{	if (is.rtv (x) ) x
	else
	{	y = NULL
		for (i in length (x):1)
		{	v = x [[i]]
			if (is.rtv (v) ) y = v
		}
		y
	}
}

