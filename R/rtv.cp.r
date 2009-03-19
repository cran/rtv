rtv.cp = function (x, unit)
{	if (missing (unit) ) unit = attr (x, "unit")
	x = as.numeric (crtv (x, unit=unit, clone=TRUE) )
	x - floor (x)
}

rtv.tod = function (x) rtv.cp (x, "day")

