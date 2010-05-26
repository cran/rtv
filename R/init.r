.conflicts.OK = 0

#order = function (x, ...) UseMethod ("order")
#order.default = function (x, ...) base::order (x, ...)
sample = function (...) UseMethod ("sample")
sample.default = function (x, ...) base::sample (x, ...)

.First.lib = function (...) rtvo.reset ()

rtvo.reset = function ()
{	str = "%Y-%m-%d %H:%M:%OS"
	options (rtv.origin=drtv ("2000-01-01 06:00:00", style=str) )
	options (rtv.unit="day")
	rtvo.date ()
	rtvo.format ()
	options (rtv.styled="%Y-%m-%d")
	options (rtv.stylex=str)
}

rtvo.date = function (date=TRUE) options (rtv.date=date)
rtvo.format = function (format=FALSE) options (rtv.format=format)








