.onLoad = function (...) rtv.reset ()

rtv.reset = function ()
{	options (rtv.explicit.format=FALSE)
	options (rtv.read.date=TRUE)
	options (rtv.print.date=FALSE)
	options (rtv.plot.date=TRUE)
	options (rtv.default.origin=as.POSIXct (strptime ("2000-01-01 00:00:00", "%Y-%m-%d %H:%M:%OS", tz="GMT") ) )
	options (rtv.default.unit="day")
	options (rtv.default.format.short="%Y-%m-%d")
	options (rtv.default.format.long="%Y-%m-%d %H:%M:%OS")
}

