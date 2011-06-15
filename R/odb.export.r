# Export an OpenOffice.org Base to a SQL file
# Author : Sylvain Mareschal
odb.export = function(
		odb,
		file
		)
	{
	# Class checks
	if (!is(odb, "odb")) {
		stop("'odb' must be an 'odb' object")
	}
	validObject(odb)
	
	# HSQLDB export
	odb.write(
		odb,
		paste(
			"SCRIPT '",
			gsub("'", "\'", file),
			"'",
			sep = ""
		)
	)
	
	invisible(TRUE)
}
