# Create a new OpenOffice.org Base from a template
# Author : Sylvain Mareschal
odb.create = function(
		odbFile,
		template = NULL,
		overwrite = c("warning", "do", "skip", "stop")
		)
	{
	# Args matching
	overwrite = match.arg(overwrite)
	
	# Checks
	if (is.null(template)) {
		template = system.file("tools/template.odb", package="ODB")
	}
	if (!file.exists(template)) {
		stop(call.=FALSE, "'template' file doesn't exist, it must be an .odb file")
	}
	
	# Overwriting
	if (file.exists(odbFile)) {
		if (overwrite == "warning")   {
			over = TRUE
			warning(call.=FALSE, "'odbFile' has been overwritten")
		} else if (overwrite == "stop") {
			over = FALSE
			stop(call.=FALSE, "'odbFile' already exists")
		} else if (overwrite == "skip") {
			over = FALSE
		} else if (overwrite == "do") {
			over = TRUE
		}
	} else {
		over = FALSE
	}
	
	# Copy
	invisible(
		file.copy(from=template, to=odbFile, overwrite=over)
	)
}
