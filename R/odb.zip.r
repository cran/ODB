# Add "files" in a ZIP "archive" using the zip shell command (theoretically available on unix and windows)
# path in "files" will be included in the archive, path in "from" won't. "from" doesn't apply to "archive"
# Author : Sylvain Mareschal
odb.zip = function(
		archive,
		files,
		from = ".",
		recursive = TRUE,
		quiet = FALSE
		)
	{
	# Working directory
	if (from != ".") {
		# Absolute path
		archive = normalizePath(archive)
		
		# Shift of WD
		workingDirectory = getwd()
		setwd(from)
		on.exit(setwd(workingDirectory))
	}
	
	# Command building
	if (recursive) {
		option = "-r "
	} else {
		option = ""
	}
	command = sprintf("zip %s%s %s",
		option,
		paste("\"", archive, "\"", sep=""),
		paste("\"", paste(files, collapse="\" \""), "\"", sep="")
	)
	
	# Command execution
	system(command, intern=quiet)
	
	invisible(TRUE)
}
