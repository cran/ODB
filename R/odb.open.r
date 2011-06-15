# Check and open a connection to an OpenOffice.org database (odb) file
# Author : Sylvain Mareschal
odb.open = function(
		odbFile,
		jarFile = NULL
		)
	{
	# File Checks
	if (is.null(jarFile)) {
		jarFile = system.file("tools/hsqldb.jar", package="ODB")
	}
	if (!file.exists(jarFile)) {
		stop(call.=FALSE, "File 'jarFile' does not exist")
	}
	if (!file.exists(odbFile)) {
		stop(call.=FALSE, "File 'odbFile' does not exist")
	}
	
	# Temporary directory
	directory = tempfile()
	dir.create(directory)
	
	# Extracts HSQLDB files
	dbFiles = c("backup", "data", "properties", "script")
	status = list()
	for(f in dbFiles) {
		status[[f]] = tryCatch(
			unzip(
				zipfile = odbFile,
				files = paste("database", f, sep="/"),
				exdir = directory
			),
			warning = function(w) { return(w) },
			error = function(e) { return(e) }
		)
	}
	for(f in dbFiles[3:4]) {
		if (!"character" %in% class(status[[f]])) {
			stop(call.=FALSE, "'", f, "' unziping from 'odbFile' failed : ", conditionMessage(status[[f]]))
		}
	}
	
	# Extracts content.xml
	status = tryCatch(
		unzip(
			zipfile = odbFile,
			files = "content.xml",
			exdir = directory
		),
		warning = function(w) { return(w) },
		error = function(e) { return(e) }
	)
	if (!"character" %in% class(status)) {
		stop(call.=FALSE, "'content.xml' unziping from 'odbFile' failed : ", conditionMessage(status))
	}
	
	# HSQLDB version of JAR file
	tryCatch(
		unzip(
			zipfile = jarFile,
			files = "META-INF/MANIFEST.MF",
			exdir = directory
		),
		warning = function(w) {
			stop(call.=FALSE, "While unziping MANIFEST from 'jarFile' : \"", conditionMessage(w), "\"")
		}
	)
	jarVersion = scan(
		file = paste(directory, "META-INF/MANIFEST.MF", sep="/"),
		what = "",
		sep = "\n",
		quiet = TRUE
	)
	if (!"Specification-Title: HSQLDB" %in% jarVersion) {
		stop(call.=FALSE, "File 'jarFile' seems to not be a .jar library for HSQLDB")
	}
	jarVersion = grep("^Specification-Version: ([0-9\\.]+)$", jarVersion, value=TRUE)
	jarVersion = gsub("^Specification-Version: ([0-9\\.]+)$", "\\1", jarVersion)
	unlink(paste(directory, "META-INF", sep="/"), recursive=TRUE)
	
	# HSQLDB version of ODB file
	odbVersion = scan(
		file = paste(directory, "database/properties", sep="/"),
		what = "",
		sep = "\n",
		quiet = TRUE
	)
	odbVersion = grep("^#HSQL Database Engine ([0-9\\.]+)$", odbVersion, value=TRUE)
	odbVersion = gsub("^#HSQL Database Engine ([0-9\\.]+)$", "\\1", odbVersion)
	if (odbVersion != jarVersion) {
		stop(call.=FALSE, "'odbFile' (", odbVersion, ") and 'jarFile' (", jarVersion, ") HSQLDB versions don't match")
	}
	
	# Renames HSQLDB files
	dbFiles.odb = paste(directory, "/database/", dbFiles, sep="")
	dbFiles.jdbc = paste(directory, "/database/ODB.", dbFiles, sep="")
	for(i in 1:length(dbFiles)) {
		if (file.exists(dbFiles.odb[i])) {
			file.rename(dbFiles.odb[i], dbFiles.jdbc[i])
		}
	}
	
	# JDBC driver
	tryCatch(
		driver <- JDBC(
			classPath = jarFile,
			driverClass = "org.hsqldb.jdbcDriver",
			identifier.quote = '"'
		),
		error = function(e) {
			stop(call.=FALSE, "While setting up JDBC driver : \"", conditionMessage(e), "\"")
		}
	)
	
	# DBI connection
	tryCatch(
		connection <- dbConnect(driver, paste("jdbc:hsqldb:file:", directory, "/database/", "ODB", sep="")),
		error = function(e) {
			stop(call.=FALSE, "While establishing DBI connection : \"", conditionMessage(e), "\"")
		}
	)
	
	# S4 Object to return
	odb = new(
		Class = "odb",
		connection,
		directory = directory,
		odbFile = odbFile,
		odbVersion = odbVersion,
		jarFile = jarFile,
		jarVersion = jarVersion
	)
		
	return(odb)
}
