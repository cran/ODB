# "odb" class
# Author : Sylvain Mareschal

setClass(
	Class = "odb",
	representation = representation(
		"JDBCConnection",
		directory = "character",
		odbFile = "character",
		odbVersion = "character",
		jarFile = "character",
		jarVersion = "character"
	),
	prototype = prototype(),
	validity = function(object) {
		returns = character(0)
		
		closed = isClosed(object)
		if (closed) {
			returns = c(returns, "DBI connection is closed")
		}
		
		if (length(object@directory) != 1 | is.na(object@directory)) {
			returns = c(returns, "'directory' must be single non-NA character")
		} else if (!closed & !file.exists(object@directory)) {
			returns = c(returns, "'directory' must be an existing directory")
		}
		
		if (length(object@odbFile) != 1 | is.na(object@odbFile)) {
			returns = c(returns, "'odbFile' must be single non-NA character")
		} else if (!file.exists(object@odbFile)) {
			returns = c(returns, "'odbFile' must be an existing file")
		}
		
		if (length(object@odbVersion) != 1 | is.na(object@odbVersion)) {
			returns = c(returns, "'odbVersion' must be single non-NA character")
		}
		
		if (length(object@jarFile) != 1 | is.na(object@jarFile)) {
			returns = c(returns, "'jarFile' must be single non-NA character")
		} else if (!file.exists(object@jarFile)) {
			returns = c(returns, "'jarFile' must be an existing file")
		}
		
		if (length(object@jarVersion) != 1 | is.na(object@jarVersion)) {
			returns = c(returns, "'jarVersion' must be single non-NA character")
		}
		
		if (any(!is.na(object@jarVersion) & !is.na(object@odbVersion) & object@jarVersion != object@odbVersion)) {
			returns = c(returns, "'jarVersion' and 'odbVersion' must be the same")
		}
		
		return(returns)
	}
)

setMethod(
	f = "show",
	signature = signature(object="odb"),
	definition = function(object) {
		cat("\nOpenOffice.org Base connection\n\n")
		cat(" ODB database   : ", object@odbFile, "\n", sep="")
		cat(" HSQLDB library : ", object@jarFile, "\n", sep="")
		cat(" HSQLDB version : ", object@jarVersion, "\n", sep="")
		cat("\n")
	}
)
