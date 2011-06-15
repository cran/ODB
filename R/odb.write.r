# Executes a series of SQL queries in a ODB connection (Write Only)
# Author : Sylvain Mareschal
odb.write = function(
		odb,
		sqlQueries,
		onError = c("warning", "stop")
		)
	{
	# Class checks
	if (!is(odb, "odb")) {
		stop("'odb' must be an 'odb' object")
	}
	validObject(odb)
	
	# Checks
	if (!is.character(sqlQueries) || length(sqlQueries) == 0 || any(is.na(sqlQueries))) {
		stop(call.=FALSE, "'sqlQueries' must be a non NA character vector")
	}	
	
	# Args matching
	onError = match.arg(onError)
	
	# Block on error or continue
	if (onError == "stop") {
		errorFun = function(e) {
			stop(call.=FALSE, "Query #", i, " : \"", conditionMessage(e), "\"")
		}
	} else {
		errorFun = function(e) {
			warning(call.=FALSE, immediate.=TRUE, "Query #", i, " : \"", conditionMessage(e), "\"")
		}
	}
	
	# Queries execution
	for(i in 1:length(sqlQueries)) {
		tryCatch(
			dbSendUpdate(odb, sqlQueries[i]),
			error = errorFun
		)
	}
}
