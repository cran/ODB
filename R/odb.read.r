# Executes a series of SQL queries in a ODB connection (Read Only)
# Author : Sylvain Mareschal
odb.read = function(
		odb,
		sqlQuery,
		stringsAsFactors = FALSE,
		check.names = FALSE,
		encode = TRUE,
		autoLogical = TRUE
		)
	{
	# Class checks
	if (!is(odb, "odb")) {
		stop("'odb' must be an 'odb' object")
	}
	validObject(odb)
	
	# Query check
	if (!is.character(sqlQuery) || length(sqlQuery) != 1 || is.na(sqlQuery)) {
		stop(call.=FALSE, "'sqlQuery' must be a unique non NA character vector")
	}	
	
	# Execution
	tryCatch(
		query <- dbSendQuery(odb, sqlQuery),
		error = function(e) {
			stop(call.=FALSE, "Error while executing SQL query  : \"", conditionMessage(e), "\"")
		},
		warning = function(w) {
			stop(call.=FALSE, "Warning while executing SQL query  : \"", conditionMessage(w), "\"")
		}
	)
	
	# Results retrieving
	results = fetch(res=query, n=-1)
	
	## Clearing results (not currently supported by JDBC)
	# dbClearResult(res=query)
	
	# Reverts check.names
	if (!check.names) {
		columns = dbColumnInfo(res=query)
		names(results) = as.character(columns$field.name)
	}
	
	# Factors to Characters
	if (!stringsAsFactors) {
		for(k in 1:ncol(results)) {
			if (is.factor(results[,k])) {
				results[,k] = as.character(results[,k])
			}
		}
	}
	
	# Re-encoding, keeping NAs
	if (encode) {
		for(k in 1:ncol(results)) {
			if (is.character(results[,k]) | is.factor(results[,k])) {
				naVals = is.na(results[,k])
				results[,k] = iconv(results[,k], from="UTF-8", to="")
				if (length(naVals) > 0) {
					results[naVals,k] = NA
				}
			}
		}
	}
	
	# Logical
	if (autoLogical) {
		for(k in 1:ncol(results)) {
			if (all(is.na(results[,k]) | results[,k] %in% c("true", "false"))) {
				results[,k] = as.logical(results[,k])
			}
		}
	}
	
	return(results)
}
