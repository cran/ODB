# Generates INSERT INTO queries from a data.frame fitting an ODB base
# Author : Sylvain Mareschal
odb.insert = function(
		odb,
		tableName,
		data,
		execute = TRUE,
		dateFormat = "%Y-%m-%d",
		...
		)
	{
	# Class checks
	if (!is(odb, "odb")) {
		stop("'odb' must be an 'odb' object")
	}
	validObject(odb)
	
	# Args checks
	if (!is.character(tableName) || length(tableName) != 1 || is.na(tableName)) {
		stop(call.=FALSE, "'tableName' must be a unique non NA character vector")
	}	
	data = as.data.frame(data)
	
	# Gets table definition
	tryCatch(
		query <- dbSendQuery(
			conn = odb,
			statement = paste("SELECT * FROM", tableName, "WHERE FALSE")
		),
		error = function(e) {
			stop(call.=FALSE, "Error while querying table '", tableName, "' : \"", conditionMessage(e), "\"")
		},
		warning = function(w) {
			stop(call.=FALSE, "Warning while querying table '", tableName, "' : \"", conditionMessage(w), "\"")
		}
	)
	overview = dbColumnInfo(res=query)
	
	# Size check
	if (nrow(overview) != ncol(data)) {
		stop("'tableName' table (", nrow(overview), ") and 'data' (", ncol(data), ") column counts don't match")
	}
	
	# Conversion from data.frame to varchar matrix
	mtx = as.matrix(
		data.frame(
			lapply(data, as.character),
			stringsAsFactors = FALSE
		)
	)
	
	# Cell processing
	isna = is.na(mtx)
	mtx[, overview$field.type == "DATE" ] = as.character(strptime(mtx[, overview$field.type == "DATE" ], dateFormat))
	mtx[, overview$data.type == "character" ] = paste(sep="", "'", gsub("'", "''", mtx[, overview$data.type == "character" ]), "'")
	mtx[, overview$data.type == "numeric" ] = gsub("[^0-9\\.]", "", mtx[, overview$data.type == "numeric" ])
	mtx[ isna ] = "NULL"
	
	# Aggregation
	SQL = paste(sep="",
		"INSERT INTO ", tableName, " VALUES (",
		apply(mtx, 1, paste, collapse=", "),
		");"
	)
	
	# Execution
	if (execute) {
		odb.write(odb, SQL, ...)
		invisible(SQL)
	} else {
		return(SQL)
	}
}
