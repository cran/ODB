# Returns the list of tables, fields and comments from an odb connection# Author : Sylvain Mareschalodb.tables = function(		odb		)	{	# Class checks	if (!is(odb, "odb")) {		stop("'odb' must be an 'odb' object")	}	validObject(odb)		# Lists tables in DB	tryCatch(		tables <- dbListTables(conn=odb),		error = function(e) {			stop(call.=FALSE, "Error while listing tables : \"", conditionMessage(e), "\"")		},		warning = function(w) {			stop(call.=FALSE, "Warning while listing tables : \"", conditionMessage(w), "\"")		}	)		# Excludes SYSTEM tables	tables = tables[ !grepl("^SYSTEM_[A-Z_]+$", tables) ]		# Lists fields of each table	overview = list()	for(name in tables) {		# Execution		tryCatch(			query <- dbSendQuery(conn=odb, statement=paste("SELECT * FROM \"", name, "\" WHERE FALSE", sep="")),			error = function(e) {				stop(call.=FALSE, "Error while querying table '", name, "' : \"", conditionMessage(e), "\"")			},			warning = function(w) {				stop(call.=FALSE, "Warning while querying table '", name, "' : \"", conditionMessage(w), "\"")			}		)		columns = dbColumnInfo(res=query)				# Factors to Characters		for(k in 1:ncol(columns)) {			if (is.factor(columns[,k])) {				columns[,k] = as.character(columns[,k])			}		}				# Adds to output		overview[[name]] = columns	}		# Comments	comments = odb.comments(odb)	for(tableName in names(overview)) {		overview[[tableName]][,"comment"] = NA		for(columnName in names(comments[[ tableName ]])) {			overview[[ tableName ]][ overview[[ tableName ]]$field.name == columnName , "comment" ] = comments[[ tableName ]][ columnName ]		}	}		return(overview)}