                                                  
# #Add all categories of gear for each cyl to original data.table as a list.
# dt <- data.table(mtcars)[,.(gear, cyl)]
# dt[,gearsL:=list(list(unique(gear))), by=cyl] # original, ugly
# dt[,gearsL:=.(list(unique(gear))), by=cyl] # improved, pretty
# head(dt)
# #Extract second element of each list in gearL1 and create row gearL1.
# #This is  nbt that groundbreaking, but explores how to access elements of columns
# #which are constructed of lists of lists. lapply is your friend.
# dt[,gearL1:=lapply(gearsL, `[`, 2)]
# dt[,gearS1:=sapply(gearsL, `[`, 2)]
# DT syntax	data.table::merge() syntax
# INNER	X[Y, nomatch=0]	merge(X, Y, all=FALSE)
# LEFT OUTER	Y[X]	merge(X, Y, all.x=TRUE)
# RIGHT OUTER	X[Y]	merge(X, Y, all.y=TRUE)
# FULL OUTER	-	merge(X, Y, all=TRUE)
# FULL OUTER WHERE NULL (NOT INNER)	-	merge(X, Y, all=TRUE), subset NA
# CROSS (cartesian)	-	- (see below)
# #the INNER JOIN returns the rows with a match in both tables
# # set the ON clause as keys of the tables:
# setkey(Employees,Department)
# setkey(Departments,Department)
# # perform the join, eliminating not matched rows from Right
# Result <- Employees[Departments, nomatch=0]
# 
# #The LEFT OUTER JOIN returns all the rows from the left table, filling in matched columns (or NA) from the right table
# # set the ON clause as keys of the tables:
# setkey(Employees,Department)
# setkey(Departments,Department)
# # perform the join using the merge function
# Result <- merge(Employees,Departments, all.x=TRUE)
# 
# # get the columns of the tables:
# leftCols <- colnames(Employees)
# rightCols <- colnames(Departments)
# # remove the match key of the Right table
# rightCols <- setdiff(rightCols,key(Departments))
# # set the column order
# setcolorder(Result,c(leftCols,rightCols))
# 
# # set the ON clause as keys of the tables:
# setkey(Employees,Department)
# setkey(Departments,Department)
# 
# # defining the Result columns, substitute Department by DepartmentName
# leftCols <- colnames(Employees)
# leftCols <- sub("Department","DepartmentName",leftCols)
# 
# # perform the join, inverting the tables, return defined columns
# Result <- Departments[Employees][, leftCols, with=FALSE]
# 
# # -- or --
# # Result <- merge(Employees, Departments, all.x=TRUE)
# # Result <- Result[, setCols, with=FALSE]
# 
# the RIGHT OUTER JOIN returns all the rows from the right table, filling in matched columns (or NA) from the left table
# 
# 
# 
# # set the ON clause as keys of the tables:
# setkey(Employees,Department)
# setkey(Departments,Department)
# 
# # perform the join - this is the basic join for data.table
# Result <- Employees[Departments]
# # this corresponds to
# # Result <- merge(Employees,Departments, all.y=TRUE)
# 
# the FULL OUTER JOIN returns all the rows from both tables, filling in matched columns (or NA)
# 


# # set the ON clause as keys of the tables:
# setkey(Employees,Department)
# setkey(Departments,Department)
# 
# # perform the join
# Result <- merge(Employees,Departments, all=TRUE)
# 
# the NOT INNER JOIN returns all the rows from both tables, where no match was obtained
# 
# 
# 
# # set the ON clause as keys of the tables:
# setkey(Employees,Department)
# setkey(Departments,Department)
# 
# # perform the join, retain only NA from matched cols on both side
# Result <- merge(Employees,Departments, all=TRUE)
# Result <- Result[is.na(EmployeeName) | is.na(DepartmentName)]
# 
# set.seed(1L)
# DF = data.frame(ID1 = sample(letters[1:2], 10, TRUE),
#                 ID2 = sample(1:3, 10, TRUE),
#                 val = sample(10),
#                 stringsAsFactors = FALSE,
#                 row.names = sample(LETTERS[1:10]))
# 
# #We can subset a particular row using its row name as shown below:
#   
#   DF["C", ]
# #   ID1 ID2 val
# # C   a   3   5
#  
#   
#    rownames(DF) = sample(LETTERS[1:5], 10, TRUE)
#   # Warning: non-unique values when setting 'row.names': 'C', 'D'
#   # Error in `row.names<-.data.frame`(`*tmp*`, value = value): duplicate 'row.names' are not allowed
# 
#    rownames(DT)
#    setkey(flights, origin)
#    head(flights)
#    ## alternatively we can provide character vectors to the function 'setkeyv()
#    set* and :=:
#      
# #In data.table, the := operator and all the set* (e.g., setkey, setorder, setnames etc..)
# #functions are the only ones which modify the input object by reference.
# #Once you key a data.table by certain columns, you can subset by querying those
# #key columns using the .() notation in i. Recall that .() is an alias to list().
#      flights[.("JFK")]
#    
#    key(flights)
#    # [1] "origin" "dest"
#    
#    
#    setdiff()
#    intersect()
#    union()
#    setequal()
#    x = data.table(c(1,2,2,2,3,4,4))
#    y = data.table(c(2,3,4,4,4,5))
#    fintersect(x, y)            # intersect
#    fintersect(x, y, all=TRUE)  # intersect all
#    fsetdiff(x, y)              # except
#    fsetdiff(x, y, all=TRUE)    # except all
#    funion(x, y)                # union
#    funion(x, y, all=TRUE)      # union all
#    fsetequal(x, y)             # setequal
#    
#    
#    flights[.("LGA", "TPA"), .(arr_delay)]   
#    
#    nams <- names(dat)
#    for(n in seq_along(nams)){
#      nam <- nams[n] 
#      char <- sprintf('%s==1',nam)
#      dat[eval(parse(text=char)), newvar := n]
#    }
#    
#    
#    dat[ ,newvar:= NA_integer_]
#    for(i in ncol(dat)) {
#      setkeyv(dat, names(dat)[i])
#      dat[J(1), newvar:=i]
#    }
# 
#    nams <- names(dat)
#    for(n in seq_along(nams)){
#      nam <- nams[n] 
#      char <- sprintf('%s==1',nam)
#      dat[eval(parse(text=char)), newvar := n]
#    }
#    
#    chinches_data<-read.csv("/Users/Rodrigo/Downloads/DISTRITOS CON CHINCHES.csv")
#    
#    nrow(chinches_data)
#    
#    index <- sample(1:nrow(chinches_data), 5)
#    chinches_data[index, ]
#    


