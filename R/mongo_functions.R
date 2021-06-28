library(mongolite)

mongo_loadCollections <- function(){
  
  col <- mongo(#collection = collectionName,
    db = databaseName,
    url = paste0("mongodb+srv://",
                 options()$mongodb$username,":",
                 options()$mongodb$password, "@",
                 options()$mongodb$host
    ),
    verbose = T)
  colls = col$run('{"listCollections":1}')
  rm(col)
  unique(colls$cursor$firstBatch$name)
}
#  leave uncommented to load unique collections
current_collections = mongo_loadCollections()


mongo_saveData <- function(data, collectionName) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              db = databaseName,
              url = paste0("mongodb+srv://",
                           options()$mongodb$username,":",
                           options()$mongodb$password, "@",
                           options()$mongodb$host
              ),
              verbose = T)
  # Insert the data into the mongo collection as a data.frame
  data <- as.data.frame((data))
  db$insert(data)
  rm(db)
}

#saveData(iris)

mongo_loadData <- function(collectionName) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              db = databaseName,
              url = paste0("mongodb+srv://",
                           options()$mongodb$username,":",
                           options()$mongodb$password, "@",
                           options()$mongodb$host
              ),
              verbose = T)
  # Read all the entries
  data <- db$find()
  rm(db)
  data
  
}

#df = loadData()

mongo_deleteData <- function(collectionName) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              db = databaseName,
              url = paste0("mongodb+srv://",
                           options()$mongodb$username,":",
                           options()$mongodb$password, "@",
                           options()$mongodb$host
              ),
              verbose = T)
  # Read all the entries
  db$drop()
  rm(db)
  
}

#mongo_deleteData(collectionName = "otherresponses")
