library(mongolite)

# insert code
options(mongodb = list(
  "host" = "cluster0.6d2ch.mongodb.net",
  "username" = "new-user1",
  "password" = "TTzxY0qbIIWs5G1j"
))

databaseName <- "meetR"

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


del = which(current_collections=="A very important meeting")
current_collections = current_collections[-del]

for(i in current_collections){
 mongo_deleteData(i)
}
