

#link = "https://docs.google.com/spreadsheets/d/1eXXCcAsbUc5K7LSqzid3TTzDideHd6NsSqZOB7jEMqM/edit?usp=sharing"

admin_name = "rob-admin"

#urlpath = "mongodb+srv://shiny-user:password1234@cluster0.yt0db.mongodb.net/test?retryWrites=true&w=majority"

options(mongodb = list(
  "host" = "cluster0.yt0db.mongodb.net",
  "username" = "shiny-user",
  "password" = "password1234"
))
databaseName <- "test"
collectionName <- "test"

saveData <- function(data) {
  #Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE,
                                    allow_invalid_hostname = TRUE))
  # Insert the data into the mongo collection as a data.frame
  data <- as_data_frame(data)
  db$insert(data)
}
newData <- function(data) {
  # Connect to the database
  # db <- mongo(collection = collectionName,
  #             url = sprintf(
  #                 "mongodb+srv://%s:%s@%s/%s",
  #                 options()$mongodb$username,
  #                 options()$mongodb$password,
  #                 options()$mongodb$host,
  #                 databaseName
  #             ),
  #             options = ssl_options(weak_cert_validation = TRUE,
  #                                   allow_invalid_hostname = TRUE))
  # # Insert the data into the mongo collection as a data.frame
  # data <- as_tibble(data)
  # db$remove('{}')
  # db$insert(data)
}
loadData <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  # Read all the entries
  data <- db$find()
  data
}
print(loadData())

saveData(df)
