# step 1 (optional): setup a new google account so that you don't give access to your own...

# step 2: authorize this account: uncomment lines 6-10

# library(googlesheets4)
# options(gargle_oauth_cache = ".secrets")
# gargle::gargle_oauth_cache() # should print .secrets
# gs4_auth() # takes you to a browser. give access to google sheets. I created an extra google account for this
# list.files(".secrets/") # make sure it worked....

# In order for the deployment to RStudio Connect to work, the .secrets directory and
# .Rprofile files need to be in the bundle. Be sure to do this from the Add Files button.
# If you cannot see the files because they are hidden from Finder
# you can press cmnd + shift + .. Then publish!

# step 3: use gs4_create() or go to the browser and create a new, blank sheet. copy the link to this sheet (share)

# paste google link or ID here...
link = "google link here"

# this is the username that allows access to setting up the scheduler. 
admin_name = "rob-admin"
