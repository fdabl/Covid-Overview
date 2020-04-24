# install.packages('rsconnect')

# we need to move it to another account, but as long as we are using my account, you
# you'll need to run this before depolying:

# rsconnect::setAccountInfo(name='ialmi',
#                           token='B294EC215AE993C837595CDA94F34CBF',
#                           secret='uB3s1xmdbLA7dif8OdtyI9CbnNu37Z5THsoNP713')


library(rsconnect)
library(rstudioapi)

# set working directory to directory of file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rsconnect::deployApp(
                     appName = "dvc_lift", 
                     account = "ialmi") # we'll change this

y
