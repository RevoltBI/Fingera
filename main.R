
#Install the libraries
library(httr)
library(data.table)
install.packages("dtplyr")
library(dtplyr)
library(xml2)
install.packages("purrr")
library(purrr)

#=======BASIC INFO ABOUT THE Fingera EXTRACTOR========#

#=======CONFIGURATION========#
## initialize application
library('keboola.r.docker.application')
app <- DockerApplication$new('/data/')

app$readConfig()

## access the supplied value of 'myParameter'
password<-app$getParameters()$`#password`
user<-app$getParameters()$user
# start_date<-app$getParameters()$from
# end_date<-app$getParameters()$to
url<-app$getParameters()$url

##Catch config errors

if(is.null(user)) stop("enter your username in the user config field")
if(is.null(password)) stop("enter your password in the #password config field")
# if(is.null(start_date)|is.null(end_date)) {
#   warning("You entered an invalid date range, the extract will extract data from last 1 day")
#   end_date<-Sys.Date()
#   start_date<-end_date-1
#   end_date<-as.POSIXct(end_date)
#   start_date<-as.POSIXct(start_date)
# }
# from<-format(as.Date(start_date), "%d.%m.%Y")
# to<-format(as.Date(end_date), "%d.%m.%Y")


# List of available endpoints
# day_schedules
# event_categories
# fingera_stations
# groups
# local_settings
# settings
# terminals
# time_logs
# user_accounts
# users

##Now process the request


getStats<-function(api,endpoint){
  
  url<-paste0(api,endpoint)
  
  r <-
    RETRY(
      "GET",
      url,
      config = authenticate(user,password),
      times = 3,
      pause_base = 3,
      pause_cap = 10
    )
  
  res<-content(r,"text",encoding = "UTF-8")%>%fromJSON
  
}

getTimelogs<-function(api,user_id){
  
  url<-paste0(api,"time_logs")
  
  args<-list(
    "user_id"=user_id
  )
  
  r <-
    RETRY(
      "GET",
      url,
      config = authenticate(user,password),
      times = 3,
      pause_base = 3,
      pause_cap = 10,
      query = args
    )
  
  res<-content(r,"text",encoding = "UTF-8")%>%fromJSON
  
}

#iterate over users
user_ids<-getStats(url,"users")%>%filter(as.Date(substr(created_at,1,10))<as.Date(end_date))%>%select(id)%>%.[,1]

test<-user_ids[1:10]

res<-map_df(user_ids,function(x){getTimelogs(url,x)})

write_csv(res,"out/tables/time_logs.csv")
