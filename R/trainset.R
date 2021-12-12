#' Edit classification of activity and pressure with TRAINSET
#'
#' This function
#'
#' @param PAM_data PAM logger dataset list
#' @param file.location Path to the folder where the labeled files should be saved
#' @param file.name Name for the file (default is PAM_data$id)
#' @return PAM logger dataset list updated
#' @export
trainset.edit <- function(PAM_data,
                          file.location,
                          file.name = paste0(PAM_data$id,'_act_pres')
                          ){
  # Create file from PAM_data if no labeled file for this bird exist
  if (!file.exists(paste0(file.location,file.name,"-labeled.csv"))){
    trainset.write(PAM_data,
                  file.location,
                  file.name)

    # Edit the file in browser
    browseURL("https://trainset.geocene.com/")

    # When labilization finished and new file saved, press enter
    cond=T
    while(cond){
      readline(prompt="Press [enter] to continue")
      if (!file.exists(paste0(file.location,file.name,"-labeled.csv"))){
        cond=F
      } else {
        warning(paste0("No labelized file found. Make sure you exported the file from trainset as ",paste0(file.location,file.name,"-labeled.csv")))
      }
    }
  }

  # Read the new file and return the updated PAM data
  trainset.read(PAM_data,
                file.location,
                file.name)
}

#' @export
trainset.write <- function(PAM_data,
                           file.location,
                           file.name = paste0(PAM_data$id,'_act_pres')
                               ){
  write.csv(rbind(
    data.frame(
      series = "acceleration",
      timestamp = strftime(PAM_data$acceleration$date , "%Y-%m-%dT%H:%M:%S%z"),
      value = PAM_data$acceleration$act,
      label = PAM_data$acceleration$class
    ),
    data.frame(
      series = "pressure",
      timestamp = strftime(PAM_data$pressure$date , "%Y-%m-%dT%H:%M:%S%z"),
      value = PAM_data$pressure$obs,
      label = ""
    )
  ),paste0(file.location,file.name,".csv"), row.names=FALSE)
}


#' @export
trainset.read <- function(PAM_data,
                          file.location,
                          file.name = paste0(PAM_data$id,'_act_pres')
                          ){
  tmp = read.csv(paste0(file.location,file.name,"-labeled.csv"))
  # check that same size as PAM
  PAM_data$acceleration$class = tmp$label[tmp$series=="acceleration"]
  PAM_data$pressure$class = tmp$label[tmp$series=="pressure"]
  PAM_data
}
