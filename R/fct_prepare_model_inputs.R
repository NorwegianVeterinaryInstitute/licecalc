#' extract_IP
#' Function to extract infestation pressure and sea temperature for a farm
#' based on the location ID.
#'
#' The extraction is done from internal data. These needs to be updated when
#' changes occur. See code in data-raw for more details.
#'
#' @param lokid The Location ID
#'
#' @returnThe output is a list with the following elements:
#' IP_1wk is infestation pressure used to predict lice 1 week ahead;
#' IP_2wk is infestation pressure used to predict lice 2 weeks ahead;
#' ST is sea temperature;
#' week_no is the week number the data are extracted from.
#'
#' @export
#'
#' @examples
#' \dontrun{
#  extract_ip("12115")
#' }
extract_ip = function(lokid) {
  which_nxt <-
    which(colnames(infestation_pressure_all) == "XNextWeek1")
  this_week <- colnames(infestation_pressure_all)[which_nxt - 1]
  if (is.element(lokid, rownames(infestation_pressure_all))) {
    IP_1wk = infestation_pressure_all[lokid, this_week]
    IP_2wk = infestation_pressure_all[lokid, which_nxt]
    ST = sea_temperature_all[lokid, this_week]
    week_no = substr(this_week, 6, 7)
    return(list(
      IP_1wk = IP_1wk,
      IP_2wk = IP_2wk,
      ST = ST,
      week_no = week_no
    ))
  } else
    return(NA)
}

#' extract_lice
#'
#' Function to extract lice data for a farm based on its location ID.
#'
#' The extraction is done from internal data. These needs to be updated when
#' changes occur. See code in data-raw for more details.
#'
#' @param lokid is the location ID
#'
#' @return The output is a list with the following elements:
#' AF is the latest reported number of adult female lice per fish in the farm;
#' OM is the latest reported number of other motile lice per fish in the farm;
#' FX is the latest reported number of sessile lice per fish in the farm.
#'
#' @export
#'
#' @examples
#' #' \dontrun{
#  extract_lice("12115")
#' }
extract_lice = function(lokid){
  this_week <- colnames(adult_lice)[ncol(adult_lice)]
  if(is.element(lokid, rownames(adult_lice))){
    AF = adult_lice[lokid, this_week]
    OM = mobile_lice[lokid, this_week]
    # FX = sessile_lice[lokid, this_week]
    week_no = substr(this_week, 6, 7)
    return(list(AF=AF, OM=OM, #FX=FX,
             week_no=week_no))
  } else return(NA)
}




