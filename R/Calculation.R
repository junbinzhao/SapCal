#' Calculate sap flow density using the HFD approach
#' @description A function to calculate sap flow density using the HFD approach. Either the temperatures
#' that are directly measured by sensors (upper, lower and side) or temperature differences between the
#' sensors (dTsym and dTasym) must be provided.
#'
#' @param L a numeric value, sapwood depth, cm
#' @param Dst a numeric value, wood thermal diffusivity, cm2 s-1; default: 0.0025
#' @param data a data frame that includes the measured sap flow data
#' @param T_up column name for the temperature measured by the upper sensor
#' @param T_low column name for the temperature measured by the lower sensor
#' @param T_side column name for the temperature measured by the side sensor
#' @param Sym column name for the temperature difference between the upper and lower sensors
#' @param Asym column name for the temperature difference between the lower and side sensors
#' @param K a numeric value or a column name, K value indicates the dTasym value when zero sap flow occurs, see Nadezhdina et al. 2012
#' @param Zax a numeric value, axial distance of the sensors, cm
#' @param Ztg a numeric value, tangential distance of the sensors, cm
#'
#' @return a data frame with an additional column of the calculated sap flow density
#' (SFD_hfd, g cm-2 h-1).
#'
#' @examples
#' # load example data
#' df <- read.csv(file = system.file("extdata", "Soil_resp_example.csv", package = "FluxGapsR"),
#' header=T)
#'
#' # convert the data into long format
#'
#' @export
Cal_HFD <- function(data,
                    T_up=NULL,
                    T_low=NULL,
                    T_side=NULL,
                    Sym=NULL,
                    Asym=NULL,
                    K,
                    L, # sapwood depth, cm
                    Dst = 0.0025, # thermal diffusivity, cm2 s-1
                    Zax = 1.5, # axial distance, cm
                    Ztg = 0.5 # tangential distance, cm
) {
  # if no temperature or difference info is provided
  if (is.null(T_up) & is.null(Sym)) {
    stop("Either temperature or temperature difference columns need to be assigned!")
  }

  # check whether K is a number or a column
  if (!is.numeric(K)) K <- enquo(K)

  # if only temperature is provided without the differences
  if (is.null(Sym) | is.null(Asym)){
    T_up <- enquo(T_up) # specify as a variable name
    T_low <- enquo(T_low)
    T_side <- enquo(T_side)
    df <- data %>%
      mutate(Sym=!!T_up-!!T_low,
             Asym=!!T_side-!!T_low,
             SFD_hfd = ifelse(Sym>0,
                              3600*Dst*(K+(Sym-Asym))*Zax/(Asym*Ztg*L), # positive flux, g cm-2 h-1
                              -3600*Dst*(-K+Asym)*Zax/((Sym-Asym)*Ztg*L) # negative flux
                              )
             )
  } else { # if the differences are provided
    # specify as variables
    Sym <- enquo(Sym)
    Asym <- enquo(Asym)
    # load data and calculate
    df <- data %>%
      mutate(SFD_hfd = ifelse(!!Sym>0,
                              3600*Dst*(K+(!!Sym-!!Asym))*Zax/(!!Asym*Ztg*L), # positive flux, g cm-2 h-1
                              -3600*Dst*(-K+!!Asym)*Zax/((!!Sym-!!Asym)*Ztg*L) # negative flux
                              )
             )
  }
  return(df)
}
