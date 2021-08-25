#' Calculate sap flow density using the HFD approach
#' @description A function to calculate sap flow density using the HFD approach (Nadezhdina et al. 2012).
#' The temperatures that are directly measured by sensors (upper, lower and side) must be provided.
#'
#' @param L a numeric value; sapwood depth, cm
#' @param Dst a numeric value; wood thermal diffusivity, cm2 s-1; default: 0.0025
#' @param data a data frame that includes the measured sap flow data
#' @param T_up column name for the temperature measured by the upper sensor
#' @param T_low column name for the temperature measured by the lower sensor
#' @param T_side column name for the temperature measured by the side sensor
#' @param K a numeric value (constant K) or a column name (dynamic K). K value indicates
#' the dTasym value when zero sap flow occurs, see Nadezhdina et al. 2012
#' @param Zax a numeric value; axial distance of the sensors, cm. Default: 1.5
#' @param Ztg a numeric value; tangential distance of the sensors, cm. Default: 0.5
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
#' df <- df %>%
#'   pivot_longer(cols = Temp1U:Temp8S,
#'                names_to = c(".value","Position"),
#'                names_sep = c(5)) %>%
#'   pivot_longer(cols = starts_with(c("K","Temp")),
#'                names_to = c(".value","Depth"),
#'                names_sep = -1) %>%
#'   pivot_wider(names_from = Position,
#'               values_from = Temp)
#'
#' # calculate sap flow density using dynamic K values that are provided as a variable
#' df_hfd1 <- Cal_HFD(df,T_up = U,T_low = L,T_side = S,K=K,L=7.5)
#' tapply(df_hfd1$SFD_hfd, df_hfd1$Depth, summary)
#'
#' # calculate sap flow density using constant K value
#' df <- df %>% select(-K)
#' df_hfd2 <- Cal_HFD(df,T_up = U,T_low = L,T_side = S,K=.6,L=7.5)
#' tapply(df_hfd2$SFD_hfd, df_hfd2$Depth, summary)
#'
#' @export
Cal_HFD <- function(data,
                    T_up,
                    T_low,
                    T_side,
                    K,
                    L, # sapwood depth, cm
                    Dst = 0.0025, # thermal diffusivity, cm2 s-1
                    Zax = 1.5, # axial distance, cm
                    Ztg = 0.5 # tangential distance, cm
) {
  # if no temperature or difference info is provided
  # if (is.null(T_up)) {
  #   stop("Temperature columns need to be assigned!")
  # }

  # define the pipe from the package "magrittr"
  `%>%` <- magrittr::`%>%`

  # check whether K is a number or a column
  ck <- try(is.numeric(K),silent = T)
  if (ck==T) {
    T_up <- dplyr::enquo(T_up) # specify as a variable name
    T_low <- dplyr::enquo(T_low)
    T_side <- dplyr::enquo(T_side)
    df <- data %>%
      dplyr::mutate(Sym=!!T_up-!!T_low,
                    Asym=!!T_side-!!T_low,
                    SFD_hfd = ifelse(Sym>0,
                              3600*Dst*(K+(Sym-Asym))*Zax/(Asym*Ztg*L), # positive flux, g cm-2 h-1
                              -3600*Dst*(-K+Asym)*Zax/((Sym-Asym)*Ztg*L) # negative flux
                              )
             )
    print("Constant K")
  } else {
    K <- dplyr::enquo(K)
    T_up <- dplyr::enquo(T_up) # specify as a variable name
    T_low <- dplyr::enquo(T_low)
    T_side <- dplyr::enquo(T_side)
    df <- data %>%
      dplyr::mutate(Sym=!!T_up-!!T_low,
                    Asym=!!T_side-!!T_low,
                    SFD_hfd = ifelse(Sym>0,
                              3600*Dst*(!!K+(Sym-Asym))*Zax/(Asym*Ztg*L), # positive flux, g cm-2 h-1
                              -3600*Dst*(-!!K+Asym)*Zax/((Sym-Asym)*Ztg*L) # negative flux
             )
      )
    print("dynamic K")
  }
  return(df)
}
