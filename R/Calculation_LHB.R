#' Calculate sap flow density using the LHB approach
#' @description A function to calculate sap flow density using the LHB approach (Trcala and Cermak 2016).
#' The temperatures that are directly measured by sensors (upper, lower and side) must be provided.
#'
#' @param data a data frame that includes the measured sap flow data
#' @param T_up a string indicates the column name for the temperature measured by the upper sensor
#' @param T_low a string indicates the column name for the temperature measured by the lower sensor
#' @param T_side a string indicates the column name for the temperature measured by the side sensor
#' @param Heat a numeric value; the heating power used for the probe (W m-1)
#' @param Zax a numeric value; axial distance of the sensors, cm. Default: 1.5
#' @param Ztg a numeric value; tangential distance of the sensors, cm. Default: 0.5
#' @param K a numeric value (constant K) or a string that indicates the column name (dynamic K).
#' K value indicates the dTasym value when zero sap flow occurs, see Nadezhdina et al. 2012
#' @param ratio a numeric value; the ratio between the thermal conductivities of wood in the axial
#' and tangential directions; default: 2 (Trcala and Cermak 2016)
#'
#' @return a data frame with an additional column of the calculated sap flow density
#' (SFD_lhb, g cm-2 h-1).
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
#' df_lhb1 <- Cal_LHB(df,T_up = "U",T_low = "L",T_side = "S",K="K",Heat=2.6)
#' head(df_lhb1$SFD_lhb)
#'
#' # calculate sap flow density using constant K value
#' df_lhb2 <- Cal_LHB(df,T_up = "U",T_low = "L",T_side = "S",K=.6,Heat=2.6)
#' head(df_lhb2$SFD_lhb)
#' @export
Cal_LHB <- function(data,
                    T_up,
                    T_low,
                    T_side,
                    K, # oC; K in HFD
                    ratio=2, # ratio between Lam_T and Lam_L
                    Heat, # W m-1
                    Zax = 1.5, # axial distance, cm
                    Ztg = 0.5 # tangential distance, cm
                    ) {
  # define parameters
  cw <- 4.186 # specific heat capacity of water, J g-1 K-1
  dx <- Ztg*0.01 # m
  dy <- Zax*0.01 # m
  H <- Heat # W m-1
  k <- ratio
  SFD_lhb <- vector(length = nrow(data)) # create a vector for calculated flux
  for (i in 1:nrow(data)) {
    # variable from data
    dT0 <- ifelse(is.numeric(K),-K,-data[i,K]) # oC; K in HFD
    T_u <- data[i,T_up] # oC; upper sensor temperature
    T_s <- data[i,T_side] # oC; side sensor temperature
    T_l <- data[i,T_low] # oC; lower sensor temperature
    Sym <- T_u-T_l
    # lam_T
    lam_T <- H/(2*pi*dT0*sqrt(k))*log(sqrt(k)*dx/dy)
    lam_L <- k*lam_T
    # equation to solve (eq.3 in Trcala paper)
    if (Sym>0){
      func1 <- function(Qw){ # positive flux
        T_s-T_u+H/(2*pi*sqrt(lam_L*lam_T))*
          (exp(cw*Qw*dy/(2*lam_L))*besselK(cw*Qw*dy/(2*lam_L),0)-
             besselK(cw*Qw*dx/(2*sqrt(lam_L*lam_T)),0))
      }
    } else {
      func1 <- function(Qw){ # negative flux
        T_s-T_l+H/(2*pi*sqrt(lam_L*lam_T))*
          (exp(cw*Qw*dy/(2*lam_L))*besselK(cw*Qw*dy/(2*lam_L),0)-
             besselK(cw*Qw*dx/(2*sqrt(lam_L*lam_T)),0))
      }
    } # end of else
    Qw <- try(as.numeric(uniroot(func1,interval = c(1e-100,200))$root),
              silent = T)
    SFD_lhb[i] <- ifelse(class(Qw)=="try-error",
                         NA,
                         ifelse(Sym>0,
                                Qw*0.36, # unit: from g m-2 s-1 to g cm-2 h-1
                                -Qw*0.36)) # check the direction of the flux
  }
dft <- data.frame(data,SFD_lhb)
return(dft)
}
