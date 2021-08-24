#' Title
#'
#' @param data
#' @param T_up
#' @param T_low
#' @param T_side
#' @param dt0
#' @param k
#' @param Heat
#' @param Zax
#' @param Ztg
#'
#' @return
#'
#' @examples
#'
#' @export
Cal_LHB <- function(data,
                    T_up=NULL,
                    T_low=NULL,
                    T_side=NULL,
                    dt0, # oC; -K in HFD
                    k=2, # ratio between Lam_T and Lam_L
                    Heat, # W m-1
                    Zax = 1.5, # axial distance, cm
                    Ztg = 0.5 # tangential distance, cm
                    ) {
  # define parameters
  cw <- 4.186 # specific heat capacity of water, J g-1 K-1
  dx <- Ztg*0.01 # m
  dy <- Zax*0.01 # m
  H <- Heat # W m-1
  SFD_lhb <- vector(length = nrow(data)) # create a vector for calculated flux
  for (i in 1:nrow(data)) {
    # variable from data
    dT0 <- ifelse(is.numeric(dt0),dt0,-data[i,dt0]) # oC; K in HFD
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
