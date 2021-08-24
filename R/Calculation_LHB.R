Cal_LHB <- function(file,k=2) {
#' Title
#'
#' @param df
#' @param No
#'
#' @return
#'
#' @examples
#'
#' @export
  Cal_LHB <- function(df,No=1) {
    # define parameters
    # k <- 2
    cw <- 4.186 # specific heat capacity of water, J g-1 K-1
    dx <- 0.005 # m
    dy <- 0.015 # m
    if (is.na(df[[paste0("Sym",No)]])){
      Qw <- NA
    } else {
      # variable from data
      H <- df[["HeaterWcm"]]*100 # W m-1
      # dT0 <- -df[[paste0("K_ave",No)]] # oC; K in HFD
      dT0 <- -df[[paste0("K",No)]] # oC; K in HFD
      # dT0 <- -.96
      T_u <- df[[paste0("Temp",No,"U")]] # oC; upper sensor temperature
      T_s <- df[[paste0("Temp",No,"S")]] # oC; side sensor temperature
      T_l <- df[[paste0("Temp",No,"L")]] # oC; lower sensor temperature
      # lam_T
      lam_T <- H/(2*pi*dT0*sqrt(k))*log(sqrt(k)*dx/dy)
      lam_L <- k*lam_T

      # equation to solve (eq.3 in Trcala paper)
      if (df[[paste0("Sym",No)]]>0){
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

      # # simplified for small values using  Taylor series expansion
      # if (df[[paste0("Sym",No)]]>0){
      # func2 <- function(Qw){ # positive flux
      #   T_s-T_u+H/(2*pi*sqrt(lam_L*lam_T))*
      #     (log(sqrt(k)*dx/dy)-(-digamma(1))*(cw*Qw*dy/(2*lam_L))-
      #        (cw*Qw*dy/(2*lam_L))*log((cw*Qw*dy/(2*lam_L))/2))
      # }
      # } else {
      # func2 <- function(Qw){ # negative flux
      #   T_s-T_l+H/(2*pi*sqrt(lam_L*lam_T))*
      #     (log(sqrt(k)*dx/dy)-(-digamma(1))*(cw*Qw*dy/(2*lam_L))-
      #        (cw*Qw*dy/(2*lam_L))*log((cw*Qw*dy/(2*lam_L))/2))
      # }
      # } # end of else

      # uniroot(func,interval = c(1,3),extendInt = "yes")
      Qw <- try(as.numeric(uniroot(func1,interval = c(1e-100,200))$root), # big values
                silent = T)
      # if (class(Qw)=="try-error"){ # check if root is found
      #   Qw <- try(-as.numeric(uniroot(func2,interval = c(1e-100,200))$root), # negative values
      #             silent = T)
      # }
      # if (class(Qw)=="try-error"){ # check if root is found
      #   Qw <- try(as.numeric(uniroot(func2,interval = c(1e-100,200))$root), # small values
      #             silent = T)
      # }
      # if (class(Qw)=="try-error"){ # check if root is found
      #   Qw <- try(-as.numeric(uniroot(func4,interval = c(1e-100,200))$root), # small negative values
      #             silent = T)
      # }
      # if (class(Qw)=="try-error") Qw <- NA # assign NA if no root is found still

      Qw <- ifelse(class(Qw)=="try-error",NA,
                   ifelse(df[[paste0("Sym",No)]]>0,Qw,-Qw)) # check the direction of the flux
    }
    return(Qw*0.36) # unit: from g m-2 s-1 to g cm-2 h-1
  }

  # calculate
  df_lhr <- read.csv(file) %>%
    mutate(Date_time = ymd_hms(Date_time)
           # K_ave1 = mean(K1,na.rm = T),
           # K_ave2 = mean(K2,na.rm = T),
           # K_ave3 = mean(K3,na.rm = T),
           # K_ave4 = mean(K4,na.rm = T),
           # K_ave5 = mean(K5,na.rm = T),
           # K_ave6 = mean(K6,na.rm = T),
           # K_ave7 = mean(K7,na.rm = T),
           # K_ave8 = mean(K8,na.rm = T)
    )
  #        ) %>%
  # rowwise() %>%
  # mutate(SFD_hfd_1 = Cal_LHB(.,No = 1))

  # use loop to calculate
  dft <- data.frame()
  for (i in 1:8){ # No. of the depths
    for (j in 1: nrow(df_lhr)){ # row number
      dft[j,i] <- Cal_LHB(df_lhr[j,],No=i)
    }
  }

  dft <- data.frame(df_lhr[,"Date_time"],dft)
  names(dft) <- c("Date_time",paste0("SFD_lhb_",1:8))
  return(dft)
}
