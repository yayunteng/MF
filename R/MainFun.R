# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' MF_measures
#'
#' \code{MF_measures}:\cr
#' Multi-functionality measures in a single ecosystem or multiple ecosystems. This is a function in the framework of Hill-Chao numbers
#'
#' @param data (a) For \code{is_normalized = \code{TRUE}}, data can be input as a vector of functions (for a single assemblage), matrix/data.frame (assemblages by functions). In this case, the data input must be normalized between 0 and 1 already. \cr
#' (b) For \code{is_normalized = \code{FALSE}}, data can only be input as a matrix/data.frame (multiple assemblages by functions).
#' @param q a numerical vector specifying the diversity orders. Default is 0, 1 and 2.
#' @param fun_cols the order number of the columns which be used as the function variable.
#' @param ecosystem \code{'single'} or \code{'multiple'}. MF measures in a single ecosystem or multiple ecosystems.
#' @param species_col (required only when \code{ecosystem = 'multiple'}), name of the column to be used to calculate the species diversity.
#' @param is_normalized a logical variable to represent whether the data input is normalized between 0 and 1, default is \code{TRUE}.
#' @param negative (required only when \code{is_normalized = FALSE}), name of columns to be normalized negatively.
#' @param is_zero_miss (required only when \code{is_normalized = FALSE}), a logical variable to determine whether the zero in data represents no value or not, default is \code{TRUE}..

#' @import devtools
#' @import ggplot2
#' @import dplyr
#' @import tidyverse
#' @importFrom stats cor

#' @return a data.frame
#' @export


MF_measures <- function(data, q = c(0,1,2), fun_cols = 1:ncol(data),
                        ecosystem = "single", species_col = NULL,
                        is_normalized = TRUE, negative = NULL, is_zero_miss = TRUE){

  if ((length(data) == 1) && (inherits(data, c("numeric", "integer"))))
    stop("Error: Your data does not have enough information.")

  if (((is.vector(data) | nrow(data)<2) && (!is_normalized)))
    stop("Error: Your data does not have enough information to be normalized automatically, please transform the data between 0 and 1 first.")
  # if (is.vector(data) & (FALSE %in% (0 <= data & data <= 1)))
  #   stop("Error: Your data does not have enough information to be normalized automatically, please transform the data between 0 and 1 first.")

  if (is.vector(data)) data <- matrix(data,nrow=1)

  if (!(ecosystem %in% c("single","multiple")))
    stop("Error: ecosystem should be 'single' or 'multiple'.")

  fun_data <- data[,fun_cols]

  # if (nrow(fun_data)<2 & (FALSE %in% (0 <= fun_data & fun_data <= 1)))
  #   stop("Error: Your data does not have enough information to be normalized automatically, please transform the data between 0 and 1 first.")
  # if ((FALSE %in% (0 <= fun_data & fun_data <= 1)) && is_normalized)
  #   stop("Error: Your data does not be normalized between 0 and 1, please change the argument is_normalized to be FALSE.")

  if(!is_normalized){
    if(is.null(negative)) fun_data <- apply(fun_data,2,func_normalized,positive=TRUE,is_zero_miss=is_zero_miss)
    else if(!all(negative %in% colnames(fun_data)))
      stop("Error: Negative columns must be included in function columns.")
    else{
      neg <- as.data.frame(fun_data) %>% dplyr::select(all_of(negative)) %>% apply(2,func_normalized,positive=FALSE,is_zero_miss=is_zero_miss)
      pos <- as.data.frame(fun_data) %>% dplyr::select(!negative) %>% apply(2,func_normalized,positive=TRUE,is_zero_miss=is_zero_miss)
      fun_data <- cbind(neg,pos)
    }
  }

  if (ecosystem == "single"){
    fun_data[is.na(fun_data)] <- 0

    qMF_output <- sapply(q,function(i) apply(fun_data,1,function(x) qMF(v=x,q=i))) %>% as.data.frame()
    if(nrow(data)==1) qMF_output <- t(qMF_output)
    names(qMF_output) <- paste0("qMF_uncorrelated_",q)

    tau <- seq(0,1,0.01)
    transform_D <- sqrt(1-abs(cor(fun_data)))
    qMF_tau_output <- sapply(q,function(i) {
      out <- lapply(tau,function(t){
        apply(fun_data,1,function(x) qMF(v=x,q=i,tau=t,di=transform_D))
      }) %>% do.call(cbind,.)
      AUC <- apply(out,1,function(x){
        AUC_L <- sum(x[seq_along(x[-1])]*diff(tau))
        AUC_R <- sum(x[-1]*diff(tau))
        (AUC_L+AUC_R)/2
      })
      AUC
    }) %>% as.data.frame()
    if(nrow(data)==1) qMF_tau_output <- t(qMF_tau_output)
    names(qMF_tau_output) <- paste0("qMF_correlated_",q)

    output <- cbind(data[,-fun_cols],qMF_output,qMF_tau_output) %>%
      pivot_longer(cols = starts_with("qMF"),
                   names_to = c("Type", "Order.q"),
                   names_pattern = "qMF_?(.*)_(.*)",
                   values_to = "qMF")
  }
  else{
    if (is.null(species_col))
      stop("Error: ")

    spe_data <- cbind(data %>% dplyr::select(species_col),fun_data) %>% as.data.frame()
    two_idx <- combn(1:nrow(spe_data),2)
    tau <- seq(0,1,0.01)
    fun_data[is.na(fun_data)] <- 0
    transform_D <- sqrt(1-abs(cor(fun_data)))
    output <- lapply(1:ncol(two_idx),function(i){
      d_x <- cbind(x1=t((spe_data %>% dplyr::select(-species_col))[two_idx[1,i],]),
                   x2=t((spe_data %>% dplyr::select(-species_col))[two_idx[2,i],]))
      rF <- sapply(q,function(y) qMF_diversity(d_x,y,diversity = "gamma"))
      aF <- sapply(q,function(y) qMF_diversity(d_x,y,diversity = "alpha"))
      bF <- rF/aF
      s_r <- c(spe_data[,1][two_idx[1,i]] %>% strsplit("[.]") %>% unlist(),
               spe_data[,1][two_idx[2,i]] %>% strsplit("[.]") %>% unlist()) %>% unique() %>% length
      s_a <- (c(spe_data[,1][two_idx[1,i]] %>% strsplit("[.]") %>% unlist(),
                spe_data[,1][two_idx[2,i]] %>% strsplit("[.]") %>% unlist()) %>% length())/2
      s_b <- s_r/s_a

      result <- data.frame("Order"=paste0("q = ",q),
                           "MF_gamma"=rF,
                           "MF_alpha"=aF,
                           "MF_beta"=bF,
                           "Species_gamma"=s_r,
                           "Species_alpha"=s_a,
                           "Species_beta"=s_b)


      rF_tau <- lapply(tau,function(t){
        out <- data.frame("Order"=paste0("q = ",q),
                          "value"=sapply(q,function(y) qMF_diversity(d_x,y,t,transform_D,diversity = "gamma")),
                          "Tau"=t)
        out
      }) %>% do.call(rbind,.)

      aF_tau <- parallel::mclapply(tau,function(t){
        out <- data.frame("Order"=paste0("q = ",q),
                          "value"=sapply(q,function(y) qMF_diversity(d_x,y,t,transform_D,diversity = "alpha")),
                          "Tau"=t)
      }) %>% do.call(rbind,.)
      bF_tau <- rF_tau$value/aF_tau$value

      result_tau <- data.frame("Order"=rF_tau$Order,
                           "Tau"=rF_tau$Tau,
                           "MF_g"=rF_tau$value,
                           "MF_a"=aF_tau$value,
                           "MF_b"=bF_tau) %>% group_by(Order) %>%
        summarise(L_g = sum(MF_g[seq_along(MF_g[-1])]*diff(Tau)),
                  R_g = sum(MF_g[-1]*diff(Tau)),
                  L_a = sum(MF_a[seq_along(MF_a[-1])]*diff(Tau)),
                  R_a = sum(MF_a[-1]*diff(Tau)),
                  L_b = sum(MF_b[seq_along(MF_b[-1])]*diff(Tau)),
                  R_b = sum(MF_b[-1]*diff(Tau))) %>% ungroup %>%
        mutate(MF_gamma_cor = (L_g+R_g)/2,
               MF_alpha_cor = (L_a+R_a)/2,
               MF_beta_cor = (L_b+R_b)/2) %>%
        dplyr::select(c(MF_gamma_cor:MF_beta_cor))



      other_data <- data[,-fun_cols]
      other <- paste(other_data[two_idx[1,i],],other_data[two_idx[2,i],],sep = "-") %>%
        rep(each=length(q)) %>% matrix(nrow = length(q)) %>% as.data.frame()
      names(other) <- names(other_data)

      cbind(other,result,result_tau)

    }) %>% do.call(rbind,.)

  }

  return(output)
}


# For functions normalized
#
# \code{func_normalized} To get the normalized functions between 0 and 1.
#
# @param x a vector of the function variable.
# @param positive a logical variable to determine whether the function should be normalized positively or not.
# @param is_zero_miss a logical variable to determine whether the zero in data represents no value or not.
# @return a vector be normalized.

func_normalized <- function(x,positive=TRUE,is_zero_miss=TRUE){
  mi <- min(x,na.rm = T)
  ma <- max(x,na.rm = T)
  if(positive) y <- (x-mi)/(ma-mi)
  else y <- (ma-x)/(ma-mi)

  if(is_zero_miss) y[x==0] <- NA
  return(y)
}


# Calculate multi-functionality measures in a single ecosystem.
#
# \code{qMF} Calculate uncorrelated and correlated MF measures using special case of Hill-Chao numbers.
#
# @param w a vector of the weighted.
# @param v a vector of the ecosystem with several functions.
# @param q a numerical vector specifying the diversity orders.
# @param tau a single value used in the correlated MF measures.
# @param di a distance matrix used in the correlated MF measures.
# @return a value of correlated/uncorrelated MF measure.

qMF <- function(w=1,v,q,tau=0.5,di=NULL){
  if(is.null(di) | tau == 0){
    ai_tau <- v[v>0]
    V <- w*ai_tau
  }
  else{
    d_tau <- ifelse(di<tau,di,tau)
    ai <- (1-d_tau/tau)%*%v
    ai_tau <- ai[ai>0]
    v <- v[ai>0]
    V <- w*v*v/ai_tau
  }
  if(q==1) exp(-sum(V*(ai_tau/sum(V*ai_tau))*log(ai_tau/sum(V*ai_tau))))
  else sum(V*(ai_tau/sum(V*ai_tau))^q)^(1/(1-q))
}



qMF_diversity <- function(v,q,tau=0.5,di=NULL,diversity="gamma"){
  F_alpha <- function(w=1,v,q,R=c(0.5,0.5),N=2){
    v1 <- v
    v[is.na(v)] <- 0
    V <- w*(v%*%R)
    v_plus <- c()
    for (i in 1:N){
      v_plus <- cbind(v_plus,v[,i]*R[i])
    }
    if (q==1) {
      Vv <- ifelse(v_plus==0,0,(v_plus/sum(V*(v%*%R)))*log(v_plus/sum(V*(v%*%R))))
      1/N*exp(-sum(t(V)%*%Vv))
    }
    else {
      Vv <- ifelse(is.na(v1),0,(v_plus/sum(V*(v%*%R)))^q)
      1/N*(sum(V*rowSums(Vv)))^(1/(1-q))
    }
  }
  F_gamma <- function(w=1,v,q,R=c(0.5,0.5),N=2){
    v[is.na(v)] <- 0
    V <- w*(v%*%R)
    if(q==1) {
      Vv <- ifelse(v%*%R==0,0,((v%*%R)/sum(V*(v%*%R)))*log((v%*%R)/sum(V*(v%*%R))))
      exp(-sum(V*Vv))
    }
    else (sum(V*((v%*%R)/sum(V*(v%*%R)))^q))^(1/(1-q))
  }
  F_alphaa_tau <- function(w=1,v,q,tau,di,R=c(0.5,0.5),N=2){
    v1 <- v
    v[is.na(v)] <- 0
    d_tau <- ifelse(di<tau,di,tau)
    ai <- (1-d_tau/tau)%*%v
    f_bar <- rowSums(v)/N
    ai_bar <- (1-d_tau/tau)%*%f_bar
    v_tau <- ifelse(f_bar==0,0,f_bar*f_bar/ai_bar)
    V <- w*v_tau

    if (q==1) {
      Vv <- ifelse(ai==0,0,(ai/sum(V%*%ai))*log(ai/sum(V%*%ai)))
      1/N*exp(-sum(t(V)%*%Vv))
    }
    else {
      Vv <- ifelse(is.na(v1),0,(ai/sum(V%*%ai))^q)
      1/N*(sum(V*rowSums(Vv)))^(1/(1-q))
    }
  }
  F_gamma_tau <- function(w=1,v,q,tau,di,R=c(0.5,0.5),N=2){
    v[is.na(v)] <- 0
    d_tau <- ifelse(di<tau,di,tau)
    ai <- (1-d_tau/tau)%*%v
    f_bar <- rowSums(v)/N
    ai_bar <- (1-d_tau/tau)%*%f_bar
    v_tau <- ifelse(f_bar==0,0,f_bar*f_bar/ai_bar)
    V <- w*v_tau

    if(q==1) {
      Vv <- ifelse(ai_bar==0,0,((ai_bar)/sum(V*ai_bar))*log((ai_bar)/sum(V*ai_bar)))
      exp(-sum(V*Vv))
    }
    else (sum(V*((ai_bar)/sum(V*ai_bar))^q))^(1/(1-q))
  }

  if(is.null(di) | tau == 0){
    if(diversity=="gamma") out = F_gamma(v=v,q=q)
    else out = F_alpha(v=v,q=q)
  }
  else{
    if(diversity=="gamma") out = F_gamma_tau(v=v,q=q,tau = tau,di=di)
    else out = F_alphaa_tau(v=v,q=q,tau = tau,di=di)
  }
  return(out)
}




