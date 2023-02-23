#' ggplot2 extension for a mf object
#'
#' \code{ggmf}: the \code{\link[ggplot2]{ggplot}} extension for \code{mf} object to plot the correlation between species richness and multi-functionality
#'
#' @param output the output from \code{mf}
#' @param scale Are scales shared across all facets (the default, \code{"fixed"}), or do they vary across rows (\code{"free_x"}), columns (\code{"free_y"}), or both rows and columns (\code{"free"})?
#' @param fit.lm If \code{TRUE}, the default, fitted line of linear model and estimated of slope are displayed.
#'
#' @import tidyverse
#' @import dplyr
#' @import broom
#'
#' @return a figure for multi-functionality with uncorrelated/correlated functions.
#'
#' @examples
#' ## Display fitted line of linear model
#' data(output1)
#' ggmf(output1, scale = 'fixed', fit.lm = TRUE)
#'
#' ## Not display fitted line of linear model
#' ggmf(output1, scale = 'fixed', fit.lm = FALSE)
#'
#' @export
ggmf = function(output, scale = 'fixed', fit.lm = TRUE){

  #cbPalette <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
  if(length(unique(output$Type))==2) output$Type <- factor(output$Type, levels = c("uncorrelated", "correlated"))

  if(fit.lm){
    lm_data <- output %>% group_by(Type, Order.q) %>% do(broom::tidy(lm(qMF ~ target_species_richness, .)))
    lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
      filter(term=="target_species_richness") %>% dplyr::select(-c(term, std.error, statistic, p.value))
    output <- suppressMessages(output %>% dplyr::left_join(lm_data))
    ggplot(data = output, aes(x = target_species_richness, y = qMF))+
      facet_grid(Order.q ~ Type, scales = scale) +
      geom_point(aes(col = Country), size=0.8)+
      geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 0.5, col = 1, formula = y ~ x)+
      geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
      #scale_colour_manual(values = cbPalette) +
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0), legend.title = element_blank())+
      labs(x = "Species Rishness", y = "Multi-functionality")
  }else{
    ggplot(data = output, aes(x = target_species_richness, y = qMF))+
      facet_grid(Order.q ~ Type, scales = scale) +
      geom_point(aes(col = Country), size=0.8)+
      #scale_colour_manual(values = cbPalette) +
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0), legend.title = element_blank())+
      labs(x = "Species Rishness", y = "Multi-functionality")
  }
}


#' ggplot2 extension for a mfbeta object
#'
#' \code{ggmfbeta}: the \code{\link[ggplot2]{ggplot}} extension for \code{mfbeta} object to plot the relationship between decomposition of species richness and multi-functionality
#'
#' @param output the output from \code{mfbeta}
#' @param scale Are scales shared across all facets (the default, \code{"fixed"}), or do they vary across rows (\code{"free_x"}), columns (\code{"free_y"}), or both rows and columns (\code{"free"})?
#' @param fit.lm If \code{TRUE}, the default, fitted line of linear model and estimated of slope are displayed.
#'
#' @import tidyverse
#' @import dplyr
#' @import broom
#' @import patchwork
#'
#' @return return a figure when input contains only uncorrelated or correlated MF decomposition. returns a list of two figures when input contains both uncorrelated and correlated MF decomposition.
#'
#' @examples
#' ## Display fitted line of linear model
#' data(output2)
#' ggmfbeta(output2, scale = 'fixed', fit.lm = TRUE)
#'
#' ## Not display fitted line of linear model
#' ggmfbeta(output2, scale = 'fixed', fit.lm = FALSE)
#'
#' @export
ggmfbeta = function(output, scale = 'fixed', fit.lm = TRUE){

  #cbPalette <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
  if("MF_beta"%in%colnames(output) & !("MF_beta_cor"%in%colnames(output))){
    if(fit.lm){
    lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_gamma ~ Species_gamma, .)))
    lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
      filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
    gamma <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
      ggplot(aes(x = Species_gamma, y = MF_gamma))+
      facet_grid(~Order, scales = scale) +
      geom_point(aes(col = target_species_richness), size=0.8)+
      geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
      geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
      scale_color_discrete(name = "Species \nCombination")+
      theme_bw() +
      theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
      labs(x = "Species Gamma", y = "MF Gamma")

    lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_alpha ~ Species_alpha, .)))
    lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
      filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
    alpha <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
      ggplot(aes(x = Species_alpha, y = MF_alpha))+
      facet_grid(~Order, scales = scale) +
      geom_point(aes(col = target_species_richness), size=0.8)+
      geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
      geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
      scale_color_discrete(name = "Species \nCombination")+
      theme_bw() +
      theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
      labs(x = "Species Alpha", y = "MF Alpha")

    lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_beta ~ Species_beta, .)))
    lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
      filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
    beta <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
      ggplot(aes(x = Species_beta, y = MF_beta))+
      facet_grid(~Order, scales = scale) +
      geom_point(aes(col = target_species_richness), size=0.8)+
      geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
      geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
      scale_color_discrete(name = "Species \nCombination")+
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
      labs(x = "Species Beta", y = "MF Beta")

  }else{
    gamma <- output %>%
      ggplot(aes(x = Species_gamma, y = MF_gamma))+
      facet_grid(~Order, scales = scale) +
      geom_point(aes(col = target_species_richness), size=0.8)+
      scale_color_discrete(name = "Species \nCombination")+
      theme_bw() +
      theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
      labs(x = "Species Gamma", y = "MF Gamma")

    alpha <- output %>%
      ggplot(aes(x = Species_alpha, y = MF_alpha))+
      facet_grid(~Order, scales = scale) +
      geom_point(aes(col = target_species_richness), size=0.8)+
      scale_color_discrete(name = "Species \nCombination")+
      theme_bw() +
      theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
      labs(x = "Species Alpha", y = "MF Alpha")

     beta <- output %>%
      ggplot(aes(x = Species_beta, y = MF_beta))+
      facet_grid(~Order, scales = scale) +
      geom_point(aes(col = target_species_richness), size=0.8)+
       scale_color_discrete(name = "Species \nCombination")+
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
      labs(x = "Species Beta", y = "MF Beta")
  }
    gamma/alpha/beta
  }else if("MF_beta_cor"%in%colnames(output) & !("MF_beta"%in%colnames(output))){
    if(fit.lm){
      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_gamma_cor ~ Species_gamma, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      gamma <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_gamma, y = MF_gamma_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Gamma", y = "MF Gamma")

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_alpha_cor ~ Species_alpha, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      alpha <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_alpha, y = MF_alpha_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Alpha", y = "MF Alpha")

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_beta_cor ~ Species_beta, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      beta <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_beta, y = MF_beta_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Beta", y = "MF Beta")
    }else{
      gamma <- output %>%
        ggplot(aes(x = Species_gamma, y = MF_gamma_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Gamma", y = "MF Gamma")

      alpha <- output %>%
        ggplot(aes(x = Species_alpha, y = MF_alpha_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Alpha", y = "MF Alpha")

      beta <- output %>%
        ggplot(aes(x = Species_beta, y = MF_beta_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Beta", y = "MF Beta")
    }
    gamma/alpha/beta
  }else{
    if(fit.lm){
      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_gamma ~ Species_gamma, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      gamma <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_gamma, y = MF_gamma))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Gamma", y = "MF Gamma")

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_alpha ~ Species_alpha, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      alpha <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_alpha, y = MF_alpha))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Alpha", y = "MF Alpha")

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_beta ~ Species_beta, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      beta <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_beta, y = MF_beta))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Beta", y = "MF Beta")
      uncorrelated <- gamma/alpha/beta

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_gamma_cor ~ Species_gamma, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      gamma <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_gamma, y = MF_gamma_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Gamma", y = "MF Gamma")

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_alpha_cor ~ Species_alpha, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      alpha <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_alpha, y = MF_alpha_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Alpha", y = "MF Alpha")

      lm_data <- output %>% group_by(Order) %>% do(broom::tidy(lm(MF_beta_cor ~ Species_beta, .)))
      lm_data <- mutate(lm_data, Significance=factor(ifelse(p.value<0.05, "Significant slope (P < 0.05)", "Insignificant slope"), levels = c("Significant slope (P < 0.05)", "Insignificant slope"))) %>%
        filter(term!="(Intercept)") %>% dplyr::select(-c(term, std.error, statistic, p.value))
      beta <- suppressMessages(output %>% dplyr::left_join(lm_data)) %>%
        ggplot(aes(x = Species_beta, y = MF_beta_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        geom_smooth(aes(lty = Significance), method = "lm", se = F, linewidth = 1, col = 1, formula = y ~ x)+
        geom_text(data = lm_data, aes(x = -Inf, y = Inf, label=paste0("Slope = ", round(estimate, 4))), hjust= -0.1, vjust= 2, size=3, col="#E41A1C")+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Beta", y = "MF Beta")
      correlated <- gamma/alpha/beta
    }else{
      gamma <- output %>%
        ggplot(aes(x = Species_gamma, y = MF_gamma))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Gamma", y = "MF Gamma")

      alpha <- output %>%
        ggplot(aes(x = Species_alpha, y = MF_alpha))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Alpha", y = "MF Alpha")

      beta <- output %>%
        ggplot(aes(x = Species_beta, y = MF_beta))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Beta", y = "MF Beta")
      uncorrelated <- gamma/alpha/beta

      gamma <- output %>%
        ggplot(aes(x = Species_gamma, y = MF_gamma_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Gamma", y = "MF Gamma")

      alpha <- output %>%
        ggplot(aes(x = Species_alpha, y = MF_alpha_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "none", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Alpha", y = "MF Alpha")

      beta <- output %>%
        ggplot(aes(x = Species_beta, y = MF_beta_cor))+
        facet_grid(~Order, scales = scale) +
        geom_point(aes(col = target_species_richness), size=0.8)+
        scale_color_discrete(name = "Species \nCombination")+
        theme_bw() +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin=margin(-6,0,0,0))+
        labs(x = "Species Beta", y = "MF Beta")

      correlated <- gamma/alpha/beta
    }
    return(list(uncorrelated=uncorrelated, correlated=correlated))
  }

}


