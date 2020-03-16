#' Make an analyte plot
#'
#' @param data desc
#' @param anlayte desc
#' @param Title desc
#' @param Ylab desc
#' @param Xlab desc
#' @param Ylimit desc
#'
#' @export


analyteplot <- function(data, analyte, Title, Ylab = "mg/L", Xlab, Ylimt){
  ggplot()+
    geom_boxplot(data = data,
                 aes(x = Farm,
                     y = analyte,
                     fill= Treatment)
    )+
    ggtitle(Title)+
    # labs(caption= "*some outliers may be omitted for visualization purposes")+
    ylab(Ylab)+
    xlab(Xlab)+

    # coord_cartesian(ylim = c())+
    coord_cartesian(ylim= c(0,Ylimt))+
    theme(plot.title = element_text(size = 20, face = "bold"),
          text = element_text(size = 9),
          panel.background = element_rect(fill = "grey90" ))


}
