#' @title Easy Perceptual and Joint Space Maps from PCA
#' @description This function products percpeptual or joint spaced maps based
#'     on the first two principal components of a PCA
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item ggplot2
#'   \item dplyr
#' }
#' @param data A data frame containing the variables on which to perform the
#'     principal components analysis. Can include a preference variable.
#' @param group Name of the grouping variable. Must be in quotations. 
#' @param pref Name of the preference variable. Must be in quotations. Can be 
#'     excluded if no preference variable exists.
#' @return 
#'   \item If a \code{pref} variable is not included, returns a perceptual map
#'   \item If a \code{pref} variable is included, returns a joint space map
#' @examples
#' #Perceptual map
#' pmdata <- greekbrands %>%
#'    select(perform, leader, fun, serious bargain, value, brand)
#' percmap(pmdata, group="brand")
#' 
#' #Joint Space map
#' jsmdata <- greekbrands %>%
#'    select(perform, leader, fun, serious bargain, value, pref, brand)
#' percmap(pmdata, group="brand", pref="pref")

percmap <- function(data, group, pref) {
   require(dplyr)
   require(ggplot2)
   grpcnum <- grep(group, colnames(data))
   sum <- data %>% 
      group_by(data[grpcnum]) %>% 
      summarise_all(mean)
   sum <- as.data.frame(sum)
   rownames(sum) <- sum[,1]
   sum <- sum[,2:ncol(sum)]
   if(missing(pref)) {
      dsum <- sum
   } else {
      prefcnum <- grep(pref,colnames(sum))
      dsum <- sum[-prefcnum]
      pref <- sum[prefcnum] %>%
         mutate_all(~(scale(.)))
   }
   pcaobj <- prcomp(dsum, scale=TRUE, rank=2)
   PC1Var <- round((pcaobj$sdev[1]^2)/length(pcaobj$sdev),4)*100
   PC2Var <- round((pcaobj$sdev[2]^2)/length(pcaobj$sdev),4)*100
   rvarm <- varimax(pcaobj$rotation, normalize=FALSE)
   acoord <- data.frame(pcaobj$rotation%*%rvarm$rotmat)
   colnames(acoord) <- c("PC1", "PC2")
   dsumstd <- dsum %>%
      mutate_all(~(scale(.)))
   PC1 <- as.matrix(dsumstd[,1:ncol(dsumstd)])%*%matrix(acoord[,1])
   PC2 <- as.matrix(dsumstd[,1:ncol(dsumstd)])%*%matrix(acoord[,2])
   bcoord <- data.frame(PC1,PC2)
   bcmax <- max(abs(bcoord))
   bcoord$hj <- ifelse(bcoord$PC1>0,
                       ifelse(bcoord$PC1>bcmax*.75, 1.15,-.2),
                       ifelse(bcoord$PC1<(-bcmax*.75), -.2, 1.15))
   upscale <- (bcmax*2)/(max(abs(acoord))*2)
   acoord <- acoord*(upscale*.75)
   acoord$dist <- acoord$PC1^2 + acoord$PC2^2
   amax <- max(acoord$dist)
   acoord$an <- atan2(acoord$PC2,acoord$PC1)*57.29577
   acoord <- acoord %>%
      mutate(ang=case_when(an>45 & an<=135 ~ 45,
                           abs(an)>135 ~ 0,
                           an>(-135) & an<=(-45) ~ 45,
                           abs(an)<45 ~ 0),
             vj=case_when(an>45 & an<=135 ~ -.2,
                           abs(an)>135 ~ 0,
                           an>(-135) & an<=(-45) ~ 0,
                           abs(an)<45 ~ 1),
             hj=case_when(an>45 & an<=135 ~ 0,
                           abs(an)>135 ~ 1,
                           an>(-135) & an<=(-45) ~ 1,
                           abs(an)<45 ~ 0))
  if(missing(pref)) {
     ggplot() +
        geom_hline(yintercept=0) + geom_vline(xintercept=0) +
        geom_segment(data=acoord,
                     aes(x=0, y=0, xend=PC1, yend=PC2),
                     size=1, color="red4", 
                     arrow=arrow(ends="last", length=unit(0.25, "cm"))) +
        geom_text(data=acoord, aes(x=PC1, y=PC2), 
                  label=rownames(acoord), color="red4", size=5,  
                  angle=acoord$ang, vjust=acoord$vj, hjust=acoord$hj) +
        geom_point(data=bcoord, aes(x=PC1, y=PC2), color="navy", size=2) +
        geom_text(data=bcoord, aes(x=PC1, y=PC2), color="navy", 
                  label=rownames(bcoord), hjust=bcoord$hj, size=5, angle=45) +
        theme(aspect.ratio=1, axis.ticks=element_blank(), axis.text=element_blank(),
              panel.background=element_rect(fill="white", color="black")) +
        scale_x_continuous(limits=c(-bcmax,bcmax),
                           expand=c(0.07,0.07)) +
        scale_y_continuous(limits=c(-bcmax,bcmax),
                           expand=c(0.07,0.07)) +
        labs(x=paste0("PC1 (",PC1Var,"%)"), y=paste0("PC2 (",PC2Var,"%)"))
   } else {
      pcoord <- cbind(pref,PC1,PC2) %>%
         mutate(PC1=pref*PC1,
                PC2=pref*PC2) %>%
         select(PC1,PC2) %>%
         summarise_all(mean)
      pscale <- sqrt(amax/(pcoord$PC1^2 + pcoord$PC2^2))
      pcoord <- pcoord*pscale
      pcoord$an <- atan2(pcoord$PC2,pcoord$PC1)*57.29577
      pcoord <- pcoord %>%
         mutate(ang=case_when(an>45 & an<=135 ~ 45,
                              abs(an)>135 ~ 0,
                              an>(-135) & an<=(-45) ~ 45,
                              abs(an)<45 ~ 0),
                vj=case_when(an>45 & an<=135 ~ -.2,
                             abs(an)>135 ~ 0,
                             an>(-135) & an<=(-45) ~ 0,
                             abs(an)<45 ~ 1),
                hj=case_when(an>45 & an<=135 ~ 0,
                             abs(an)>135 ~ 1,
                             an>(-135) & an<=(-45) ~ 1,
                             abs(an)<45 ~ 0))
      ggplot() +
         geom_hline(yintercept=0) + geom_vline(xintercept=0) +
         geom_segment(data=pcoord,
                      aes(x=0, y=0, xend=PC1, yend=PC2),
                      size=1, color="darkgreen",
                      arrow=arrow(ends="last", length=unit(0.25, "cm"))) +
         geom_text(data=pcoord, aes(x=PC1, y=PC2),
                   label="Pref", color="darkgreen", size=5, 
                   angle=pcoord$ang, vjust=pcoord$vj, hjust=pcoord$hj) +
         geom_segment(data=acoord,
                      aes(x=0, y=0, xend=PC1, yend=PC2),
                      size=1, color="red4", 
                      arrow=arrow(ends="last", length=unit(0.25, "cm"))) +
         geom_text(data=acoord, aes(x=PC1, y=PC2), 
                   label=rownames(acoord), color="red4", size=5,  
                   angle=acoord$ang, vjust=acoord$vj, hjust=acoord$hj) +
         geom_point(data=bcoord, aes(x=PC1, y=PC2), color="navy", size=2) +
         geom_text(data=bcoord, aes(x=PC1, y=PC2), color="navy", 
                   label=rownames(bcoord), hjust=bcoord$hj, size=5, angle=45) +
         theme(aspect.ratio=1, axis.ticks=element_blank(), axis.text=element_blank(),
               panel.background=element_rect(fill="white", color="black")) +
         scale_x_continuous(limits=c(-bcmax,bcmax),
                            expand=c(0.07,0.07)) +
         scale_y_continuous(limits=c(-bcmax,bcmax),
                            expand=c(0.07,0.07)) +
         labs(x=paste0("PC1 (",PC1Var,"%)"), y=paste0("PC2 (",PC2Var,"%)"))
   }
}
