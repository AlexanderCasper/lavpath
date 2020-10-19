nameorder <- function(model) {
neu <-  dplyr::select(dplyr::filter(lavaan::parameterEstimates(model, standardized=TRUE),op == "~"),'Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, pvalue=pvalue, Beta=std.all)
neu[4:7] <- sapply(neu[4:7], round, 3)
neu2 <- neu[2:7]
neu$Indicator <- NULL
names(neu)[1] <- "Indicator"
neu3 <- rbind(neu,neu2)
names(neu3)[1] <- "variables"
neu3$variables <- as.factor(neu3$variables)
levels(neu3$variables)
}

