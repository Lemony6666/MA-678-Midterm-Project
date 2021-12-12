output1 <- function(table1){
  for(i in 1:ncol(table1)){
    table1[, i] <- as.character(table1[, i])
  }
  myft <- flextable::regulartable(table1, 
                                  col_keys = names(table1))
  myft <- set_header_labels(myft )
  myft <- merge_h(myft, part = "header")
  myft <- merge_v(myft, part = "header")
  myft <- theme_vanilla(myft)
  myft <- bg(myft, i = 1, bg = "#ADADAD", part = "header")
  myft <- flextable::align(myft, align = "center", part = "all" )
  myft <- border_remove(myft)
  myft <- hline_top(myft, part = "header", border = fp_border(width = 1))
  myft <- hline_bottom(myft, part = "header", border = fp_border(width = 1))
  myft <- hline(myft, i = 1, part = "header", border = fp_border(width = 1))
  myft <- hline_bottom(myft, part = "body", border = fp_border(width = 1))
  # myft <- autofit(myft)
  # myft <- width(myft, j = 1,  width = 1.5)
  # myft <- width(myft, j = 2:ncol(table1),  width = 0.8)
  myft
}

Compare_Dunn_two <- function(aa1){
  group_name <- names(aa1)[1]
  names(aa1)[1] <- "group"
  for (k in 2:ncol(aa1)) {
    variable_name <- names(aa1)[k]
    aa1[, c(1,k)]$group <- as.factor(aa1[, c(1,k)]$group)
    names(aa1)[c(1,k)] <- gsub("-", "_", names(aa1)[c(1,k)])
    names(aa1)[c(1,k)] <- gsub("/", "_", names(aa1)[c(1,k)])
    names(aa1)[c(1,k)] <- gsub(" ", "", names(aa1)[c(1,k)])
    names(aa1)[c(1,k)] <- gsub("\\(", "", names(aa1)[c(1,k)])
    names(aa1)[c(1,k)] <- gsub("\\)", "", names(aa1)[c(1,k)])
    names(aa1)[c(1,k)] <- gsub("（", "", names(aa1)[c(1,k)])
    names(aa1)[c(1,k)] <- gsub("）", "", names(aa1)[c(1,k)])
    gongshi <- as.formula(paste(names(aa1)[k], "~", names(aa1)[1]))
    tes <- PMCMRplus::kwAllPairsDunnTest(gongshi, data = aa1, p.adjust="bonf")
    Statistic <- as.data.frame(tes$statistic)
    p_value <- as.data.frame(tes$p.value)
    for (j in 1:ncol(p_value)) {
      tabl <- as.data.frame(matrix(data = NA, nrow = (ncol(p_value)-(j-1)), ncol = 3))
      for (i in j:nrow(p_value)) {
        tabl[i-(j-1), 1] <- paste(names(p_value)[j], "-", rownames(p_value)[i])
        tabl[i-(j-1), 2] <- Statistic[i, j]
        tabl[i-(j-1), 3] <- p_value[i, j]
      }
      if(j==1)
        tabl1 <- tabl
      else
        tabl1 <- rbind(tabl1, tabl)
    }
    tabl1[, 2:3] <- lapply(tabl1[, 2:3], as.numeric)
    tabl1[, 2:3] <- round(tabl1[, 2:3], 3)
    names(tabl1) <- c(group_name, "Stat.", "P_value")
    tabl1$`Variable` <- ""
    tabl1$`Variable`[1] <- variable_name
    tabl1 <- tabl1[, c(ncol(tabl1), 1:ncol(tabl1)-1)]
    if(k==2)
      tabl2 <- tabl1
    else
      tabl2 <- rbind(tabl2, tabl1)
  }
  for (i in 1:nrow(tabl2)) {
    if(tabl2$`P_value`[i] > 0.999){
      tabl2$`P_value`[i] <- "> 0.999"
    }else if(tabl2$`P_value`[i] < 0.001){
      tabl2$`P_value`[i] <- "< 0.001"
    }
  }
  output1(tabl2)
}