.logit5Confuse <-
function(lm.out, out, n.vars, nm, new.data, prob_cut) {

  # confusion matrix
  if (!new.data) {
    tbl <- table(out[,n.vars], out[,n.vars+1])  # actual and predicted label

    hit0 <- tbl[1,1]; hit1 <- tbl[2,2]
    mis0 <- tbl[1,2]; mis1 <- tbl[2,1]
    per0 <- hit0 / (hit0 + mis0); per1 <- hit1 / (hit1 + mis1)
    hitT <- hit0 + hit1
    tot0 <- tbl[1,1] + tbl[1,2]
    tot1 <- tbl[2,1] + tbl[2,2]
    totG <- tot0 + tot1
    perT <- hitT / totG
    per0G <- tot0 / totG
    per1G <- tot1 / totG
    ln <- nchar(nm[1])

    cat("Probability threshold for predicting ",
        levels(lm.out$model[,nm[1]])[2], ":", " ", prob_cut, "\n", sep="")
    cat("\n")
    cat(.fmtc(" ",ln+8), "Baseline         Predicted", "\n")
    .dash(51)
    cat(.fmtc(" ",ln+7), "Total  %Tot        0      1  %Correct", "\n")
    .dash(51)
    cat(.fmtc(" ",ln), "  0  ", .fmti(tot0,6), .fmt(100*per0G,1,5), " ",
        .fmti(hit0,6), .fmti(mis0,6), "   ",
        .fmt(100*per0,1), "\n")
    cat(.fmtc(nm[1],ln), "  1  ", .fmti(tot1,6), .fmt(100*per1G,1,5), " ",
        .fmti(mis1,6), .fmti(hit1,6), "   ",
        .fmt(100*per1,1), "\n")
    .dash(51)
    cat(.fmtc(" ",ln), "Total", .fmti(totG,6), .fmtc(" ",25),
        .fmt(100*perT,1), "\n")
    cat("\n")

    hit1 <- hitT - hit0
    mis1 <- tot1 - hit1
    accuracy <- ((hit0 + hit1) / (hit0 + hit1 + mis0 + mis1)) * 100
    recall <- ((hit1) / (hit1 + mis1)) * 100
    precision <- ((hit1) / (hit1 + mis0)) * 100
    cat("Accuracy:", .fmt(accuracy,2), "\n")
    cat("Recall:", .fmt(recall,2), "\n")
    cat("Precision:", .fmt(precision,2), "\n")
    cat("\n")
  }


}
