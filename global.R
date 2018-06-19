require(shiny)
require(shinythemes)
require(PerFit)
require(plotly)
require(ltm)
require(DT)


# col <- colorRampPalette(c("snow3", "yellowgreen"))(ncol(prop))
col <- c("yellowgreen", "snow3")

mycss <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: yellowgreen;
border-color: yellowgreen;
}
.shiny-input-container {
margin-bottom: 0px;
padding-bottom: 0px !important;
padding-top: 0px;}
.progress-bar{
margin-bottom: 0px;
padding-bottom: 0px !important;
}
.#file_progress{ 
margin-bottom: 2px;
}
.center {
    display: block;
margin-left: auto;
margin-right: auto;
}

td.small .shiny-input-container{width:auto;}
td.small{width:30px;}
"


plotPerc <- function(data, xlabsize){
  prop <- data.frame(p = apply(data, 2, mean))
  prop$p <- sort(prop$p, decreasing = TRUE)
  prop$q <- 1 - prop$p
  plot_ly(x = rownames(prop), y = prop[,1], type = "bar", 
          marker = list(color = col[1]))%>%
    layout(xaxis = list(title = "", type = 'categorical', nticks = nrow(prop),
                        tickvals = rownames(prop), ticktext = rownames(prop),
                        tickfont = list(size = xlabsize)), 
           yaxis = list(title = "Proportion of correct answers"))
  }


plotItems <- function(data){ 
  prop.punct <- prop.table(table(apply(data, 1, sum)))
  prop.punct <- data.frame(punct = names(prop.punct), p = as.vector(prop.punct))
  
  scores <- data.frame(scores = c(0:ncol(data)), p = 0)
  scores$p[scores$scores %in% prop.punct$punct] <- prop.punct$p

    plot_ly(x = scores$scores, 
          y = scores$p, type = "bar", marker = list(color = col[1]))%>%
    layout(xaxis = list(title = "Overall score", type = 'categorical', nticks = nrow(scores),
                        tickvals = paste(scores$scores), ticktext = paste(scores$scores),
                        range = c(0, nrow(scores))), 
           yaxis = list(title = ""))
  }

###############

Sanity.cls <- function(x)
{
  if (class(x) != "PerFit")
  {
    stop('Object "x" is not of class PerFit. Aborted.')
  }
}

plot.PerFit <- function(x, UDlvl = NA, ...)
{  

  Sanity.cls(x)  
  # 
  upp.PFS  <- c("Cstar", "C.Sato", "U3", "ZU3", "G", "Gnormed", "Gpoly", "Gnormed.poly", "U3poly", "D.KB")
  low.PFS  <- c("r.pbis", "NCI", "Ht", "A.KB", "E.KB", "lz", "lzstar", "lzpoly")
  # 
  pf.scores       <- x$PFscores[[1]]
  PFS.NA          <- is.na(pf.scores)
  pf.scores.noNAs <- pf.scores[!PFS.NA]
  
  cutoff.res <- cutoff(x, UDlvl = UDlvl)
  
  
  x.line       <- cutoff.res$Cutoff
  # 
  PFS.flagged  <- flagged.resp(x, cutoff.res, UDlvl = UDlvl, scores = FALSE)[[1]][,2]
  # Find correct scale for y-axis:
  dens <- density(pf.scores.noNAs)
  ymax <- max(dens$y)
  
  p <- plot_ly(x = dens$x, y = dens$y, 
               type = 'scatter', mode = 'lines', line = list(color = 'rgba(205, 201, 201, 1)'))
  if(x$PFStatistic %in% low.PFS){
    p <- add_trace(p, x = dens$x[dens$x<x.line], y = dens$y[dens$x<x.line], fill = 'tozeroy', fillcolor = 'rgba(154, 205, 50, 1)')
  }else{if(x$PFStatistic %in% upp.PFS){
    p <- add_trace(p, x = dens$x[dens$x>x.line], y = dens$y[dens$x>x.line], fill = 'tozeroy', fillcolor = 'rgba(154, 205, 50, 1)')
  }}
  p <- add_trace(p, x = c(x.line, x.line), 
                 y = c(0, max(density(pf.scores.noNAs)$y)), mode = "lines", 
                 line = list(color = 'rgba(102, 102, 102, 1)'))
  p <- add_trace(p, x = c(cutoff.res$Cutoff.CI[1], cutoff.res$Cutoff.CI[2]), 
                 y = c(0, 0), mode = "lines", 
                 line = list(color = 'rgba(102, 102, 102, 1)', width = 10))
  for(i in PFS.flagged){
    p <- add_trace(p, x = i, 
                   y = max(density(pf.scores.noNAs)$y), mode = "markers", 
                   color = I('rgba(154, 205, 50, 1)'))
  } 
  p <- layout(p, showlegend = FALSE)
  p  
  
}


################

AllFlagged <- function(indexsList, UDlvl, IRT.PModel){
  # indexs <- c("Cstar", "C.Sato", "U3", "Ht", "lz")
  Indexs <- indexsList
  Cutoff <- lapply(Indexs, function(x) cutoff(x))
  UDlvl <- lapply(Cutoff, function(x) {
    switch(UDlvl,
           "value" = NA, 
           "conservativeIC" = if(x$Tail == "upper"){x$Cutoff.CI[2]}else{x$Cutoff.CI[1]},
           "extremeIC" = if(x$Tail == "upper"){x$Cutoff.CI[1]}else{x$Cutoff.CI[2]})
    })
    
  Flagged <- lapply(seq_along(Cutoff), function(x) flagged.resp(Indexs[[x]], ord = TRUE, UDlvl = UDlvl[[x]]))
  names(Flagged) <- names(Indexs)
  FlaggedIDs <- sort(unique(unlist(lapply(Flagged, function(x) x$Scores[,1]))))
  AllFlagged <- as.data.frame(matrix(data = NA, nrow = length(FlaggedIDs), ncol = length(Indexs) + 1, dimnames = list(c(1:length(FlaggedIDs)), c("FlaggedIDs", paste(names(Indexs))))))
  AllFlagged$FlaggedIDs <- FlaggedIDs
  for(i in (names(Indexs))){
    AllFlagged[which(AllFlagged$FlaggedIDs %in% Flagged[[i]]$Scores[,1]), i] <- Flagged[[i]]$Scores[,ncol(Flagged[[i]]$Scores)]
  }
  AllFlagged$All <- apply(AllFlagged, 1, function(x) if(sum(is.na(x)) == 0) {"*"} else {NA})
  AllFlagged
}


profiles <- function(flagged.dataframe){
  profiles.all <- matrix(0, nrow = nrow(flagged.dataframe), ncol = ncol(flagged.dataframe) - 2)
  colnames(profiles.all) <- colnames(flagged.dataframe)[c(-1,-ncol(flagged.dataframe))]
  profiles.all[!is.na(flagged.dataframe[,c(-1,-ncol(flagged.dataframe))])] <- 1
  profiles.all <- as.data.frame(profiles.all)
  
  require(utils)
  aux <- lapply(seq_along(colnames(profiles.all)), function(x) c(0, 1))
  names(aux) <- colnames(profiles.all)
  profiles <- expand.grid(aux)
  profiles <- profiles[-1,]
  
  require(plyr)
  profiles$count <- unlist(lapply(c(1:nrow(profiles)), function(x){
    nrow(match_df(profiles.all, profiles[x,]))
  }))
  

  profiles$perc <- (profiles$count/sum(profiles$count))*100
  
  profiles$flags <- apply(profiles[,1:5], 1, sum)
  
  prop.tab <- aggregate(profiles$perc, by=list(Category = profiles$flags), FUN = sum)
  prop.tab <- prop.tab[order(prop.tab$Category, decreasing = TRUE),]
  
  profiles <- profiles[order(profiles$flags, decreasing = TRUE),]
  profiles$overall.perc <- NA
  tab <- as.data.frame(table(profiles$flags))
  tab <- tab[order(tab$Var1, decreasing = TRUE), ]
  ind.hline <- cumsum(tab$Freq)[-6]
  ind <- c(1, ind.hline+1)[-6]
  profiles$overall.perc[ind] <- prop.tab$x
  
  
  profiles$flags <- NA
  profiles$flags[ind] <- tab$Var1
  
  profiles
}

##########
unidimTest.jorge1 <- function (object, data, thetas, IRT = TRUE, z.vals = NULL, B = 100, ...)
{
  if (missing(object) && (missing(data) & missing(thetas)))
    stop("either 'object' or both 'data' and 'thetas' must be supplied.\n")
  if (missing(object)) {
    if (!inherits(data, "matrix") && !inherits(data, "data.frame"))
      stop("'data' must be either a data.frame or a matrix")
    data <- data.matrix(data)
    if (any(its <- apply(data, 2, function(x) {
      x <- x[!is.na(x)]
      length(unique(x))
    }) > 2))
      stop("'data' contain more that 2 distinct values for item(s): ",
           paste(which(its), collapse = ", "))
   
    data.01s <- apply(data, 2, function(x) if (all(unique(x) %in% c(1, 0, NA))) FALSE else TRUE)
    if (any(data.01s))
      stop("'data' is not dichotomous (coded 0, 1) for item(s): ",
           paste(which(data.01s), collapse = ", "))
    
    data <- data[complete.cases(data), ]
    n <- nrow(data)
    if (n == 0)
      stop("zero rows after case-wise missing values deletion.\n")
    p <- ncol(data)
    if (nrow(thetas) != p)
      stop("the dimensions of 'data' and 'thetas' do not much.\n")
    parms <- thetas
  } else {
    if (!class(object) %in% c("ltm", "rasch", "tpm"))
      stop("Use only with 'ltm', 'rasch' or 'tpm' objects.\n")
    if (inherits(object, "ltm") && any(c(object$ltst$factors >
                                         1, object$ltst$quad.z1)))
      stop("\nfor 'ltm' objects it is assumed that the two-parameter logistic model has been fitted\n\t(i.e., one latent variable and no nonlinear terms).")
    data <- object$X
    data <- data.matrix(data)
    data <- data[complete.cases(data), ]
    n <- nrow(data)
    if (n == 0)
      stop("\nzero rows after case-wise missing values deletion.\n")
    p <- ncol(data)
    parms <- if (inherits(object, "tpm"))
      cbind(object$coef[, 2:3], plogis(object$coef[, 1])) else object$coef
    fsc <- factor.scores(object, resp.patterns = data)$score.dat
    ablts <- fsc$z1
    se.ablts <- fsc$se.z1
    IRT <- FALSE
  }

  eigenRho <- function(data, ...) {

    rho <- psych::polychoric(data, delete = FALSE)$rho
    rho. <- rho
    diag(rho.) <- max(rho * upper.tri(rho))
    list(Rho = rho, ev = eigen(rho., symmetric = TRUE, only.values = TRUE)$values) # or eigs(rho., 2), see other function
  }
  eigR <- eigenRho(data)
  rho <- eigR$Rho
  Tobs <- eigR$ev
  T.boot <- matrix(0, B, length(Tobs))
  withProgress(message = 'Computing', value = 0, {
    # !!!!
  for (b in 1:B) {
    if (!missing(object)) z.vals <- rnorm(n, ablts, se.ablts)
    data.new <- rmvlogis(n, parms, IRT = IRT, z.vals = z.vals)
    T.boot[b, ] <- eigenRho(data.new)$ev
    incProgress(1/(B))
  }
  })
  # !!!
  pval <- (sum(T.boot[, 2] >= Tobs[2], na.rm = TRUE) + 1)/(B + 1)
  if (!is.null(cnams <- colnames(data)))
    dimnames(rho) <- list(cnams, cnams)
  out <- list(Tobs = Tobs, T.boot = T.boot, p.value = pval,
              Rho = rho, call = if (missing(object)) NULL else object$call)
  class(out) <- "unidimTest"
  out
}


# 
# unidimTest <- function (object, data, thetas, IRT = TRUE, z.vals = NULL, B = 1000, ...) {
#   if (missing(object) && (missing(data) & missing(thetas)))
#     stop("either 'object' or both 'data' and 'thetas' must be supplied.\n")
#   if (missing(object)) {
#     if (!inherits(data, "matrix") && !inherits(data, "data.frame"))
#       stop("'data' must be either a data.frame or a matrix")
#     data <- data.matrix(data)
#     if (any(its <- apply(data, 2, function (x) { x <- x[!is.na(x)]; length(unique(x)) } ) > 2))
#       stop("'data' contain more that 2 distinct values for item(s): ", paste(which(its), collapse = ", "))
#     data <- apply(data, 2, function (x) if (all(unique(x) %in% c(1, 0, NA))) x else x - 1)
#     data <- data[complete.cases(data), ]
#     n <- nrow(data)
#     if (n == 0)
#       stop("zero rows after case-wise missing values deletion.\n")
#     p <- ncol(data)
#     if (nrow(thetas) != p)
#       stop("the dimensions of 'data' and 'thetas' do not match.\n")
#     parms <- thetas
#   } else {
#     if (!class(object) %in% c("ltm", "rasch", "tpm"))
#       stop("Use only with 'ltm', 'rasch' or 'tpm' objects.\n")
#     if (inherits(object, "ltm") && any(c(object$ltst$factors > 1, object$ltst$quad.z1)))
#       stop("\nfor 'ltm' objects it is assumed that the two-parameter logistic model has been fitted\n\t(i.e., one latent variable and no nonlinear terms).")
#     data <- object$X    
#     data <- data.matrix(data)
#     data <- data[complete.cases(data), ]
#     n <- nrow(data)
#     if (n == 0)
#       stop("\nzero rows after case-wise missing values deletion.\n")
#     p <- ncol(data)
#     parms <- if (inherits(object, "tpm")) cbind(object$coef[, 2:3], plogis(object$coef[, 1])) else object$coef
#     fsc <- ltm::factor.scores(object, resp.patterns = data)$score.dat
#     ablts <- fsc$z1
#     se.ablts <- fsc$se.z1
#     IRT  <- FALSE
#   }
#   ind <- t(combn(p, 2))
#   n.ind <- nrow(ind)
#   #withProgress(message = 'Computing', value = 0, {
#     eigenRho <- function (data, ...) {
#       rho <- diag(p)
#       for (i in 1:n.ind) {
#         r <- ind[i, ]
#         rho[rbind(r, rev(r))] <- polychor(data[, r[1]], data[, r[2]], ...)
#         #incProgress(1/(n.ind+B*n.ind))
#       }
#       rho. <- rho
#       diag(rho.) <- rep(max(rho[ind]), p)
#       list(Rho = rho, ev = eigen(rho., symmetric = TRUE, only.values = TRUE)$values)
#     }
#     eigR <- eigenRho(data)
#     rho <- eigR$Rho
#     Tobs <- eigR$ev
#     T.boot <- matrix(0, B, length(Tobs))
#     for (b in 1:B) {
#       if (!missing(object))
#         z.vals <- rnorm(n, ablts, se.ablts)
#       data.new <- rmvlogis(n, parms, IRT = IRT, z.vals = z.vals)
#       T.boot[b, ] <- eigenRho(data.new)$ev
#     }
#     
#   #})
#   pval <- (sum(T.boot[, 2] >= Tobs[2], na.rm = TRUE) + 1) / (B + 1)
#   if (!is.null(cnams <- colnames(data)))
#     dimnames(rho) <- list(cnams, cnams)
#   out <- list(Tobs = Tobs, T.boot = T.boot, p.value = pval, Rho = rho, 
#               call = if (missing(object)) NULL else object$call)
#   class(out) <- "unidimTest"
#   
#   out
# }

plot.unidimTest <- function(x, ...){
  q95 <- as.numeric(quantile(x$T.boot[,2], c(.95)))
  
  plot_ly(x = x$T.boot[,2], type = "histogram", histnorm = "probability",
          marker = list(color = 'rgba(205, 201, 201, 1)'), 
          name = "Simulated values")%>%
    add_segments(x=q95, y=0, xend=q95, yend=1, line=list(color='rgba(102, 102, 102, 1)'), 
                 marker = list(color = "transparent"), name = "5% significance",
                 showlegend = TRUE)%>%
    add_trace(x = x$Tobs[2], y = 0, type = "scatter", mode = "markers", 
              marker = list(color = 'rgba(154, 205, 50, 1)'),  showlegend = TRUE,
              name = "Observed value")%>%
    layout(showlegend = TRUE, legend = list(xanchor = "right", yanchor = "top"))
}


probs.plm <- function(IP, Th, const = FALSE)
{
  N <- length(Th)
  I <- nrow(IP)
  if (const) IP[, 1] <- 1.702 * IP[, 1]
  res <- matrix(NA, N, I)
  for (i in 1:I)
  {
    res[, i] <- IP[i, 3] + (1 - IP[i, 3]) / (1 + exp(-(IP[i, 1]*(Th - IP[i, 2]))))
  }
  return(res)
}

# This is the main MODFIT() function:
# I adapted the code from my GGUM package, so at points it might look 'funny'.
MODFIT <- function (data, IP, const = TRUE, precision = 4)
{
  N <- nrow(data)
  I <- ncol(data)
  N.NAs <- N - colSums(is.na(data))
  #  if (I <= 10) {
  doublets <- t(combn(I, 2))
  triplets <- t(combn(I, 3))
  #  } else {
  #    obs.props <- colMeans(data > 0, na.rm = TRUE)
  #    group.low <- sort(order(obs.props)[1:ceiling(I/3)])
  #    group.med <- sort(order(obs.props)[(ceiling(I/3) + 1):ceiling(2 * I/3)])
  #    group.high <- sort(order(obs.props)[(ceiling(2 * I/3) + 1):I])
  #    groups <- cbind(group.low = group.low[1:floor(I/3)],
  #                    group.med[1:floor(I/3)], group.high[1:floor(I/3)])
  #    packets <- lapply(seq_len(nrow(groups)), function(row) sort(groups[row, ]))
  #    if ((I%%3) == 1) {
  #      packets[[1]][4] <- group.low[ceiling(I/3)]
  #    }
  #    if ((I%%3) == 2) {
  #      packets[[1]][4] <- group.low[ceiling(I/3)]
  #      packets[[2]][4] <- group.med[ceiling(I/3)]
  #    }
  #    doublets <- matrix(unlist(lapply(packets, function(x) combn(x, 2))), ncol = 2, byrow = TRUE)
  #    triplets <- matrix(unlist(lapply(packets, function(x) combn(x, 3))), ncol = 3, byrow = TRUE)
  #  }
  N.NAs.doublets <- apply(doublets, 1, function(vec) N - sum(rowSums(is.na(data[, vec])) > 0))
  N.NAs.triplets <- apply(triplets, 1, function(vec) N - sum(rowSums(is.na(data[, vec])) > 0))
  nodes.chi <- seq(-3, 3, length.out = 61)
  N.nodes.chi <- length(nodes.chi)
  weights <- dnorm(nodes.chi)/sum(dnorm(nodes.chi))
  probs.array <- array(NA, dim = c(N.nodes.chi, I, 2))
  probs.array[, , 2] <- probs.plm(IP, nodes.chi, const)
  probs.array[, , 1] <- 1 - probs.array[, , 2]
  
  weights.arr <- array(rep(weights, I * 2), c(N.nodes.chi, I, 2))
  N.NAs.mat <- matrix(rep(N.NAs, 2), nrow = I, byrow = FALSE)
  expected.mat.drasgow <- N.NAs.mat * apply((probs.array * weights.arr), 2:3, sum)
  observed.mat.drasgow <- t(apply(data, 2, function(vec) table(factor(vec, levels = 0:1))))
  expected.order <- t(apply(expected.mat.drasgow, 1, order))
  expected.mat.drasgow <- t(sapply(1:I, function(it) expected.mat.drasgow[it, expected.order[it, ]]))
  observed.mat.drasgow <- t(sapply(1:I, function(it) observed.mat.drasgow[it, expected.order[it, ]]))
  expected.mat.drasgow.less5 <- rowSums(expected.mat.drasgow < 5, na.rm = TRUE)
  N.expected.mat.drasgow.less5 <- sum(expected.mat.drasgow.less5 > 0)
  pos.expected.mat.drasgow.less5 <- which(expected.mat.drasgow.less5 > 0)
  df <- rep(1, I)
  if (N.expected.mat.drasgow.less5 > 0) {
    sapply(1:N.expected.mat.drasgow.less5, function(it) {
      item <- pos.expected.mat.drasgow.less5[it]
      pos.sum <- expected.mat.drasgow.less5[item]
      if (sum(expected.mat.drasgow[item, 1:pos.sum]) < 5)
      {
        pos.sum <- pos.sum + 1
      }
      expected.mat.drasgow[item, pos.sum] <- sum(expected.mat.drasgow[item, 1:pos.sum])
      expected.mat.drasgow[item, 1:(pos.sum - 1)] <- 1
      observed.mat.drasgow[item, pos.sum] <- sum(observed.mat.drasgow[item, 1:pos.sum])
      observed.mat.drasgow[item, 1:(pos.sum - 1)] <- 1
      df[item] <- 2 - pos.sum
    })
  }
  chisq <- rowSums(((observed.mat.drasgow - expected.mat.drasgow)^2)/expected.mat.drasgow,
                   na.rm = TRUE)
  chisq.df <- chisq/df
  chisq.adj <- sapply(1:I, function(it) max(0, 3000 * (chisq[it] - df[it])/N.NAs[it] + df[it]))
  chisq.adj.df <- chisq.adj/df
  singlets.res <- cbind(Item = 1:I, N.NAs, df, chisq, chisq.df, chisq.adj, chisq.adj.df)
  doublets.NAs <- cbind(doublets, N.NAs.doublets)
  doublets.res <- t(apply(doublets.NAs, 1, function(vec) {
    item1 <- vec[1]
    item2 <- vec[2]
    N.NAs.d <- vec[3]
    probs.array.item1 <- probs.array[, item1, ]
    probs.array.item2 <- probs.array[, item2, ]
    probs.array.item1.arr <- array(rep(probs.array.item1, 2), c(N.nodes.chi, 2, 2))
    probs.array.item2.arr <- array(rep(probs.array.item2, 2), c(N.nodes.chi, 2, 2))
    probs.array.item2.arr <- aperm(probs.array.item2.arr, c(1, 3, 2))
    weights.arr2 <- array(rep(weights.arr, 2 * 2), c(N.nodes.chi, 2, 2))
    expected.mat.drasgow.it1.it2 <- N.NAs.d * apply(probs.array.item1.arr *
                                                      probs.array.item2.arr * weights.arr2,
                                                    2:3, sum)
    observed.mat.drasgow.it1.it2 <- table(factor(data[, item1], levels = 0:1),
                                          factor(data[, item2], levels = 0:1))
    
    expected.order.it1.it2 <- order(c(expected.mat.drasgow.it1.it2))
    expected.mat.drasgow.it1.it2 <- c(expected.mat.drasgow.it1.it2)[expected.order.it1.it2]
    observed.mat.drasgow.it1.it2 <- c(observed.mat.drasgow.it1.it2)[expected.order.it1.it2]
    expected.mat.drasgow.it1.it2.less5 <- expected.mat.drasgow.it1.it2 < 5
    df.it1.it2 <- 2^2 - 1
    
    if (sum(expected.mat.drasgow.it1.it2.less5, na.rm = TRUE) > 0) {
      pos.sum <- max(which(expected.mat.drasgow.it1.it2.less5 == 1))
      if (sum(expected.mat.drasgow.it1.it2[1:pos.sum]) < 5)
      {
        pos.sum <- pos.sum + 1
      }
      expected.mat.drasgow.it1.it2[pos.sum] <- sum(expected.mat.drasgow.it1.it2[1:pos.sum])
      expected.mat.drasgow.it1.it2 <- expected.mat.drasgow.it1.it2[pos.sum:((1 + 1)^2)]
      observed.mat.drasgow.it1.it2[pos.sum] <- sum(observed.mat.drasgow.it1.it2[1:pos.sum])
      observed.mat.drasgow.it1.it2 <- observed.mat.drasgow.it1.it2[pos.sum:((1 + 1)^2)]
      df.it1.it2 <- (1 + 1)^2 - pos.sum
    }
    chisq <- sum(((observed.mat.drasgow.it1.it2 - expected.mat.drasgow.it1.it2)^2)/expected.mat.drasgow.it1.it2,
                 na.rm = TRUE)
    chisq.df <- chisq/df.it1.it2
    chisq.adj <- max(0, 3000 * (chisq - df.it1.it2)/N.NAs.d + df.it1.it2)
    chisq.adj.df <- chisq.adj/df.it1.it2
    c(Item1 = item1, Item2 = item2, N = N.NAs.d, df = df.it1.it2,
      chisq = chisq, chisq.df = chisq.df, chisq.adj = chisq.adj,
      chisq.adj.df = chisq.adj.df)
  }))
  doublets.res <- cbind(Doublet = 1:nrow(doublets), doublets.res)
  triplets.NAs <- cbind(triplets, N.NAs.triplets)
  triplets.res <- t(apply(triplets.NAs, 1, function(vec) {
    item1 <- vec[1]
    item2 <- vec[2]
    item3 <- vec[3]
    N.NAs.t <- vec[4]
    probs.array.item1 <- probs.array[, item1, ]
    probs.array.item2 <- probs.array[, item2, ]
    probs.array.item3 <- probs.array[, item3, ]
    probs.array.item1.arr <- array(rep(probs.array.item1, 2 * 2), c(N.nodes.chi, 2, 2, 2))
    probs.array.item2.arr <- array(rep(probs.array.item2, 2 * 2), c(N.nodes.chi, 2, 2, 2))
    probs.array.item2.arr <- aperm(probs.array.item2.arr, c(1, 3, 2, 4))
    probs.array.item3.arr <- array(rep(probs.array.item3, 2 * 2), c(N.nodes.chi, 2, 2, 2))
    probs.array.item3.arr <- aperm(probs.array.item3.arr, c(1, 4, 3, 2))
    weights.arr3 <- array(rep(weights.arr, (1 + 1)^3), c(N.nodes.chi, 1 + 1, 1 + 1, 1 + 1))
    expected.mat.drasgow.it1.it2.it3 <- N.NAs.t * apply(probs.array.item1.arr *
                                                          probs.array.item2.arr * probs.array.item3.arr *
                                                          weights.arr3, 2:4, sum)
    
    observed.mat.drasgow.it1.it2.it3 <- table(factor(data[, item1], levels = 0:1),
                                              factor(data[, item2], levels = 0:1),
                                              factor(data[, item3], levels = 0:1))
    
    expected.order.it1.it2.it3 <- order(c(expected.mat.drasgow.it1.it2.it3))
    expected.mat.drasgow.it1.it2.it3 <- c(expected.mat.drasgow.it1.it2.it3)[expected.order.it1.it2.it3]
    observed.mat.drasgow.it1.it2.it3 <- c(observed.mat.drasgow.it1.it2.it3)[expected.order.it1.it2.it3]
    expected.mat.drasgow.it1.it2.it3.less5 <- expected.mat.drasgow.it1.it2.it3 < 5
    df.it1.it2.it3 <- (1 + 1)^3 - 1
    
    if (sum(expected.mat.drasgow.it1.it2.it3.less5, na.rm = TRUE) > 0) {
      pos.sum <- max(which(expected.mat.drasgow.it1.it2.it3.less5 == 1))
      if (sum(expected.mat.drasgow.it1.it2.it3[1:pos.sum]) < 5) {
        pos.sum <- pos.sum + 1
      }
      expected.mat.drasgow.it1.it2.it3[pos.sum] <- sum(expected.mat.drasgow.it1.it2.it3[1:pos.sum])
      expected.mat.drasgow.it1.it2.it3 <- expected.mat.drasgow.it1.it2.it3[pos.sum:((1 + 1)^3)]
      observed.mat.drasgow.it1.it2.it3[pos.sum] <- sum(observed.mat.drasgow.it1.it2.it3[1:pos.sum])
      observed.mat.drasgow.it1.it2.it3 <- observed.mat.drasgow.it1.it2.it3[pos.sum:((1 + 1)^3)]
      df.it1.it2.it3 <- (1 + 1)^3 - pos.sum
    }
    chisq <- sum(((observed.mat.drasgow.it1.it2.it3 - expected.mat.drasgow.it1.it2.it3)^2)/
                   expected.mat.drasgow.it1.it2.it3, na.rm = TRUE)
    chisq.df <- chisq/df.it1.it2.it3
    chisq.adj <- max(0, 3000 * (chisq - df.it1.it2.it3)/N.NAs.t + df.it1.it2.it3)
    chisq.adj.df <- chisq.adj/df.it1.it2.it3
    c(Item1 = item1, Item2 = item2, Item3 = item3, N = N.NAs.t,
      df = df.it1.it2.it3, chisq = chisq, chisq.df = chisq.df,
      chisq.adj = chisq.adj, chisq.adj.df = chisq.adj.df)
  }))
  triplets.res <- cbind(Triplet = 1:nrow(triplets), triplets.res)
  f.int <- function(x) {
    if (x < 1)
      1
    else (if (x < 2)
      2
      else (if (x < 3)
        3
        else (if (x < 4)
          4
          else (if (x < 5)
            5
            else (if (x < 7)
              6
              else 7)))))
  }
  singlets.table <- c(table(factor(sapply(singlets.res[, 7], f.int), levels = 1:7)),
                      round(mean(singlets.res[, 7]), 4), round(sd(singlets.res[, 7]), 4))
  doublets.table <- c(table(factor(sapply(doublets.res[, 9], f.int), levels = 1:7)),
                      round(mean(doublets.res[, 9]), 4), round(sd(doublets.res[, 9]), 4))
  triplets.table <- c(table(factor(sapply(triplets.res[, 10], f.int), levels = 1:7)),
                      round(mean(triplets.res[, 10]), 4), round(sd(triplets.res[, 10]), 4))
  all.table <- rbind(singlets.table, doublets.table, triplets.table)
  rownames(all.table) <- c("Singlets", "Doublets", "Triplets")
  colnames(all.table) <- c("Less_1", "1_to_2", "2_to_3", "3_to_4",
                           "4_to_5", "5_to_7", "Larger_7", "Mean", "SD")
  res <- list(Singlets = round(singlets.res, precision),
              Doublets = round(doublets.res, precision),
              Triplets = round(triplets.res, precision),
              Summary.table = round(all.table, precision))
  class(res) <- "MODFIT"
  return(res)
}

plotMonotonicity <- function (x, item, ci = TRUE, alpha = 0.05, 
                              color = "black", transparancy = 20, ask = TRUE, ...) 
{
  
  up.lo.bound.mean <- function(n, alpha = 0.05) {
    n[n < 1e-10] <- 1e-10
    n <- matrix(n)
    p <- length(n)
    scores <- rep(c(0:(p - 1)), n)
    m <- mean(scores)
    ase <- sd(scores)/sqrt(sum(n))
    z <- qnorm(1 - alpha/2)
    matrix(c(m - z * ase, m + z * ase), 1, 2)
  }
  def.par <- par(no.readonly = TRUE)
  results <- x$results
  m <- x$m
  all.items <- 1:length(x$I.labels)
  if (ask == TRUE){
    par(ask = TRUE)}else{
      par(ask = FALSE)}
  
  c1 <- as.numeric(col2rgb(color))
  colorCi = rgb(c1[1], c1[2], c1[3], transparancy, maxColorValue = 255)
  j <- item
  plot.matrix <- results[[j]][[2]]
  x.labels <- paste(plot.matrix[, 2], "-", plot.matrix[, 
                                                       3], sep = "")
  est <- t(plot.matrix[, (m + 4 + 2):(m + 4 + m)])
  if (ci) {
    n = plot.matrix[, 4]
    n[n < 1e-10] = 1e-10
    if (m > 2) 
      se = t(apply(est, 1, function(x) sqrt((x - 
                                               x^2)/n)))
    if (m == 2) 
      se = sqrt((est - est^2)/n)
    lo = (est - qnorm(1 - alpha/2) * se)
    lo[lo < 0] <- 0
    up = (est + qnorm(1 - alpha/2) * se)
    up[up > 1] <- 1
  }
  
  plot_ly() %>%
    add_trace(x = c((1:length(up))[!is.na(up)], rev((1:length(lo))[!is.na(lo)])), 
              y=c(up[!is.na(up)], rev(lo[!is.na(lo)])), 
              fill="tozerox", 
              fillcolor="rgba(205, 201, 201, 0.4)", 
              line=list(
                color = "transparent", 
                dash = "solid", 
                width = 1.88976377953
              ), 
              mode="lines", 
              showlegend=FALSE, 
              type="scatter", 
              xaxis="x", 
              yaxis="y")%>%
    add_trace(x = plot.matrix[, 1], 
              y = as.vector(est),
              line = list(
                color = "rgba(0,0,0,1)", 
                width = 1.88976377953
              ), 
              mode="lines", 
              showlegend=FALSE,
              type="scatter", 
              xaxis="x", 
              yaxis="y")%>%
    layout(xaxis = list(title = "Rest score group", type = 'categorical', 
                        nticks = nrow(plot.matrix), 
                        tickvals = c(1: nrow(plot.matrix)),
                        ticktext = x.labels,
                        range = c(1, nrow(plot.matrix))),
           yaxis = list(title = "Item step response function", range = c(0, 1)),
           title = paste(results[[j]][[1]]))
}

plotDiff <- function(data, caseRow){
  require(colorspace)
  m <- apply(data, 2, mean)
  data2 <- data[,order(m, decreasing = TRUE)]
  v.expected <- c(rep(1, sum(data2[caseRow,])), 
                  rep(0, ncol(data2) - sum(data2[caseRow,])))
  tertiles <- quantile(x = c(1:ncol(data)), probs = c(1/3, 2/3))
  xgroups <- c(round(tertiles[1]), round(tertiles[2]))
  xgroups <- xgroups + 0.5
  
  m <- m[order(m, decreasing = TRUE)]
  prop <- data.frame(m = m, items = names(m), correct = as.integer(data2[caseRow,]))
  prop$items <- factor(prop$items, levels = prop[["items"]])

  plot_ly() %>%
    add_lines(x = prop$items, y = prop$m, 
              color = I("black"), showlegend = FALSE)%>%
    add_trace(x = prop$items, y = prop$m, type = "scatter", mode = "markers",
              marker = list(color = factor(prop$correct, labels = c("red", "green"))), 
              inherit = FALSE, showlegend = FALSE)%>%
    layout(xaxis = list(title = "", type = 'categorical', nticks = nrow(prop),
                        tickvals = rownames(prop), ticktext = rownames(prop),
                        tickfont = list(size = 7)), 
           yaxis = list(title = "Proportion of correct answers"),
           shapes = list(
             list(type = "rect", layer = 'below',
                  fillcolor = "yellowgreen", line = list(color = "yellowgreen"), opacity = 0.2,
                  x0 = 0, x1 = xgroups[1], xref = "x",
                  y0 = 0, y1 = 1, yref = "y"),
             list(type = "rect", layer = 'below',
                  fillcolor = "gold", line = list(color = "gold"), opacity = 0.2,
                  x0 = xgroups[1], x1 = xgroups[2], xref = "x",
                  y0 = 0, y1 = 1, yref = "y"),
             list(type = "rect", layer = 'below',
                  fillcolor = "red", line = list(color = "red"), opacity = 0.2,
                  x0 = xgroups[2], x1 = length(m), xref = "x",
                  y0 = 0, y1 = 1, yref = "y"),
             list(type = "line", 
                  line = list(color = "gray"), opacity = 1,
                  x0 = sum(data2[caseRow,]) + 0.5, x1 = sum(data2[caseRow,]) + 0.5, xref = "x",
                  y0 = 0, y1 = 1, yref = "y")))
}

sum_tertiles <- function(vector, frequence = "absolute"){
  tertiles <- quantile(c(1:length(vector)), probs = c(1/3, 2/3))
  xgroups <- c(0, round(tertiles[1]), round(tertiles[2]), length(vector))
  xgroups <- sort(xgroups)
  nd <- as.data.frame(vector)
  colnames(nd) <- "answ"
  nd$index <- c(1:length(vector))
  Dat <- split(nd, cut(nd$index, unique(xgroups), include.lowest=TRUE))
  if(frequence == "absolute"){
    res <- lapply(Dat, function(x) sum(x$answ))
  }
  if(frequence == "relative"){
    res <- lapply(Dat, function(x) sum(x$answ)/length(x$answ))
  }
  return(unlist(res))
}

plotProfile <- function(data, caseRow, main){
  require(colorspace)
  m <- apply(data, 2, sum)
  data <- data[,  order(m, decreasing = TRUE)]
  correct <- sum_tertiles(as.numeric(data[caseRow,]), frequence = "relative")
  
  plot_ly(x = c(1:3), y = correct, type = "scatter", mode = "markers + lines",
              color = I("black"), showlegend = FALSE)%>%
    layout(title = main,
           xaxis = list(title = "", range = c(0.5,3.5)), 
           yaxis = list(title = "Proportion of correct answers"),
           shapes = list(
             list(type = "rect", layer = 'below',
                  fillcolor = "yellowgreen", line = list(color = "yellowgreen"), opacity = 0.2,
                  x0 = 0.5, x1 = 1.5, xref = "x",
                  y0 = 0, y1 = 1, yref = "y"),
             list(type = "rect", layer = 'below',
                  fillcolor = "gold", line = list(color = "gold"), opacity = 0.2,
                  x0 = 1.5, x1 = 2.5, xref = "x",
                  y0 = 0, y1 = 1, yref = "y"),
             list(type = "rect", layer = 'below',
                  fillcolor = "red", line = list(color = "red"), opacity = 0.2,
                  x0 = 2.5, x1 = 3.5, xref = "x",
                  y0 = 0, y1 = 1, yref = "y")))
}

patterns <- function(data, caseRow, responseOptions) {
  data_aux <- data
  m <- apply(data_aux, 2, mean)
  data_aux <- data_aux[, order(m, decreasing = TRUE)]
  data_aux2 <- data_aux
  ncorrect <- sum(data_aux[caseRow, ])
  nitems <- ncol(data_aux)
  p <- 1/responseOptions
  
  # Observed:
  data_aux[caseRow,]
  
  # Normal:
  simNormal <- c(rep(1, times = ncorrect), rep(0, times = nitems - ncorrect))
  
  # Cheater:
  Nimputation <- round(0.18 * nitems)
  simCheater <- if(Nimputation < ncorrect){
    c(
      rep(1, times = ncorrect - Nimputation),
      rep(0, times = nitems - Nimputation - (ncorrect - Nimputation)),
      rep(1, times = Nimputation)
    )}else{if(ncorrect == 0){NA}else{
      as.numeric(sort(data_aux[caseRow,], decreasing = FALSE))
    }}
  sum(simCheater) 
  
  # Creative respondant:
  simCreative <- if(nitems - ncorrect > Nimputation){
    c(rep(0, times= Nimputation), c(rep(1, times = ncorrect)),
      c(rep(0, times = nitems - Nimputation - ncorrect)))}else{if(ncorrect == nitems){
        NA
      }else{
        as.numeric(sort(data_aux[caseRow,]))
      }}
  simCreative
  
  # Lucky guesser:
  Nimputation <- round(0.4 * nitems)
  simLucky <- if(Nimputation*p + nitems -  Nimputation >= ncorrect & Nimputation*p < ncorrect){
    SL = 0
    while(sum(SL) != ncorrect){
      SL <- c(rep(1, times = ncorrect - Nimputation*p),
              rep(0, times = nitems - Nimputation - (ncorrect - Nimputation*p)),
              rbinom(n = Nimputation, size = 1, prob = p))
      SL
    }
    SL
  }else{if(ncorrect == 0){NA
  }else{if(ncorrect == nitems){NA
  }else{if(Nimputation*p > ncorrect){
    SL = 0
    Nimputation = ncorrect/p
    while (sum(SL) != ncorrect) {
      SL <-
        c(rep(0, times = nitems - Nimputation),
          rbinom(
            n = Nimputation,
            size = 1,
            prob = p
          ))
      SL
    }
  }else{
    SL = 0
    Nimputation <- round((ncorrect - nitems)/(p - 1))
    while (sum(SL) != ncorrect) {
      SL <-
        c(rep(1, times = nitems - Nimputation),
          rbinom(
            n = Nimputation,
            size = 1,
            prob = p
          ))
      SL
    }
    SL    
  }}}}
  sum(simLucky)
  
  # Careless:
  Nimputation <- round(0.4 * nitems)
  simCareless <- if(Nimputation*0.5 + nitems - Nimputation > ncorrect & Nimputation*0.5 < ncorrect){
    SC = 0
    while(sum(SC)!=ncorrect){    
      SC <- c(
        rbinom(n = Nimputation, size = 1, prob = 0.5),
        rep(1, times = ncorrect - Nimputation*0.5),
        rep(0, times = nitems - Nimputation - (ncorrect - Nimputation*0.5)))
    }
    SC
  }else{if(ncorrect == 0){NA
  }else{if(ncorrect == nitems){NA
  }else{if(Nimputation*0.5 >= ncorrect){
    Nimputation <- min(c(round(ncorrect/p), nitems))
    SC = 0
    while (sum(SC) != ncorrect) {
      SC <-
        c(rbinom(n = Nimputation, size = 1, prob = 0.5),
          rep(0, times = nitems - Nimputation))
    }
    SC   
  }else{
    Nimputation <- round((ncorrect - nitems)/(0.5 - 1))
    SC = 0
    while (sum(SC) != ncorrect) {
      SC <-
        c(rbinom(n = Nimputation, size = 1, prob = 0.5),
          rep(1, times = nitems - Nimputation))
    }
    SC    
  }}}}
  simCareless
  sum(simCareless)
  
  
  return(list("simNormal" = simNormal, 
              "simCheater" = simCheater, 
              "simCreative" = simCreative, 
              "simLucky" = simLucky, 
              "simCareless" = simCareless))
}

colortable <- function(htmltab, css, style="table-condensed table-bordered"){
  tmp <- str_split(htmltab, "\n")[[1]] 
  CSSid <- gsub("\\{.+", "", css)
  CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
  CSSidPaste <- gsub("#", "", CSSid)
  CSSid2 <- paste(" ", CSSid, sep = "")
  ids <- paste0("<td id='", CSSidPaste, "'")
  for (i in 1:length(CSSid)) {
    locations <- grep(CSSid[i], tmp)
    tmp[locations] <- gsub("<td", ids[i], tmp[locations])
    tmp[locations] <- gsub(CSSid2[i], "", tmp[locations], 
                           fixed = TRUE)
  }
  htmltab <- paste(tmp, collapse="\n")
  Encoding(htmltab) <- "UTF-8"
  list(
    tags$style(type="text/css", paste(css, collapse="\n")),
    tags$script(sprintf( 
      '$( "table" ).addClass( "table %s" );', style
    )),
    HTML(htmltab)
  )
}

