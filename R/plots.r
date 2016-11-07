
quiz.results.plot = function(dat, qu,part = qu$parts[[1]]) {
  part = qu$parts[[1]]
  if (part$type=="sc") {
    return(quiz.sc.plot(dat=dat,qu=qu,part=part))
  }
}

quiz.sc.plot = function(dat, qu, part = qu$parts[[1]]) {
  restore.point("quiz.sc.plot")
  choices = unlist(part$choices)

  var = names(dat)[[3]]
  choices.barplot(values=dat[[var]], choices)
}

choices.barplot = function(values, choices=names(counts), counts=NULL,col="#ff8888", axes=FALSE,....) {
  restore.point("choices.barplot")

  if (is.null(counts)) {
    counts = rep(0, length(choices))
    names(counts) = choices
    cc = table(dat[[var]])
    counts[names(cc)] = cc
  }

  bp = barplot(counts, names=choices,col=col,axes = axes,...)
  text(x=bp, y=counts, labels=counts, pos=3, xpd=NA)
}
