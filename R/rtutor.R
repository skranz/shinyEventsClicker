
# blocks specified in RTutor
shinyEventsClicker.block.types.df = function(...) {
  restore.point("shinyEventsClicker.block.types.df")

  types = c("quiz")
  n = length(types)
  bt.df = data_frame(type=types, package="shinyEventsClicker", is.widget=TRUE, parse.inner.blocks = FALSE, remove.inner.blocks= TRUE, is.parent=FALSE, is.container = TRUE, dot.level=0, arg.li = vector("list",n))

  bt.df
}


rtutor.widget.quiz = function() {
  list(
    is.task = TRUE,
    parse.fun = rt.clicker.quiz.parse,
    init.task.state = rt.clicker.quiz.init.task.state,
    init.handlers = rt.clicker.quiz.init.handlers,
    ui.fun = rt.clicker.quiz.ui,
    rmd.fun = RTutor3::rtutor.quiz.rmd
  )
}

rt.clicker.quiz.init.task.state = function(ts, task.ind=ts$task.ind, ups=NULL,opts=NULL,...) {
  ts$cs = as.environment(list(main.dir = opts$clicker.dir))
  return(ts)
}

rt.clicker.quiz.ui = function(ts, wid=ts$wid, ...) {
  restore.point("rt.clicker.quiz.ui")
  wid$ui
}

rt.clicker.quiz.init.handlers = function(wid=ts$wid,ps=get.ps(), app=getApp(),ts=NULL,opts=ps$opts,...) {
  restore.point("rt.clicker.quiz.init.handlers")

  qu = wid
  cs = ts$cs
  buttonHandler(qu$clickerBtnId,function(...) {
    rt.clicker.send(ts=ts,wid=wid,opts=opts)
  })
  buttonHandler(qu$stopBtnId,function(...) {
    stop.in.sec = as.integer(getInputValue(qu$stopInputId))
    restore.point("rt.clicker.stopBtnHandler")

    if (is.na(stop.in.sec)) stop.in.sec=3
    cs$stop.time = as.integer(Sys.time()) + stop.in.sec
  })


  #add.quiz.handlers(qu=wid, quiz.handler=rtutor.quiz.handler)
}



rt.clicker.send = function(ts=NULL, wid=ts$wid, ct=wid$ct, cs=ts$cs, opts=NULL, app=getApp()) {
  restore.point("rt.clicker.quiz.send")
  write.clicker.task(ct, main.dir=opts$clicker.dir)
  rt.clicker.start.task.observer(ts=ts,cs=cs, wid=wid, ct=ct, opts=opts)
}

rt.clicker.start.task.observer = function(ts=NULL, cs=ts$cs, wid=ts$wid,ct=wid$ct,opts=NULL, app=getApp()) {
  restore.point("rt.clicker.start.task.observer")

  cs$main.dir = opts$clicker.dir
  cs$start.time = as.integer(Sys.time())
  cs$stop.time = NULL
  cs$stopped = FALSE

  if (!is.null(cs[["task.obs"]])) {
    try(cs$task.obs$destroy())
  }

  cs$task.obs = observe({
    app=getApp()
    restore.point("task.observer")

    dir = file.path(cs$main.dir, "sub",ct$task.id)
    files = list.files(dir)
    cs$num.sub = max(0,length(files)-1)
    if (!is.null(cs$stop.time)) {
      cs$stop.in.sec = round(cs$stop.time - as.integer(Sys.time()))
      cs$stopped = cs$stop.in.sec < 0
      if (!cs$stopped) {
        stop.tag = p(style="color: #d00",paste0("Stop in ", cs$stop.in.sec, " sec."))
      } else {
        stop.tag = p("Submission has stopped.")
      }
    } else {
      stop.tag = NULL
    }
    setUI(wid$numSubUIId,tagList(
      stop.tag,
      p(paste0("Running: ", round(as.integer(Sys.time())-cs$start.time))," sec."),
      p(paste0("Replies: ", cs$num.sub))
    ))
    if (!cs$stopped) {
      invalidateLater(1000)
    } else {
      setUI(wid$numSubUIId,"")
      rt.show.task.results(ts=ts, wid=wid, cs=cs)
    }
  })
}

rt.show.task.results = function(ts, wid=ts$wid, cs=ts$cs, ...) {
  show.task.results(cs=cs, ct=wid$ct, outputId = wid$resultsUIId, postfix=wid$id)
}

rt.clicker.quiz.parse = function(inner.txt,type="quiz",name="",id=paste0("quiz_",bi),args=NULL, bdf=NULL, bi=NULL, ps=get.ps(),opts = ps$opts,...) {
  id = paste0(ps$name,"__",id)
  restore.point("rt.clicker.quiz.parse")
  whiskers =NULL
  if (isTRUE(ps$opts$use.whiskers)) {
    whiskers = ps$pre.env$.whiskers
  }

  qu = shinyQuiz(id = id,yaml = merge.lines(inner.txt), bdf = NULL,add.handler = FALSE, whiskers=whiskers, add.check.btn=FALSE)

  qu$clickerBtnId = paste0("clickerBtn_",id)
  qu$stopBtnId = paste0("stopBtn_",id)
  qu$stopInputId = paste0("stopInInput_",id)
  qu$resultsUIId = paste0("resultsUI_",id)
  qu$numSubUIId = paste0("numSubUIId_",id)

  stop.in = first.non.null(opts$clicker.stop.in, 5)
  rt.ui = qu$ui
  qu$ui = tagList(
    rt.ui,
    HTML("<table><tr><td>"),
    smallButton(qu$clickerBtnId,label="Start", extra.style="margin-bottom: 2px;"),
    HTML("</td><td>"),
    smallButton(qu$stopBtnId,label="Stop in ",extra.style="margin-bottom: 2px;"),
    HTML("</td><td>"),
    tags$input(id = qu$stopInputId,type = "text", class = "form-control", value = stop.in,style="width: 4em; padding-left: 10px; padding-right: 5px; padding-top: 0; padding-bottom: 0; margin-left: 5px; margin-top:0; margin-bottom: 0; height: 100%;"),
    HTML("</td></tr></table>"),
    uiOutput(qu$numSubUIId),
    #uiOutput(qu$resultsUIId)
    bsCollapse(open="Results",
      slimCollapsePanel("Results", uiOutput(qu$resultsUIId))
    )
  )
  qu$ct = clickerQuiz(id=id,yaml = inner.txt, whiskers=whiskers)


  qu
}
