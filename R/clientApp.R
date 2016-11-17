
clicker.client.example = function() {
  main.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  app = clickerClientApp(main.dir)
  viewApp(app)
}

clickerClientApp = function(main.dir, default.token=cclient.default.token(), need.token=FALSE) {
  app = eventsApp()
  glob = app$glob
  glob$main.dir = main.dir
  glob$cur.task.file = NULL
  glob$mainUI = "mainUI"
  glob$token.dir = file.path(main.dir,"tokens")

  glob$default.token = cclient.fill.token(default.token)
  glob$courses = list()
  glob$imported.task.files = NULL

  if (dir.exists(glob$token.dir)) (
    stop(paste0("token.directory ", glob$token.dir, " does not exist."))
  )

  app$need.token = need.token
  app$token.key = token.key
  glob$rv = reactiveValues()

  obs = observe({
    clicker.update.tasks(main.dir = main.dir,glob = glob)
  })
  set.global.observer("myobs",obs=obs)

  app$ui = bootstrapPage(
    uiOutput("mainUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    glob = app$glob
    rv = glob$rv

    cclient.observe.html.query(app=app, session=session)

  })
  app
}

.glob.obs.env = new.env()

cclient.start.task.observer = function(tok=app$glob$default.token,app=getApp()) {
  if (nchar(tok$userid)==0) {
    tok$userid = paste0("Guest__", random.string(1,5))
  }
  app$tok = tok
  app$userid = tok$userid
  app$courseid = tok$courseid

  # destroy old observer
  if (!is.null(app[["task.obs"]]))
    app$task.obs$destroy()

  glob=app$glob
  app$task.obs = observe({
    glob$rv[[app$courseid]]
    restore.point("app.task.observer")
    clicker.update.client.task()
    cat("task.nonce changed...")
  })
}

clicker.update.client.task = function(ct = app$glob$ct.li[[app$courseid]], app=getApp()) {
  restore.point("clicker.update.client.task")

  if (is.null(ct)) return()
  if (is.function(ct$init.handler)) {
    ct$init.handler(ct)
  } else if (is.character(ct$init.handler)) {
    do.call(ct$init.handler, list(ct=ct))
  }

  ui = tagList(
    h4(app$courseid," - ", app$userid),
    ct$ui
  )
  setUI(app$glob$mainUI, ui)
}

clicker.update.tasks = function(main.dir, glob=app$glob, app=getApp(), millis=1000) {
  restore.point("clicker.update.tasks")
  cat(".")
  #cat("\nI am observed...", sample.int(1000,1))

  files = list.files(file.path(main.dir, "tasks"),full.names = FALSE)

  files = setdiff(files, glob$imported.task.files)
  if (length(files)==0) {
    invalidateLater(millis)
    return()
  }
  # sort filest by date, newest first
  files = files[order(file.mtime(file.path(main.dir,"tasks",files)))]

  for (file in files) {
    ct = read.task.file(file, glob=glob)
    courseid = first.non.null(ct[["courseid"]],"default")
    ct$courseid = courseid
    glob$ct.li[[courseid]] = ct
    # update reactive values
    glob$rv[[courseid]] = runif(1)
  }
  glob$imported.task.files = c(glob$imported.task.files, files)
  invalidateLater(millis)
}


clicker.submit = function(values, app=getApp()) {
  restore.point("clicker.submit")

  cat("\nclicker.submit")
  glob = app$glob
  userid = app$userid

  ct = glob$ct
  vals = c(list(submitTime=Sys.time(), userid=app$userid), as.list(values))

  # first submission
  if (ct$num.sub==0) {
    dir.create(ct$sub.dir)
    writeLines(paste0(names(vals), collapse=","),file.path(ct$sub.dir,"colnames.csv"))
  }

  sub.file = file.path(ct$sub.dir, paste0(userid,"_",ct$task.id,".sub"))
  write.table(as.data.frame(vals), file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)
  glob$ct$num.sub = ct$num.sub+1

  setUI("mainUI",p("Your answer is submitted."))
}

read.task.file = function(file, glob=app$glob, app=getApp()) {
  restore.point("read.task.file")
  ct = readRDS(file.path(glob$main.dir,"tasks", file))
  ct$file = file
  ct$num.sub = 0
  ct$sub.dir = file.path(glob$main.dir,"sub", ct$task.id)

  if (isTRUE(ct$type=="quiz")) {
    ct$ui = quiz.ui(ct$qu)
    ct$init.handler="clicker.quiz.handlers"
  }
  ct
}

set.global.observer = function(id, ..., obs=observe(...)) {
  li = getOption("global.shiny.observer", default=list())
  if (!is.null(li[[id]])) {
    try(li[[id]]$destroy())
  }
  li[[id]] = obs
  options(global.shiny.observer=li)
}

destroy.global.observers = function() {
  # destroy.global.observers()
  li = getOption("global.shiny.observer", default=list())
  for (obs in li) {
    try(obs$destroy())
  }
}



#' This function must be called in the initHandler of the app
cclient.observe.html.query = function(app=getApp(),session = app$session, token.timeout.sec=300) {
  session = app$session
  observe(priority = -100,x = {
    query <- parseQueryString(session$clientData$url_search)
    restore.point("parse.url.query")
    if (is.null(query$key)) {
      query$key = app$token.key
    }
    cclient.dispatch.html.query(query, token.timeout.sec=token.timeout.sec)
  })
}

cclient.dispatch.html.query = function(query, app=getApp(), token.timeout.sec=Inf) {
  restore.point("cclient.dispatch.html.query")

  glob = app$glob
  mainUI = glob$mainUI

  if (!isTRUE(app$need.token)) {
    cclient.start.task.observer()
    return()
  }
  failed.ui = cclient.failed.login.ui()

  key = query$key
  if (is.null(key)) {
    setUI(mainUI, failed.ui)
    return()
  }

  # check if token file exists
  file = paste0(glob$token.dir,"/",key,".tok")
  if (!file.exists(file)) {
    setUI(mainUI, failed.ui)
    return()
  }

  # load token
  tok = readRDS(file=file)

  now = as.numeric(Sys.time())
  if (isTRUE(now > as.numeric(tok$time)+tok$valid)) {
    html="<h2>Timout. Your login token is not active anymore. Please login again.</h2>"
    setUI(mainUI, HTML(html))
    return()
  }

  cclient.start.task.observer(tok=tok)
}


cclient.failed.login.ui = function(app=getApp()) {
  html="<h2>Login failed</h2>"
  HTML(html)
}
