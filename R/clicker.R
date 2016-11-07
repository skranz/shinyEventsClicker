clicker.client.example = function() {
  main.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  app = clickerClientApp(main.dir)
  viewApp(app)
}

clicker.client.example = function() {
  course.id = "mycourse"
  task.id = "mytask"

  yaml = 'question: What is 20*20?
sc: [100,200,400*,500]'


  yaml = 'question: 10^2 is?
sc: [100*,200,400,500]'

  qu = clickerQuiz(yaml=yaml)

  ct = list(
    course.id = course.id,
    task.id = task.id,
    ui = qu$ui,
    init.handler = clicker.quiz.handlers(qu=qu)
  )

  main.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  write.clicker.task(ct,main.dir)
  app = clickerClientApp(main.dir)
  viewApp(app)
}


clickerClientApp = function(main.dir) {
  app = eventsApp()
  glob = app$glob
  glob$main.dir = main.dir
  glob$cur.task.file = NULL

  glob$rv = reactiveValues(task.nonce=0)

  obs = observe({
    clicker.update.tasks(main.dir = main.dir,glob = glob)
  })
  set.global.observer("myobs",obs=obs)

  app$ui = bootstrapPage(
    h4("Clicker Client"),
    uiOutput("mainUI")
  )
  appInitHandler(function(...,app=getApp()) {
    app$userid = "testuser"
    glob = app$glob
    rv = glob$rv
    app$task.obs = observe({
      rv$task.nonce
      cat("task.nonce changed...")
      if (!is.null(glob$ct)) {
        restore.point("zddbfhdbfdk")

        ct = glob$ct
        ct$init.handler()
        setUI("mainUI", ct$ui)

      }
    })
  }

  )
  app
}

.glob.obs.env = new.env()

clicker.update.tasks = function(main.dir, glob, app=getApp(), millis=1000) {
  restore.point("clicker.update.tasks")
  cat(".")
  #cat("\nI am observed...", sample.int(1000,1))

  files = list.files(file.path(main.dir, "tasks"),full.names = FALSE)
  if (length(files)==0) {
    invalidateLater(millis)
    return()
  }
  # sort filest by date, newest first
  files = files[rev(order(file.mtime(file.path(main.dir,"tasks",files))))]
  file = files[1]

  if (identical(file, glob$last.task.file)) {
    invalidateLater(millis)
    return()
  }


  close.current.task(glob)
  glob$last.task.file = file
  glob$ct = import.task.file(file, glob)


  #glob$sub.con = file(glob$ct$sub.path, open="at")
  glob$rv$task.nonce = runif(1)
  invalidateLater(millis)
}

close.current.task = function(glob) {
  ct = glob[["ct"]]
  if (is.null(ct)) return()
  try(close(glob$sub.con))

}

clicker.submit = function(values, app=getApp()) {
  restore.point("clicker.submit")

  cat("\nclicker.submit")
  glob = app$glob
  userid = app$userid

  ct = glob$ct
  vals = c(list(submitTime=Sys.time(), userid=app$userid), as.list(values))

  file = glob$ct$sub.path


  # first submission
  if (ct$num.sub==0) {
    dir.create(ct$sub.dir)
    writeLines(paste0(names(vals), collapse=","),file.path(ct$sub.dir,"colnames.csv"))
  }

  sub.file = file.path(ct$sub.dir, paste0(userid,".sub"))
  write.table(as.data.frame(vals), file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)
  glob$ct$num.sub = ct$num.sub+1

  setUI("mainUI",p("Your answer is submitted."))
}

import.task.file = function(file, glob) {
  restore.point("import.task.file")
  ct = readRDS(file.path(glob$main.dir,"tasks", file))
  ct$file = file
  ct$num.sub = 0
  ct$sub.dir = file.path(glob$main.dir,"sub", ct$task.id)
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
