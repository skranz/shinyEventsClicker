examples.click.server = function() {
  main.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  app = clickerServerApp(main.dir=main.dir)
  viewApp(app)

}


clickerServerApp = function(main.dir, template.dir=file.path(main.dir, "templates"), userid="default") {
  restore.point("clickerServerApp")

  library(shinyAce)
  app = eventsApp()

  cs = as.environment(nlist(
    main.dir, template.dir
  ))
  app$cs = cs
  cs$templ = load.clicker.quiz.templates(cs=cs)

  headerBox = tagList(

  )

  mainBox = div(
    aceEditor("quizText", value=cs$templ[[1]]$source_text,showLineNumbers = FALSE,wordWrap = TRUE,mode = "yaml",height = "10em"),
    uiOutput("msgUI"),
    HTML("<table><tr><td>"),
    smallButton("sendBtn",label="Send"),
    HTML("</td><td>"),
    smallButton("stopBtn",label="Stop in "),
    HTML("</td><td>"),
    tags$input(id = "stopSecInput",type = "text", class = "form-control", value = "2",style="width: 4em; padding-left: 5px; padding-right: 5px; padding-top: 0; padding-bottom: 0; margin-left: 2px; margin-top:0; margin-bottom: 0;"),
    HTML("</td></tr></table>")
  )

  resultBox = tagList(
    uiOutput("taskNumSubUI"),
    uiOutput("resultsUI")
  )


  app$ui =dashboardPage(
    dashboardHeader(title = "ShinyClicker"),
    dashboardSidebar(
      selectInput("selTempl", label="Template", choices = names(cs$templ))
),
    dashboardBody(
      fluidRow(
        box(mainBox),
        box(resultBox)
      )
    )
  )



  selectChangeHandler("selTempl",function(app,...) {
    cs = app$cs
    te = getInputValue("selTempl")
    if (nchar(te)==0) return()
    templ = cs$templ[[te]]
    updateAceEditor(app$session, "quizText",value=templ$source_text)
  })
  buttonHandler("sendBtn", function(app,...) {
    yaml = getInputValue("quizText")
    parse.and.send.quiz.task(yaml)
  })
  buttonHandler("stopBtn",function(cs=app$cs, app,...) {
    stop.in.sec = as.integer(getInputValue("stopSecInput"))
    restore.point("stopBtnHandler")

    if (is.na(stop.in.sec)) stop.in.sec=3
    cs$stop.time = as.integer(Sys.time()) + stop.in.sec
  })

  appInitHandler(function(...,app=getApp()) {
    cs = as.environment(as.list(app$cs))
    app$cs = cs
    init.app.presenter(userid=userid,cs=cs)

  })
  app
}


init.app.presenter = function(userid, cs = app$cs, app=getApp()) {
  restore.point("init.app.presenter")
  cs$userid = userid
  cs$user.dir = file.path(cs$main.dir, "presenters",userid)
  cs$courses.dir = file.path(cs$user.dir,"courses")
  courses = list.dirs(cs$courses.dir)

}

parse.and.send.quiz.task = function(yaml, app=getApp()) {
  restore.point("parse.and.send.quiz.task")

  cs = app$cs
  ct = try(as.environment(clickerQuiz(yaml = yaml)))
  if (is(ct,"try-error")) {
    setUI("msgUI",p(as.character(ct)))
    return()
  } else {
    setUI("msgUI",p(paste0("Task sent to users. TaskId: ", ct$task.id)))
  }
  cs$ct = ct
  write.clicker.task(ct, main.dir=app$cs$main.dir)
  start.server.task.observer(cs=cs)
}

load.clicker.quiz.templates = function(template.dir = cs$template.dir, cs=NULL) {
  restore.point("load.clicker.quiz.templates")

  dir = template.dir[[1]]
  for (dir in template.dir) {
    files = list.files(path = dir, pattern = glob2rx("*.yaml"),full.names = TRUE)
    li = lapply(files, function(file) {
      res = NULL
      try(res<-import.yaml.with.source(file=file, add.head=FALSE))
      res
    })
    li = do.call(c, li)
  }
  li
}

start.server.task.observer = function(ct=cs$ct,cs=app$cs,app=getApp()) {
  restore.point("server.task.observer")

  cs$start.time = as.integer(Sys.time())
  cs$stop.time = NULL
  cs$stopped = FALSE

  if (!is.null(cs[["task.obs"]])) {
    try(cs$task.obs$destroy())
  }

  cs$task.obs = observe({
    app=getApp()
    cs = app$cs
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
    setUI("taskNumSubUI",tagList(
      stop.tag,
      h4(paste0("Running: ", round(as.integer(Sys.time())-cs$start.time))),
      h4(paste0("Replies: ", cs$num.sub))
    ))
    if (!cs$stopped) {
      invalidateLater(1000)
    } else {
      show.task.results()
    }
  })
}

write.clicker.task = function(ct,main.dir,file = random.string(nchar = 20)) {
  restore.point("write.clicker.task")

  long.file = file.path(main.dir,"tasks",file)
  saveRDS(as.list(ct), long.file, compress=FALSE)
}

import.yaml.with.source = function(txt=readLines(file, warn=FALSE), file=NULL, source.field = "source_text", add.head=TRUE, tab.len=2) {
  restore.point("import.yaml.with.source")

  txt = sep.lines(mark_utf8(txt))
  char = substring(txt,1,1)
  start.rows = which(!char %in% c(" ","\t","#",""))

  if (length(start.rows)==0) return(list())

  pos = cbind(start.rows,c(start.rows[-1]-1,length(txt)))
  blocks = lapply(seq.int(NROW(pos)), function(i) {
    merge.lines(txt[pos[i,1]:pos[i,2]])
  })

  li = lapply(blocks,function(txt) {
    el = import.yaml(text = txt)
    if (!add.head) {
      txt = merge.lines(substring(sep.lines(txt)[-1],1+tab.len))
    }
    el[[1]][[source.field]] = txt
    el
  })
  li = do.call(c,li)
  li
}
