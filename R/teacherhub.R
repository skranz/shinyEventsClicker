
examples.teacherhub = function() {
  restore.point.options(display.restore.point = TRUE)

  tgroup.dir = "D:/libraries/shinyEventsClicker/teacherhub/tgroups/kranz"
  app = TeacherHubApp(tgroup.dir=tgroup.dir,init.userid="kranz", need.password=FALSE, need.user=FALSE)
  viewApp(app)

}

# RTutor Teacher-Hub

# A teaching group shares common docker containers and directories
#
# a container for teachers
# a quiz container
# a problem set container

# directory structure:
#
# tgroup
#   running
#   teacher
#     courses
#       slides
#       problem_sets
#       quiz server

# tgroups must be added by an admin
# teachers can be added by an admin or teacher in the tgroup
#
# login database can be shared among different tgroups


TeacherHubApp = function(tgroup.dir, login.db.dir=NULL, app.title="RTutor TeacherHub", ...) {
  restore.point("TeacherHubApp")
  app = eventsApp()

  app$ui = teacher.hub.main.ui()

  glob = app$glob

  glob$tgroup.dir = tgroup.dir
  db.arg = list(dbname=paste0(login.db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginModule(db.arg = db.arg, login.fun=teacher.hub.login, app.title=app.title,container.id = "centerUI",...)

  restore.point("TeacherHubApp.with.lop")

  appInitHandler(function(...,app=getApp()) {
    restore.point("TeachHubApp.initHandler")
    hide.jquery.pane("mainPanes","west")
    initLoginDispatch(lop)
  })
  app
}

teacher.hub.login = function(userid,app=getApp(),...) {
  restore.point("teacher.hub.login")

  tgroup.dir = app$glob$tgroup.dir
  user.dir = file.path(tgroup.dir,"teachers",userid)
  courses.dir = file.path(user.dir, "courses")
  courseids = list.dirs(courses.dir, full.names=FALSE, recursive=FALSE)

  courses = vector("list", length(courseids))
  names(courses) = courseids

  th = as.environment(nlist(
    userid,
    tgroup.dir,
    user.dir,
    courses.dir,
    courseids,
    courses
  ))


  app$th = th

  show.teacher.hub.ui(th, app)
}

teacher.hub.main.ui = function(app=getApp()) {
  restore.point("show.teacher.hub.ui")

  json.opts ="
  defaults: {
    resizable: true,
    closable: false,
    slideable: true,
    spacing_open: 5
  },
  north: {
    size: 'auto',
    resizable: false,
    closable: false,
    slideable: false,
    spacing_open: 0
  },
  east: {
    resizable: true,
    spacing_open: 0,
    spacing_closed: 0,
    size: 0
  }
  "

  panes = jqueryLayoutPanes(id="mainPanes",json.opts=json.opts,
    north = div(p("TeacherHub"),thinHR()),
    west = div(uiOutput("westUI")),
    center = div(uiOutput("centerUI"))
  )

  ui = bootstrapPage(
    contextMenuHeader(),
    fancytreeHeader(extensions=c("table","gridnav","dnd")),
    aceEditorHeader(),
    jqueryLayoutHeader(),
    panes
  )

}


show.teacher.hub.ui = function(th=app$th,app=getApp()) {
  restore.point("show.teacher.hub.ui")

  id = "thTree"
  n = length(th$courses)
  game.nodes = NULL
  if (length(n)>0) {
    courses.nodes = data_frame(key = paste0("courseNode_",th$courseids), title=th$courseids, expanded=TRUE, nodeType = "course", courseid=th$courseids)
  }
  tree.nodes = list(
    list(key = "thTreeCourses", title = "Courses", folder=TRUE, expanded=TRUE, children = courses.nodes)
  )
  tree = fancytree(id="thTree", source=tree.nodes)

  clickHandler("thTree", function(...) {
    args = list(...)
    restore.point("thTreeClick")
    nodeType = args$data$nodeType
    if (is.null(nodeType)) return(NULL)
    if (nodeType == "course") {
      th.show.course(args$data$courseid)
    }

  })
  setUI("westUI",tree)
  setUI("centerUI",HTML(""))
  show.jquery.pane("mainPanes",c("west","north"))
  #setUI("westUI",tree)
  #setUI("centerUI",HTML(""))
}

th.show.course = function(courseid, app=getApp(), th=app$th) {
  restore.point("th.show.course")

  th$courseid = courseid
  th$course.dir = file.path(th$courses.dir,courseid)

  types = c("slides","ps")
  #types = "slides"

  tree.nodes = vector("list",length(types))
  for (i in seq_along(types)) {
    type = types[i]
    els = list.dirs(file.path(th$course.dir,type),full.names = FALSE,recursive = FALSE)
    if (length(els)==0) {
      child.nodes = NULL
    } else {
      child.nodes = data_frame(key = paste0(type,"Node_",els), title=els, expanded=TRUE, nodeType = type, courseid=th$courseid, itemId=els, itemType=type)
    }

    tree.nodes[[i]] = list(key = paste0("ctree",type), title = type, folder=TRUE, expanded=TRUE, children = child.nodes)
  }

  # javascript code that specifies how tree
  # table columns are rendered
  js.render = paste0('
    if (node.data.itemType === "slides") {
      ',fancytree.table.button(2,"showSlidesBtn","node.data.itemId","Show"),'
    }
  ')

  tree = fancytree.table(id="courseTree",col.width=c("*"), num.cols=2, js.render=js.render,keyboard=FALSE,tabable=FALSE,source=tree.nodes)

  classEventHandler("showSlidesBtn",stop.propagation = TRUE, event = "click",
    function(...){
      args = list(...)
      restore.point("showSlidesButton_click")
      th.show.slides(slidesId=args$data$rowid)
    }
  )


  setUI("centerUI",tree)
}

th.show.slides = function(slidesId, app=getApp(), th=app$th) {
  restore.point("th.show.slides")
  cat("\nshow slides ", slidesId)
}
