
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

