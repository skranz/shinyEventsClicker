
cclient.default.token = function() {
  list(courseid="default", userid="", created=Sys.time(), valid=Inf)
}

cclient.token = function(courseid, userid, valid=Inf) {
  list(courseid=courseid, userid=userid, created=Sys.time(), valid=valid)
}

cclient.fill.token = function(tok) {
  copy.into.missing.fields(tok, cclient.default.token())
}
