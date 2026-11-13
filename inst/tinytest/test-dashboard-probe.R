# A child accepting HTTP connections must not defeat startup.timeout by
# leaving the parent's response read blocked on R's global network timeout.
local({
  for (status in c("200 OK", "503 Unavailable", "stalled")) {
    port <- httpuv::randomPort()
    ready <- tempfile("dashboard-probe-ready-")
    child <- callr::r_bg(function(port, status, ready) {
      listener <- serverSocket(port)
      on.exit(close(listener))
      file.create(ready)
      con <- socketAccept(listener, blocking = TRUE, open = "a+b", timeout = 5)
      on.exit(close(con), add = TRUE)
      if (status == "stalled") {
        Sys.sleep(10)
      } else {
        readLines(con, n = 4L)
        writeBin(charToRaw(paste0("HTTP/1.0 ", status,
                                 "\r\nContent-Length: 0\r\n\r\n")), con)
        flush(con)
      }
    }, args = list(port, status, ready), supervise = TRUE)
    on.exit(if (child$is_alive()) child$kill(), add = TRUE)
    on.exit(unlink(ready), add = TRUE)
    started <- proc.time()[["elapsed"]]
    while (!file.exists(ready) && child$is_alive() &&
           proc.time()[["elapsed"]] - started < 5) Sys.sleep(.02)
    expect_true(file.exists(ready))
    started <- proc.time()[["elapsed"]]
    expect_identical(persephone3:::dashboard_probe(port, timeout = .2), status == "200 OK")
    elapsed <- proc.time()[["elapsed"]] - started
    expect_true(elapsed < 1.5, info = paste(status, "probe took", elapsed, "seconds"))
    if (child$is_alive()) child$kill()
    child$wait(timeout = 3000)
    expect_false(child$is_alive())
    unlink(ready)
  }
})
