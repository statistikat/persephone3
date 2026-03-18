message("agent")

# auto method selection", {
  res <- perAgent(AirPassengers, max_iter = 2)

  expect_inherits(res, "persephoneAgent")
  expect_true(res$selected_method %in% c("x13", "tramoseats"))
  expect_true(is.persephone(res$final_model))
  expect_true(isTRUE(res$should_adjust))
  expect_true(nrow(res$history) >= 2)
  expect_true(any(res$history$accepted))
#

# iterative improvement", {
  res <- perAgent(
    AirPassengers,
    method = "x13",
    speclist = list(transform.fun = "None"),
    max_iter = 3
  )

  initial_score <- res$history$score[res$history$iteration == 0][1]
  expect_identical(res$selected_method, "x13")
  expect_true(any(res$history$action == "enable_log"))
  expect_true(isTRUE(res$final_summary$log_transform))
  expect_true(res$final_score > initial_score)
#

# no identifiable seasonality", {
  set.seed(123)
  x <- ts(rnorm(120), start = c(2010, 1), frequency = 12)
  res <- perAgent(x, method = "x13", max_iter = 2)

  expect_false(res$pretests$has_identifiable_seasonality)
  expect_false(res$should_adjust)
  expect_true(grepl("No identifiable seasonality", res$stopped_reason))
#

# openai decider helper", {
  decider <- perAgentOpenAI()
  expect_true(is.function(decider))
  expect_true(all(c("credentials", "api_key") %in% names(formals(perAgentOpenAI))))
#
