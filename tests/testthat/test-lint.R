linter_list <- list(
  no_absolute_path = lintr::absolute_path_linter,
  nonportable_path = lintr::nonportable_path_linter,
  semicolon_terminator = lintr::semicolon_terminator_linter,
  todo = lintr::todo_comment_linter,
  true_false = lintr::T_and_F_symbol_linter,
  undesirable = lintr::undesirable_operator_linter,
  concatenation = lintr::unneeded_concatenation_linter,
  snake_case = lintr::object_name_linter(styles = "snake_case"),
  infix_spaces = lintr::infix_spaces_linter,
  function_left_parentheses = lintr::function_left_parentheses_linter,
  spaces_left = lintr::spaces_left_parentheses_linter,
  spaces_inside = lintr::spaces_inside_linter,
  open_curly = lintr::open_curly_linter,
  closed_curly = lintr::closed_curly_linter,
  # line_length = lintr::line_length_linter(80),
  no_tab = lintr::no_tab_linter,
  assignment = lintr::assignment_linter,
  commas = lintr::commas_linter,
  no_commented_code = lintr::commented_code_linter
)

test_that("No lintr errors", {
  lintr::expect_lint_free(linters = linter_list)
})
