#' Eliminates tailcalls under special circumstances
#'
#' `tailcall_eliminate` takes a function. If that functions has a `Recall` call as
#' the last operation, it will attempt to replace the recursion by a `GOTO` statement
#' in the byte-code representation. Beware, this can have unintended problems, if
#' the calls have side-effects.
#'
#' @param fun a function
#' @return the same function, but with more efficient byte-code.
#' If the function was optimized.
#' @importFrom rcompilertools tidy_bytecode
#' @export
tailcall_eliminate <- function(fun) {
  fun_args <- formals(fun)
  compiled_body <- tidy_bytecode(body(fun))

  # do we have our special case for tail call optimization?
  is_tail_call <- ends_with_recall(compiled_body, length(fun_args))
  if (!is_tail_call) {
    return(fun)
  }

  # now we can replace the last Recall call with a goto OP
  # but before we need to bind each argument promise of Recall to
  # the respective input args of the function. For this, we need
  # to create a promise for each function argument and then run the
  # assign function together with the original Recall code
  old_const_pool_size <- length(compiled_body$constant_pool)
  old_opcode_size <- nrow(compiled_body$code)
  new_const_pool <- compiled_body$constant_pool
  for (arg in rev(names(fun_args))) {
    new_const_pool[[length(new_const_pool) + 1]] <- list(
      as.name(".Code"),
      list(12L, as.name("GETVAR.OP"), 0L, as.name("RETURN.OP")), # GETVAR.OP at position 1 of the const pool
      list(as.name(arg))
    )
    # we also need to get the original call from the Recall argument
    # that is why we store the name here in this pass and
    # use it to complete the call in the next lapply
    # don't judge
    new_const_pool[[length(new_const_pool) + 1]] <- as.name(arg)
  }

  # not very readable, but it really is just experimental software :)
  new_const_pool[[length(new_const_pool) + 1]] <- as.name("<-")
  const_pool_size <- length(new_const_pool)
  arg_seq <- if (length(fun_args) == 0) {
    integer()
  } else {
    seq(0, length(fun_args), 2)
  }
  new_ops <- do.call(`c`, lapply(arg_seq, function(i) {
    # the position of the name of the function argument in the constant pool
    name_position <- old_const_pool_size + i + 1

    # we need to find the position of the original promise of the Recall call
    arg_prom_pos <- compiled_body$code$op_code_int[old_opcode_size - 4 - i + 1]

    # we need to construct an expression that is an assignment call
    # this is later the operand to the CALL.OP (38)
    new_const_pool[[name_position + 1]] <<- bquote(`<-`(
      .(new_const_pool[[name_position + 1]]),
      .(compiled_body$constant_pool[[arg_prom_pos + 1]][[3]][[1]])
    ))
    c(
      23L, const_pool_size - 1, # GETFUN.OP
      29L, name_position - 1, # MAKEPROM.OP
      29L, arg_prom_pos, # MAKEPROM.OP
      38L, name_position, # CALL.OP
      4L # POP.OP
    )
  }))
  new_ops <- c(
    new_ops,
    c(2L, 1L, 1L) # GOTO 1, Return
  )

  # now we retain everything just before the Recall + argument promises
  # and add the new op codes after that
  idx_before_recall_and_promises <- (nrow(compiled_body$code) - 3 - 2 * length(fun_args) - 2)
  op_codes_until_recall <- compiled_body$code[1:idx_before_recall_and_promises, ]$op_code_int
  new_ops <- as.integer(c(op_codes_until_recall, new_ops))
  new_bytecode <- .Internal(mkCode(new_ops, new_const_pool))
  new_fun <- .Internal(bcClose(fun_args, new_bytecode, environment(fun)))
  new_fun
}

ends_with_recall <- function(ops, n_args) {
  n <- nrow(ops$code)
  get_fun_pos <- n - 3 - n_args * 2 - 1
  get_fun_pos > 0 &&
    ops$code$op_code[n] == "RETURN.OP" &&
    ops$code$op_code[n - 2] == "CALL.OP" &&
    ops$code$op_code[get_fun_pos] == "GETFUN.OP" &&
    ops$constant_pool[[ops$code$op_code_int[get_fun_pos + 1] + 1]] == "Recall"
}
