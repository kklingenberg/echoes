/* Function composition */
let (%) = (f, g, x) => f(g(x));

/* Monadic bind for result */
let (>>=): (result('a, 't), 'a => result('b, 't)) => result('b, 't) =
  (o, f) =>
    switch (o) {
    | Ok(v) => f(v)
    | Error(_) as error => error
    };

/* Reduce errors */
let join_results: list(result('a, list('t))) => result(list('a), list('t)) =
  items =>
    List.fold_right(
      (e, acc) =>
        switch (e, acc) {
        | (Ok(elem), Ok(accum)) => Ok([elem, ...accum])
        | (Error(_) as error, Ok(_)) => error
        | (Ok(_), Error(_) as error) => error
        | (Error(e), Error(eaccum)) => Error(e @ eaccum)
        },
      items,
      Ok([]),
    );

let join2:
  (result('a, list('t)), result('b, list('t))) => result(('a, 'b), list('t)) =
  (first, second) =>
    switch (first, second) {
    | (Ok(f), Ok(s)) => Ok((f, s))
    | (Ok(_), Error(_) as error) => error
    | (Error(_) as error, Ok(_)) => error
    | (Error(ef), Error(es)) => Error(ef @ es)
    };

let join3:
  (result('a, list('t)), result('b, list('t)), result('c, list('t))) =>
  result(('a, 'b, 'c), list('t)) =
  (first, second, third) =>
    switch (first, join2(second, third)) {
    | (Ok(f), Ok((s, t))) => Ok((f, s, t))
    | (Ok(_), Error(_) as error) => error
    | (Error(_) as error, Ok(_)) => error
    | (Error(ef), Error(erest)) => Error(ef @ erest)
    };
