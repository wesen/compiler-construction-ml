
type foobar = Foobar of string
            | Blorg of string
and call_exp = { func: string;
                 args: foobar list }
with sexp
