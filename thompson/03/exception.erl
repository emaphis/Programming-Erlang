%% exception handling

-module(exception).

-export([ try_return/1, try_wildcard/1]).


return_error(X) when X < 0 ->
    throw({'EXIT', {badarith,
                    [{exception,return_error,1},
                     {erl_val,do_apply,5},
                     {shell,exprs,6},
                     {shell,eval_exprs,6},
                     {shell,eval_loop,5}]}});
return_error(X) when X == 0 ->
    1/X;
return_error(X) when X > 0 ->
    {'EXIT', {badarith,
                    [{exception,return_error,1},
                     {erl_val,do_apply,5},
                     {shell,exprs,6},
                     {shell,eval_exprs,6},
                     {shell,eval_loop,5}]}}.

try_return(X) when is_integer(X) ->
    try return_error(X) of
        Val -> {normal, Val}
    catch
        exit:Reason -> {exit, Reason};
        throw:Throw -> {throw, Throw};
        error:Error -> {error, Error}
    end.

try_wildcard(X) when is_integer(X) ->
    try return_error(X)
    catch
        throw:Throw -> {throw, Throw};
        error:_     -> error;
        Type:Error  -> other;
        _:_         -> other
    end.
