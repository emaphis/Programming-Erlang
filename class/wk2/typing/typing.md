* statik typig in Erlang programs

Numbers - integer(), float(), ranges N..M
Atoms,   - atoms().
Booleans - boolean().
Tuples - {T1,...,T}.
Lists -  list(T), or [T]
Strings - string().
Functions - fun(),...,fun((T1,...,Tn)-> T)

% dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl eunit common_test


