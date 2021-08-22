-module(barrier).
-export([start/1, wait/2]).

start(Refs) ->
  spawn_link(fun () -> loop(Refs, []) end).


loop(Refs, PidRefs) ->
  case lists_are_the_same(Refs, PidRefs) of
    true ->
      [Pid ! {continue, Ref} || {Pid, Ref} <- PidRefs],
      loop(Refs, []);
    false ->
      receive
        {arrive, {Pid, Ref}} ->
          case lists:member(Ref, Refs) of
            true ->
              loop(Refs, [{Pid, Ref}|PidRefs]);
            false ->
              Pid ! {continue, Ref},
              loop(Refs, PidRefs)
          end
      end
  end.


wait(Barrier, Ref) ->
  Barrier ! {arrive, {self(), Ref}},
  receive
    {continue, Ref} ->
      ok
  end.

lists_are_the_same(List1, List2) ->
  lists:sort(List1) =:= lists:sort(List2).

