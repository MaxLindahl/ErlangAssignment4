-module(allocator).

-export([start/1, request/2, release/2]).

start(NamedResources) ->
  spawn_link(fun () ->
    allocator(NamedResources, [])
             end).

request(Pid, ResourceList) ->
  Ref = make_ref(),
  Pid ! {request, {self(), Ref, ResourceList}},
  receive
    {granted, Ref, Resources} ->
      Resources
  end.

release(Pid, R) ->
  Ref = make_ref(),
  Pid ! {release, {self(), Ref, R}},
  receive
    {released, Ref} ->
      ok
  end.


allocator(NamedResources, WaitingResources) ->
  io:format("a = ~p~n", [maps:get(a, NamedResources)]), %%for debugging but they are kind of nice to see the currently available resources
  io:format("b = ~p~n", [maps:get(b, NamedResources)]),
  io:format("c = ~p~n", [maps:get(c, NamedResources)]),
  case length(WaitingResources) =:= 0 of
    false ->
      [LastResource | Rest] = lists:reverse(WaitingResources),

      {Pid1, Ref1, Resources1} = LastResource,
      ResourcesAvailable1 = [maps:get(Resource, NamedResources) || Resource <- Resources1],


      case containsZeroOrLess(ResourcesAvailable1) of
        false ->                                                      %%Resources have become available!
          ResourcesToRemoveMap1 = getResourcesAsMap(Resources1, #{}),
          Keys1 = maps:keys(ResourcesToRemoveMap1),
          NewNamedResources1 = removeResourcesFromMap(NamedResources, ResourcesToRemoveMap1, Keys1),
          Pid1 ! {granted, Ref1, Resources1},
          allocator(NewNamedResources1, lists:reverse(Rest));
        true ->
          resourcesnotavailable
      end;
    true ->
      noresourceswaiting
  end,

  receive
    {request, {Pid, Ref, Resources}} ->
      case length(WaitingResources) =:= 0 of
        true ->                                                                           %%no resources waiting
          FindResult = [maps:find(N, NamedResources) || N <- Resources],
          case lists:member(error, FindResult) of
            true ->                                                                       %%one or more resources do not exist in NamedResources map
              Pid ! error,
              allocator(NamedResources, WaitingResources);
            false ->                                                                      %%all resources exist in map
              ResourcesAvailable2 = [Val =:= 0 || {ok, Val} <- FindResult],
              case lists:member(true, ResourcesAvailable2) of
                true ->                                                                   %%one or more of the resources are not available, add to wait list
                  allocator(NamedResources, [{Pid, Ref, Resources} | WaitingResources]);
                false ->                                                                            %% all resources are available
                  ResourcesToRemoveMap = getResourcesAsMap(Resources, #{}),
                  Keys = maps:keys(ResourcesToRemoveMap),
                  NewNamedResources = removeResourcesFromMap(NamedResources, ResourcesToRemoveMap, Keys),
                  Pid ! {granted, Ref, Resources},
                  allocator(NewNamedResources, WaitingResources)
              end
          end;
        false ->                                                                        %%Resources are waiting, keep these waiting too
          allocator(NamedResources, [{Pid, Ref, Resources} | WaitingResources])
      end;
    {release, {Pid, Ref, Resources}} ->
      ResourcesToAddMap = getResourcesAsMap(Resources, #{}),
      Keys = maps:keys(ResourcesToAddMap),
      NewNamedResources = addResourcesFromMap(NamedResources, ResourcesToAddMap, Keys),
      Pid ! {released, Ref},
      allocator(NewNamedResources, WaitingResources)
  end.

%%checks if any element in a list (List) is of the value 0 or less
%%returns true if one or more elements is 0 or less, false if all elements are above 0
containsZeroOrLess([]) ->
  false;
containsZeroOrLess(List) ->
  [H|T] = List,
  case H =<0 of
    true ->
      true;
    false ->
      containsZeroOrLess(T)
  end.

%%Takes the currently available resources(Map) and a map containing resources to remove(ResourcesToRemove) and a list of the keys in ResourcesToRemove (KeyList)
%%Returns a new map of available resources with the removed resources removed from it
removeResourcesFromMap(Map, _, []) ->
  Map;

removeResourcesFromMap(Map, ResourcesToRemove, KeyList) ->
  [H|T] = KeyList,
  removeResourcesFromMap(maps:put(H, maps:get(H,Map)-maps:get(H,ResourcesToRemove), Map), ResourcesToRemove, T).


%%Takes the currently available resources(Map) and a map containing returning resources(ResourcesToAdd) and a list of the keys in ResourcesToAdd (KeyList)
%%Returns a new map of available resources with the newly returned resources added to it
addResourcesFromMap(Map, _, []) ->
  Map;
addResourcesFromMap(Map, ResourcesToAdd, KeyList) ->
  [H|T] = KeyList,
  addResourcesFromMap(maps:put(H, maps:get(H,Map)+maps:get(H,ResourcesToAdd), Map), ResourcesToAdd, T).


%%turns list of resources into a map with the key being the resource and value the amount of times it occurs
%%input: list with resources, ex [a,a,b] and empty map #{}
%%output: map with key=resource name, value= amount ex with input above #{a=>2,b=>1}
getResourcesAsMap([], Map) ->
  Map;
getResourcesAsMap(Resources, Map) ->
  [H|T] = Resources,
  case maps:find(H, Map) of
    {ok, _} ->
      getResourcesAsMap(T, maps:put(H, maps:get(H, Map)+1, Map));
    error ->
      getResourcesAsMap(T, maps:put(H, 1, Map))
  end.
