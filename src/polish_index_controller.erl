-module(polish_index_controller).

-export([dispatch/1]).

-include("polish.hrl").

dispatch({Req, CT, _Path, _Method}) ->
  case polish_utils:is_user_logged(Req) of
    %% specify no-cache, otherwise the redirect won't work later
    false ->
      {ok, HTML} = login_dtl:render([]),
      {?OK, CT, HTML, [{"Cache-Control", "no-cache"}]};
    true  ->
      {file, {"index.html", "www/", [{"Cache-Control", "no-cache"}]}}
  end.
