-module(polish_index_controller).

-export([dispatch/1]).

-include("polish.hrl").

dispatch({Req, _CT, _Path, _Method}) ->
  case polish_utils:is_user_logged(Req) of
    %% specify no-cache, otherwise the redirect won't work later
    false -> {"login.html", "www/", [{"Cache-Control", "no-cache"}]};
    true  -> {"index.html", "www/", [{"Cache-Control", "no-cache"}]}
  end.
