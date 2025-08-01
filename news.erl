-module(news).
-export([scrape_sina_titles/0]).

-define(URL, "https://news.sina.com.cn").
-define(REGEX, "\"[^\" ]+\\.shtml\" target=\"_blank\">([^<]{22,})<").

scrape_sina_titles() ->
    % 启动 inets
    inets:start(),
    ssl:start(),

    % 发起 HTTP GET 请求
    case httpc:request(get, {?URL, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Html}} ->
            % 将二进制 body 转为字符串用于正则匹配
            % Html = binary_to_list(Body),

            % 使用 re 模块匹配所有标题
            fetch(?REGEX, Html);
        {ok, {{_, Code, _}, _, _}} ->
            io:format("HTTP 请求失败，状态码: ~p~n", [Code]);
        {error, Reason} ->
            io:format("请求出错: ~p~n", [Reason])
    end.

fetch(Reg, Html) ->
    case re:run(Html, Reg, [global, {capture, [1], binary}]) of
        {match, Matches} ->
            io:format("找到 ~p 个新闻标题:~n", [length(Matches)]),
            lists:foreach(
                fun(Title) ->
                    io:format("- ~ts~n", Title)
                end,
                Matches
            );
        nomatch ->
            io:format("未找到匹配的新闻标题。~n")
    end.