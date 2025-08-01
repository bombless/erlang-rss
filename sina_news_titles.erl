-module(sina_news_titles).
-export([get_and_print_titles/0]).

% 注意：此代码需要 inets 和 ssl 应用程序已启动
% 在调用此函数前，请确保执行了：
% application:ensure_all_started(inets).
% application:ensure_all_started(ssl). % 如果需要HTTPS

get_and_print_titles() ->
    Url = "https://news.sina.com.cn",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            extract_and_print_titles(Body);
        {ok, {{_, StatusCode, _}, _, _}} ->
            io:format("Error: HTTP request failed with status code ~p~n", [StatusCode]);
        {error, Reason} ->
            io:format("Error: HTTP request failed with reason ~p~n", [Reason])
    end.

extract_and_print_titles(HtmlString) ->
    % 定义要匹配的正则表达式
    % \.shtml 正向查找匹配 ".shtml"
    % target="_blank"> 匹配字面量 target="_blank">
    % ([^<]*) 捕获组，匹配任意数量的非 '<' 字符（标题内容）
    % < 匹配结束的 '<'
    % 注意：Erlang的re模块使用PCRE语法，lookbehind语法可能受限，因此使用捕获组和精确匹配
    Pattern = "\\.shtml\" target=\"_blank\">([^<]{22,})<",
    Options = [global, {capture, all_but_first, binary}],
    case re:run(HtmlString, Pattern, Options) of
        {match, Matches} ->
            % Matches 是一个列表，每个元素是匹配到的捕获组列表
            % 例如: [["标题1"], ["标题2"]]
            Titles = [Title || [Title] <- Matches, Title =/= []], % 提取非空标题
            lists:foreach(fun(Title) -> io:format("~ts~n", [Title]) end, Titles);
        nomatch ->
            io:format("No news titles found matching the pattern.~n");
        {error, ErrSpec} ->
            io:format("Regex error: ~p~n", [ErrSpec])
    end.
