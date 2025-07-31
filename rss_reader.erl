-module(rss_reader).
-export([fetch_and_print_rss/0, fetch_and_print_rss/1]).

-include_lib("xmerl/include/xmerl.hrl").

%% 无参数版本：不过滤
fetch_and_print_rss() ->
    fetch_and_print_rss([]).

%% 有参数版本：传入要过滤的关键字列表
fetch_and_print_rss(FilterWords) when is_list(FilterWords) ->
    Url = "http://www.people.com.cn/rss/politics.xml",
    inets:start(),
    ssl:start(),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {Doc, _} = xmerl_scan:string(binary_to_list(Body)),
            Items = xmerl_xpath:string("//item", Doc),
            print_items(Items, FilterWords);
        {ok, {{_, Code, _}, _, _}} ->
            io:format("HTTP 请求失败，状态码: ~p~n", [Code]);
        {error, Reason} ->
            io:format("请求出错: ~p~n", [Reason])
    end.

%% 打印条目，跳过包含过滤词的
print_items([], _) ->
    ok;
print_items([Item | Rest], FilterWords) ->
    Title = get_text(Item, "title"),
    Link = get_text(Item, "link"),
    Description = get_text(Item, "description"),

    CleanTitle = remove_html_tags(Title),
    CleanDesc = remove_html_tags(Description),

    %% 检查是否包含任何过滤词
    TextToCheck = CleanTitle ++ " " ++ CleanDesc,
    case contains_any_word(TextToCheck, FilterWords) of
        true ->
            %% 跳过
            print_items(Rest, FilterWords);
        false ->
            %% 打印
            io:format("标题: ~ts~n", [CleanTitle]),
            io:format("链接: ~ts~n", [Link]),
            io:format("描述: ~ts~n", [CleanDesc]),
            io:format("~n"),
            print_items(Rest, FilterWords)
    end.

%% 提取文本内容
get_text(Item, Tag) ->
    case xmerl_xpath:string(Tag ++ "/text()", Item) of
        [] -> 
            "";
        TextNodes when is_list(TextNodes) ->
            lists:flatten([T#xmlText.value || T <- TextNodes, record_is_xmlText(T)])
    end.

record_is_xmlText(#xmlText{}) -> true;
record_is_xmlText(_) -> false.

%% 去除 HTML 标签
remove_html_tags(Text) ->
    re:replace(Text, "<[^>]*>", "", [global, {return, list}, unicode]).

%% 检查 Text 是否包含 FilterWords 中任意一个词
contains_any_word(_Text, []) ->
    false;
contains_any_word(Text, [Word | Rest]) ->
    case string:str(Text, Word) of
        0 -> % 未找到
            contains_any_word(Text, Rest);
        _ -> % 找到了
            true
    end.
