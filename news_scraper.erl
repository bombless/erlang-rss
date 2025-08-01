-module(news_scraper).
-export([scrape_sina_titles/0, inspect_body/1]).

-define(URL, "https://news.sina.com.cn").
-define(REGEX, "\"[^\" ]+\\.shtml\" target=\"_blank\">([^<]+)<").

scrape_sina_titles() ->
    inets:start(),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),

    % 设置 httpc 自动处理 gzip
    httpc:set_options([{autoredirect, true}, {verbose, true}]),

    % 请求头中声明支持 gzip
    Headers = [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"},
               {"Accept-Encoding", "gzip, deflate"}],

    case httpc:request(get, {?URL, Headers}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, RespHeaders, Body}} ->
            io:format("响应头: ~p~n", [RespHeaders]),

            % 检查是否为 gzip 压缩
            case proplists:get_value("content-encoding", RespHeaders) of
                "gzip" ->
                    io:format("检测到 gzip 压缩，正在解压...~n"),
                    case zlib:gunzip(Body) of
                        Decompressed when is_binary(Decompressed) ->
                            parse_html(Decompressed, RespHeaders);
                        _ ->
                            io:format("gzip 解压失败~n")
                    end;
                _ ->
                    io:format("无压缩，直接解析~n"),
                    parse_html(Body, RespHeaders)
            end;
        {ok, {{_, Code, _}, Hdrs, _}} ->
            io:format("HTTP 错误: ~p~n响应头: ~p~n", [Code, Hdrs]);
        {error, Reason} ->
            io:format("请求失败: ~p~n", [Reason])
    end.

%% 解析 HTML：解码 UTF-8 并提取标题
% parse_html(Body) ->
%     try
%         Html = unicode:characters_to_list(Body, utf8),
%         case re:run(Html, ?REGEX, [global, {capture, [1], list}]) of
%             {match, Matches} ->
%                 io:format("找到 ~p 个新闻标题:~n", [length(Matches)]),
%                 lists:foreach(fun(Title) -> io:format("- ~ts~n", [Title]) end, Matches);
%             nomatch ->
%                 io:format("未找到匹配的新闻标题。~n"),
%                 inspect_body(Body)
%         end
%     catch
%         error:badarg ->
%             io:format("UTF-8 解码失败，正在逐字节检查前 100 字节...~n"),
%             inspect_body(Body)
%     end.
parse_html(Body, _RespHeaders) ->
    decode_and_extract(Body).

% decode_and_extract(Binary) ->
%     % inspect_body(Binary),
%     % try
%         % Html = unicode:characters_to_list(Binary, utf8),
%         Html = unicode:characters_to_binary(Binary, utf8),
%         % Html = list_to_binary(Html),
%         case re:run(Html, ?REGEX, [global, {capture, [1], list}]) of
%             {match, Matches} ->
%                 io:format("找到 ~p 个新闻标题:~n", [length(Matches)]),
%                 lists:foreach(fun(Title) -> io:format("- ~ts~n", [Title]) end, Matches);
%             nomatch ->
%                 io:format("未找到匹配的新闻标题。~n")
%         % end
%     % catch
%     %     error:badarg ->
%     %         io:format("UTF-8 解码失败，可能是编码问题~n")
%     end.
decode_and_extract(Binary) ->
    % 将输入确保为二进制
    Html = case erlang:is_list(Binary) of
        true -> list_to_binary(Binary);
        false -> Binary
    end,
    % 应用正则匹配
    case re:run(Html, ?REGEX, [global, {capture, [1], list}]) of
        {match, Matches} ->
            io:format("找到 ~p 个新闻标题:~n", [length(Matches)]),
            lists:foreach(
                fun(Title) ->
                    io:put_chars(list_to_binary(lists:flatten(Title)))
                    % Title = lists:flatten(Title),
                    % 打印原始二进制的16进制表示
                    % Hex = binary_to_hex(Title),
                    % io:format("- 标题 (hex): ~s ~ts ~n", [Hex, list_to_binary(lists:flatten(Title))])
                    % 可选：尝试以UTF-8转为字符串，失败则打标记
                    % case unicode:characters_to_list(Title, utf8) of
                    %     Str when is_list(Str) ->
                    %         io:format("  -> 解码为UTF-8: ~ts~n", [Str]);
                    %     {incomplete, _, _} ->
                    %         io:format("  -> UTF-8 解码不完整~n");
                    %     {error, _, _} ->
                    %         io:format("  -> UTF-8 解码失败，可能不是UTF-8编码~n")
                    % end
                end,
                Matches
            );
        nomatch ->
            io:format("未找到匹配的新闻标题。~n")
    end.

% 辅助函数：将二进制转换为16进制字符串
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- lists:flatten(Bin)]).

%% 逐字节打印前 N 字节的十六进制值，帮助诊断
% inspect_body(Body) when is_binary(Body) ->
%     io:format("响应体前 100 字节的十六进制:~n"),
%     Bytes = binary_to_list(Body),
%     inspect_bytes(lists:sublist(Bytes, 100), 0).
%% 辅助函数：跳过 ASCII 文本字符（包括空格、制表符、换行、可见字符）
%% 常见文本字符范围：$\s (32), $\t (9), $\n (10), $\r (13), 以及 32-126 的可打印 ASCII
is_ascii_text_char($\s) -> true;
is_ascii_text_char($\t) -> true;
is_ascii_text_char($\n) -> true;
is_ascii_text_char($\r) -> true;
is_ascii_text_char(C) when C >= 32, C =< 126 -> true;
is_ascii_text_char(_) -> false.
%% 跳过所有前导的 ASCII 文本字符
drop_ascii_prefix([H|T]) ->
    case is_ascii_text_char(H) of
        true -> drop_ascii_prefix(T);
        false -> [H|T]
    end;
drop_ascii_prefix([]) -> [].
% %% 忽略开头的 ASCII 文本字符，从第一个非文本字节开始打印十六进制
inspect_body(Body) when is_binary(Body) ->
    io:format("响应体前 100 字节的十六进制（跳过开头 ASCII 文本）:~n"),
    Bytes = binary_to_list(Body),
    case drop_ascii_prefix(Bytes) of
        [] ->
            io:format("  (响应体全是 ASCII 文本，未发现二进制数据)~n");
        BinaryStart ->
            Truncated = lists:sublist(BinaryStart, 99),
            if is_list(Truncated) -> io:format("列表哦");
            true -> io:put_chars("不是列表哦 ~n")
            end,
            io:put_chars(list_to_binary(Truncated)),
            inspect_bytes(Truncated, 0)
    end.

inspect_bytes([], _Index) ->
    ok;
inspect_bytes([Byte | Rest], Index) ->
    % 打印字节位置和十六进制
    io:format("~3w: 0x~2.16.0B ", [Index, Byte]),
    % 每 16 字节换行
    case Index rem 16 of
        15 -> io:format("~n");
        _  -> ok
    end,
    inspect_bytes(Rest, Index + 1).