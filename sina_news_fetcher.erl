-module(sina_news_fetcher).
-export([fetch_and_print/0]).

fetch_and_print() ->
    Host = "news.sina.com.cn",
    Port = 443,
    Path = "/",
    
    % Establish SSL connection (since it's HTTPS)
    case ssl:connect(Host, Port, [{active, false}, binary, {verify, verify_none}], 5000) of
        {ok, Socket} ->
            % Send HTTP GET request
            Request = build_request(Host, Path),
            case ssl:send(Socket, Request) of
                ok ->
                    % Receive response
                    case receive_all(Socket, []) of
                        {ok, Response} ->
                            ssl:close(Socket),
                            process_response(Response);
                        {error, Reason} ->
                            ssl:close(Socket),
                            io:format("Error receiving data: ~p~n", [Reason])
                    end;
                {error, Reason} ->
                    ssl:close(Socket),
                    io:format("Error sending request: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Connection error: ~p~n", [Reason])
    end.

build_request(Host, Path) ->
    % Note: We include Accept-Encoding header but we'll handle gzip decompression ourselves
    [
        "GET ", Path, " HTTP/1.1\r\n",
        "Host: ", Host, "\r\n",
        "User-Agent: Mozilla/5.0 (compatible; Erlang client)\r\n",
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n",
        "Accept-Encoding: gzip\r\n",
        "Connection: close\r\n\r\n"
    ].

receive_all(Socket, Acc) ->
    case ssl:recv(Socket, 0, 5000) of
        {ok, Data} ->
            receive_all(Socket, [Data | Acc]);
        {error, closed} ->
            {ok, list_to_binary(lists:reverse(Acc))};
        {error, Reason} ->
            {error, Reason}
    end.

process_response(Response) ->
    % Split into headers and body
    case split_response(Response) of
        {Headers, Body} ->
            case is_gzipped(Headers) of
                true ->
                    case decompress_gzip(Body) of
                        {ok, Decompressed} ->
                            io:format("~ts~n", [Decompressed]);
                        {error, Reason} ->
                            io:format("Gzip decompression error: ~p~n", [Reason])
                    end;
                false ->
                    io:format("~s~n", [Body])
            end;
        error ->
            io:format("Invalid HTTP response~n")
    end.

split_response(Response) ->
    case binary:split(Response, <<"\r\n\r\n">>) of
        [Headers, Body] -> {Headers, Body};
        _ -> error
    end.

is_gzipped(Headers) ->
    case re:run(Headers, "content-encoding: gzip", [caseless]) of
        nomatch -> false;
        _ -> true
    end.

decompress_gzip(Compressed) ->
    try
        % The compressed data might include HTTP headers, so we need to find the actual gzip start
        % Gzip magic number: 0x1f, 0x8b
        case binary:match(Compressed, <<16#1f, 16#8b>>) of
            {Pos, 2} ->
                % Extract the gzip data starting from the magic number
                <<_:Pos/binary, GzipData/binary>> = Compressed,
                % Use Erlang's zlib to decompress
                Z = zlib:open(),
                zlib:inflateInit(Z, 31),  % 31 means auto-detect gzip/zlib
                Decompressed = zlib:inflate(Z, GzipData),
                zlib:inflateEnd(Z),
                zlib:close(Z),
                {ok, iolist_to_binary(Decompressed)};
            nomatch ->
                {error, no_gzip_magic_number_found}
        end
    catch
        _:Reason -> {error, Reason}
    end.
