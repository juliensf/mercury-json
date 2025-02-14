%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018, 2025 Julien Fischer.
% See the file COPYING for license details.
%---------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This program queries GitHub's System Status API* and reads the most recent
% status rollup as a JSON object. It then serializes that JSON object into a
% Mercury value and prints it out.
%
% In C grades, this program needs to be linked against libcurl (e.g. by
% passing the flag -lcurl to the Mercury compiler).
%
% * See <https://www.githubstatus.com/api> for details.
%
%---------------------------------------------------------------------------%

:- module messages.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    http_get_request("https://www.githubstatus.com/api/v2/status.json",
        MaybeResponse, !IO),
    (
        MaybeResponse = ok(Response),

        % Attempt to convert the response string into a JSON value.
        %
        MaybeJMessage = json.maybe_from_string(Response),
        (
            MaybeJMessage = ok(JMessage),
            MaybeMessage = json.from_json(JMessage),
            (
                MaybeMessage = ok(Message),
                print_message(Message, !IO)
            ;
                MaybeMessage = error(ConvError),
                report_error(ConvError ++ "\n", !IO)
            )
        ;
            MaybeJMessage = error(Context, ErrorDesc),
            ErrorMsg = error_context_and_desc_to_string(Context, ErrorDesc),
            report_error(ErrorMsg, !IO)
        )
    ;
        MaybeResponse = error(HTTP_Error),
        report_error(HTTP_Error ++ "\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- type message
    --->    message(
                page :: page,
                status :: status
            ).

:- type page
    --->    page(
                id         :: string,
                name       :: string,
                url        :: string,
                time_zone  :: string,
                updated_at :: string
            ).

:- type status
    --->    status(
                indicator   :: string,
                description :: string
            ).

:- instance from_json(message) where [
    func(from_json/1) is message_from_json
].

:- instance from_json(page) where [
    func(from_json/1) is page_from_json
].

:- instance from_json(status) where [
    func(from_json/1) is status_from_json
].

:- func message_from_json(json.value) = maybe_error(message).

message_from_json(Value) = MaybeMessage :-
    ( if
        json.get_object(Value, Object),
        map.search(Object, "page", PageValue),
        map.search(Object, "status", StatusValue)
    then
        MaybePage = from_json(PageValue),
        MaybeStatus = from_json(StatusValue),
        (
            MaybePage = ok(Page),
            MaybeStatus = ok(Status),
            Message = message(Page, Status),
            MaybeMessage = ok(Message)
        ;
            MaybePage = ok(_),
            MaybeStatus = error(StatusError),
            MaybeMessage = error(StatusError)
        ;
            MaybePage = error(PageError),
            MaybeStatus = ok(_),
            MaybeMessage = error(PageError)
        ;
            MaybePage = error(PageError),
            MaybeStatus = error(_StatusError),
            MaybeMessage = error(PageError)
        )
    else
        MaybeMessage = error("message is not a JSON object")
    ).

:- func page_from_json(json.value) = maybe_error(page).

page_from_json(Value) = MaybePage :-
    ( if json.get_object(Value, Object) then
        Id = json.search_string(Object, "id", ""),
        Name = json.search_string(Object, "name", ""),
        URL = json.search_string(Object, "url", ""),
        TimeZone = json.search_string(Object, "time_zone", ""),
        UpdatedAt = json.search_string(Object, "updated_at", ""),
        Page = page(Id, Name, URL, TimeZone, UpdatedAt),
        MaybePage = ok(Page)
    else
        MaybePage = error("page is not a JSON object")
    ).

:- func status_from_json(json.value) = maybe_error(status).

status_from_json(Value) = MaybeStatus :-
    ( if json.get_object(Value, Object) then
        Indicator = json.search_string(Object, "indicator", ""),
        Description = json.search_string(Object, "description", ""),
        Status = status(Indicator, Description),
        MaybeStatus = ok(Status)
    else
        MaybeStatus = error("status is not a JSON object")
    ).

%---------------------------------------------------------------------------%

:- pred print_message(message::in, io::di, io::uo) is det.

print_message(Message, !IO) :-
    Message = message(Page, Status),
    io.format("       Time: %s\n", [s(Page ^ updated_at)], !IO),
    io.format("  Indicator: %s\n", [s(Status ^ indicator)], !IO),
    io.format("       Desc: %s\n", [s(Status ^ description)], !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% The code in the following sections handles the HTTP GET request.
% There are three implementations: C (using libCurl), C# and Java.

:- pragma foreign_decl("C", "
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <curl/curl.h>

    extern MR_Word do_http_get_request(MR_String);
    extern size_t write_response_callback(void *, size_t, size_t, void *);

    struct Response {
      char *data;
      size_t size;
    };
").

:- pragma foreign_decl("C#", "
    using System;
    using System.IO;
    using System.Net;
    using System.Text;
").

:- pragma foreign_decl("Java", "
    import java.io.*;
    import java.net.*;
").

%---------------------------------------------------------------------------%

:- pred http_get_request(string::in, maybe_error(string)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    http_get_request(URLStr::in, MaybeResponse::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    MaybeResponse = do_http_get_request(URLStr);
").

:- pragma foreign_code("C", "

MR_Word
do_http_get_request(MR_String uri)
{
    CURL        *curl;
    CURLcode    res = CURLE_OK;
    char        *err_msg;

    struct Response response;

    // The response is initially empty -- the callback will grow it as
    // required.
    //
    response.data = MR_GC_malloc(1);
    response.size = 0;

    curl = curl_easy_init();
    if (curl) {
        // XXX check return values;
        curl_easy_setopt(curl, CURLOPT_URL, uri);
        curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_response_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&response);
        curl_easy_setopt(curl, CURLOPT_USERAGENT, ""libcurl-agent/1.0"");
        res = curl_easy_perform(curl);
        if (res == CURLE_OK) {
            return MER_make_ok(response.data);
        } else {
            return MER_make_error(MR_make_string_const(""error during GET request""));
        }
    } else {
        return MER_make_error(MR_make_string_const(""cannot initialise libCurl""));
    }
}

size_t
write_response_callback(void *contents, size_t size, size_t nmemb, void *userp)
{
  size_t realsize = size * nmemb;
  struct Response *mem = (struct Response *)userp;

  mem->data = MR_GC_realloc(mem->data, mem->size + realsize + 1);
  if(mem->data == NULL) {
    return 0;
  }

  MR_memcpy(&(mem->data[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->data[mem->size] = 0;

  return realsize;
}

").

:- pragma foreign_proc("C#",
    http_get_request(URLStr::in, MaybeResponse::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    try {
        WebRequest request = WebRequest.Create(URLStr);
        request.Credentials = CredentialCache.DefaultCredentials;
        WebResponse response = request.GetResponse();
        Stream dataStream = response.GetResponseStream();
        StreamReader reader = new StreamReader(dataStream);
        string responseFromServer = reader.ReadToEnd();
        MaybeResponse = MER_make_ok(responseFromServer);
        reader.Close();
        response.Close();
    } catch (Exception e) {
        MaybeResponse = MER_make_error(e.Message);
    }
").

:- pragma foreign_proc("Java",
    http_get_request(URLStr::in, MaybeResponse::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    try {
        StringBuilder result = new StringBuilder();
        URL url = new URL(URLStr);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod(""GET"");
        BufferedReader rd =
            new BufferedReader(new InputStreamReader(conn.getInputStream()));
        String line;
        while ((line = rd.readLine()) != null) {
            result.append(line);
        }
        rd.close();
        MaybeResponse = MER_make_ok(result.toString());
    } catch (Exception e) {
        MaybeResponse = MER_make_error(e.getMessage());
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", make_ok(in) = out, "MER_make_ok").
:- pragma foreign_export("C#", make_ok(in) = out, "MER_make_ok").
:- pragma foreign_export("Java", make_ok(in) = out, "MER_make_ok").
:- func make_ok(string) = maybe_error(string).

make_ok(S) = ok(S).

:- pragma foreign_export("C", make_error(in) = out, "MER_make_error").
:- pragma foreign_export("C#", make_error(in) = out, "MER_make_error").
:- pragma foreign_export("Java", make_error(in) = out, "MER_make_error").
:- func make_error(string) = maybe_error(string).

make_error(E) = error(E).

%---------------------------------------------------------------------------%

:- pred report_error(string::in, io::di, io::uo) is det.

report_error(Msg, !IO) :-
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "error: %s", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%
:- end_module messages.
%---------------------------------------------------------------------------%
