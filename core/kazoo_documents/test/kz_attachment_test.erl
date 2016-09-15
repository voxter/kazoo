%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_attachment_test).

-include_lib("eunit/include/eunit.hrl").

decode_plain_text_test() ->
    Data = <<"foobar">>,
    Base64Data = base64:encode(Data),
    InlineData = <<"data:text/plain;base64,", Base64Data/binary>>,

    ?assertEqual({'undefined', Data}, kz_attachment:decode_base64(Base64Data)),
    ?assertEqual({<<"text/plain">>, Data}, kz_attachment:decode_base64(InlineData)).

decode_image_png_test_() ->
    Data = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,5,0,0,0,5,8,6,0,0,0,141,
             111,38,229,0,0,0,28,73,68,65,84,8,215,99,248,255,255,63,195,127,6,32,5,195,
             32,18,132,208,49,241,130,88,205,4,0,14,245,53,203,209,142,14,31,0,0,0,0,73,
             69,78,68,174,66,96,130
           >>,
    Base64Data = base64:encode(Data),
    InlineData = <<"data:image/png;base64,", Base64Data/binary>>,

    [?_assertEqual({'undefined', Data}, kz_attachment:decode_base64(Base64Data))
    ,?_assertEqual({<<"image/png">>, Data}, kz_attachment:decode_base64(InlineData))
    ].

decode_application_pdf_test_() ->
    Data =
        <<37,80,68,70,45,49,46,51,10,37,196,229,242,229,235,167,243,160,208,196,198,10,
          52,32,48,32,111,98,106,10,60,60,32,47,76,101,110,103,116,104,32,53,32,48,32,
          82,32,47,70,105,108,116,101,114,32,47,70,108,97,116,101,68,101,99,111,100,
          101,32,62,62,10,115,116,114,101,97,109,10,120,1,69,142,177,10,194,64,16,68,
          123,191,226,149,90,120,217,61,141,155,107,21,63,32,176,224,7,28,177,16,34,
          196,203,255,147,21,11,171,97,102,30,195,44,140,44,88,198,196,82,14,61,95,6,
          52,39,203,131,156,248,76,60,120,211,221,154,82,27,66,171,65,75,42,86,196,74,
          31,129,112,252,219,239,76,150,93,157,185,122,80,34,162,120,69,245,7,134,246,
          168,224,51,157,187,18,229,147,253,58,181,245,128,191,184,123,124,25,55,9,16,
          32,44,10,101,110,100,115,116,114,101,97,109,10,101,110,100,111,98,106,10,53,
          32,48,32,111,98,106,10,49,49,57,10,101,110,100,111,98,106,10,50,32,48,32,111,
          98,106,10,60,60,32,47,84,121,112,101,32,47,80,97,103,101,32,47,80,97,114,101,
          110,116,32,51,32,48,32,82,32,47,82,101,115,111,117,114,99,101,115,32,54,32,
          48,32,82,32,47,67,111,110,116,101,110,116,115,32,52,32,48,32,82,32,47,77,101,
          100,105,97,66,111,120,32,91,48,32,48,32,54,49,50,32,55,57,50,93,10,62,62,10,
          101,110,100,111,98,106,10,54,32,48,32,111,98,106,10,60,60,32,47,80,114,111,
          99,83,101,116,32,91,32,47,80,68,70,32,47,84,101,120,116,32,93,32,47,67,111,
          108,111,114,83,112,97,99,101,32,60,60,32,47,67,115,49,32,55,32,48,32,82,32,
          62,62,32,47,70,111,110,116,32,60,60,32,47,84,84,49,32,56,32,48,32,82,10,62,
          62,32,62,62,10,101,110,100,111,98,106,10,57,32,48,32,111,98,106,10,60,60,32,
          47,76,101,110,103,116,104,32,49,48,32,48,32,82,32,47,78,32,49,32,47,65,108,
          116,101,114,110,97,116,101,32,47,68,101,118,105,99,101,71,114,97,121,32,47,
          70,105,108,116,101,114,32,47,70,108,97,116,101,68,101,99,111,100,101,32,62,
          62,10,115,116,114,101,97,109,10,120,1,133,85,93,104,28,85,20,62,187,51,59,
          121,137,67,17,109,75,43,117,240,175,33,164,97,82,173,38,22,181,219,77,186,
          187,77,216,174,155,141,54,85,170,211,217,217,238,116,39,51,227,157,217,180,9,
          125,10,130,111,90,16,196,87,69,124,18,45,136,208,106,196,228,197,190,180,84,
          168,137,20,139,32,40,180,88,65,16,250,32,10,126,103,118,178,59,27,145,204,
          112,103,190,57,247,59,231,158,243,221,123,24,162,190,101,195,247,157,180,70,
          52,231,134,34,95,201,206,30,159,61,161,245,125,71,105,82,169,159,112,25,102,
          224,103,203,229,41,198,174,231,90,252,238,189,238,173,83,138,45,55,246,113,
          172,222,185,45,191,50,53,43,48,193,90,193,240,106,129,57,71,148,210,137,148,
          97,211,23,33,81,223,235,176,143,158,13,125,198,239,3,63,208,172,86,114,192,
          159,0,171,177,47,32,61,152,183,92,75,216,166,150,23,198,130,86,22,94,221,118,
          146,185,110,53,207,49,182,188,230,156,22,231,202,215,30,140,254,160,57,61,
          137,247,32,114,62,87,51,198,25,63,5,252,161,105,76,76,3,15,1,175,251,225,225,
          74,155,147,78,183,154,51,217,182,61,61,84,23,71,102,98,251,241,70,171,192,
          120,152,40,189,180,216,168,190,12,188,13,248,162,123,170,116,44,230,175,152,
          65,238,4,240,99,176,223,109,88,69,222,15,141,72,218,102,135,197,42,48,124,37,
          93,120,21,230,131,35,157,172,89,227,19,192,207,2,47,53,189,73,206,97,39,240,
          114,48,63,205,118,230,127,191,216,200,149,128,177,150,156,62,99,28,45,3,111,
          7,190,223,114,242,204,71,28,121,192,15,203,28,115,20,120,202,117,74,188,46,
          234,149,223,180,130,168,198,1,224,79,195,70,181,16,243,215,67,81,101,223,71,
          96,255,171,110,31,41,2,67,135,204,67,13,81,96,59,242,201,20,124,39,58,79,79,
          3,47,137,86,133,107,127,2,248,146,33,38,242,192,136,153,249,201,114,103,88,
          67,96,69,161,151,82,6,89,228,209,41,60,77,114,233,31,212,30,144,77,243,17,
          242,73,96,174,142,111,135,242,96,184,24,2,195,1,235,52,208,175,20,98,158,173,
          236,31,80,19,54,246,101,70,128,103,25,67,196,254,26,213,240,213,246,179,49,
          203,136,35,222,142,124,204,77,220,28,226,184,180,72,6,120,237,149,239,196,60,
          79,222,33,235,242,147,24,7,229,41,249,57,121,84,30,35,77,126,65,62,36,63,47,
          143,195,58,38,31,140,124,4,124,23,16,181,91,1,175,120,7,81,219,145,222,160,
          86,79,62,171,200,57,132,143,67,63,131,227,69,25,6,200,224,111,68,104,70,204,
          132,26,23,118,181,6,124,255,189,183,151,196,107,182,121,253,157,63,18,234,
          112,109,205,184,206,174,62,9,95,58,150,84,59,210,191,182,89,237,204,47,153,
          219,153,53,60,111,102,110,37,170,209,50,63,102,110,225,190,217,83,151,23,175,
          102,163,62,27,153,111,40,203,218,219,216,85,175,135,189,177,3,155,89,89,84,
          238,68,123,50,135,106,89,125,222,81,86,159,149,108,1,135,120,214,97,117,105,
          95,50,226,149,243,203,59,59,188,5,210,214,228,75,175,222,232,191,114,254,127,
          53,97,125,88,103,139,18,170,212,221,11,187,124,255,228,199,172,166,245,86,
          233,94,137,150,134,244,139,250,93,253,35,253,7,253,119,125,77,255,0,232,55,
          233,93,233,11,233,107,233,178,244,165,116,149,52,105,69,90,149,190,145,190,
          149,62,147,190,194,215,231,176,174,74,151,145,91,242,212,181,79,89,231,244,
          32,211,246,57,52,227,19,198,245,240,41,14,136,21,96,54,215,207,214,13,165,
          206,96,174,155,41,159,237,205,43,176,206,221,19,221,89,75,61,172,238,86,31,
          85,199,213,135,213,199,213,41,117,80,61,160,30,82,119,168,251,49,70,212,130,
          186,23,51,187,59,42,113,79,177,214,54,222,101,188,55,250,206,166,217,72,171,
          246,142,112,86,13,168,39,144,165,129,187,155,23,247,168,221,137,134,56,169,
          251,160,51,71,235,114,120,141,118,119,219,136,162,197,221,235,161,99,13,154,
          65,197,54,157,141,180,11,240,237,224,27,187,249,31,111,238,73,100,151,122,5,
          39,203,150,247,200,35,114,49,238,193,172,124,0,93,56,217,211,143,163,220,165,
          202,132,50,174,100,73,83,6,149,49,101,68,57,202,56,170,149,59,84,83,246,98,
          118,12,207,137,100,246,136,158,224,244,40,130,191,79,104,157,195,127,139,40,
          231,249,11,194,62,221,8,181,253,186,254,140,150,197,111,210,210,138,174,57,
          60,164,25,142,163,69,83,129,38,172,192,18,243,86,109,152,248,31,204,126,68,
          127,190,24,253,91,83,219,175,154,45,49,223,182,81,42,117,141,232,95,250,186,
          135,123,10,101,110,100,115,116,114,101,97,109,10,101,110,100,111,98,106,10,
          49,48,32,48,32,111,98,106,10,49,48,56,56,10,101,110,100,111,98,106,10,55,32,
          48,32,111,98,106,10,91,32,47,73,67,67,66,97,115,101,100,32,57,32,48,32,82,32,
          93,10,101,110,100,111,98,106,10,51,32,48,32,111,98,106,10,60,60,32,47,84,121,
          112,101,32,47,80,97,103,101,115,32,47,77,101,100,105,97,66,111,120,32,91,48,
          32,48,32,54,49,50,32,55,57,50,93,32,47,67,111,117,110,116,32,49,32,47,75,105,
          100,115,32,91,32,50,32,48,32,82,32,93,32,62,62,10,101,110,100,111,98,106,10,
          49,49,32,48,32,111,98,106,10,60,60,32,47,84,121,112,101,32,47,67,97,116,97,
          108,111,103,32,47,80,97,103,101,115,32,51,32,48,32,82,32,62,62,10,101,110,
          100,111,98,106,10,56,32,48,32,111,98,106,10,60,60,32,47,84,121,112,101,32,47,
          70,111,110,116,32,47,83,117,98,116,121,112,101,32,47,84,114,117,101,84,121,
          112,101,32,47,66,97,115,101,70,111,110,116,32,47,76,77,73,66,83,83,43,77,101,
          110,108,111,45,82,101,103,117,108,97,114,32,47,70,111,110,116,68,101,115,99,
          114,105,112,116,111,114,10,49,50,32,48,32,82,32,47,69,110,99,111,100,105,110,
          103,32,47,77,97,99,82,111,109,97,110,69,110,99,111,100,105,110,103,32,47,70,
          105,114,115,116,67,104,97,114,32,49,48,49,32,47,76,97,115,116,67,104,97,114,
          32,49,49,54,32,47,87,105,100,116,104,115,32,91,10,54,48,50,32,48,32,48,32,48,
          32,48,32,48,32,48,32,48,32,48,32,48,32,48,32,48,32,48,32,48,32,54,48,50,32,
          54,48,50,32,93,32,62,62,10,101,110,100,111,98,106,10,49,50,32,48,32,111,98,
          106,10,60,60,32,47,84,121,112,101,32,47,70,111,110,116,68,101,115,99,114,105,
          112,116,111,114,32,47,70,111,110,116,78,97,109,101,32,47,76,77,73,66,83,83,
          43,77,101,110,108,111,45,82,101,103,117,108,97,114,32,47,70,108,97,103,115,
          32,51,51,32,47,70,111,110,116,66,66,111,120,10,91,45,53,53,56,32,45,51,55,53,
          32,55,49,56,32,49,48,52,49,93,32,47,73,116,97,108,105,99,65,110,103,108,101,
          32,48,32,47,65,115,99,101,110,116,32,57,50,56,32,47,68,101,115,99,101,110,
          116,32,45,50,51,54,32,47,67,97,112,72,101,105,103,104,116,32,55,50,57,10,47,
          83,116,101,109,86,32,57,57,32,47,88,72,101,105,103,104,116,32,53,52,55,32,47,
          83,116,101,109,72,32,56,51,32,47,65,118,103,87,105,100,116,104,32,54,48,50,
          32,47,77,97,120,87,105,100,116,104,32,54,48,50,32,47,70,111,110,116,70,105,
          108,101,50,32,49,51,32,48,32,82,10,62,62,10,101,110,100,111,98,106,10,49,51,
          32,48,32,111,98,106,10,60,60,32,47,76,101,110,103,116,104,32,49,52,32,48,32,
          82,32,47,76,101,110,103,116,104,49,32,52,48,50,52,32,47,70,105,108,116,101,
          114,32,47,70,108,97,116,101,68,101,99,111,100,101,32,62,62,10,115,116,114,
          101,97,109,10,120,1,221,87,139,95,84,85,30,255,157,251,189,103,134,199,48,15,
          152,65,16,133,25,167,49,18,70,20,69,197,116,29,17,84,162,135,74,143,25,75,67,
          5,197,18,49,133,94,68,178,185,86,162,70,102,145,153,219,107,205,210,109,107,
          42,215,38,49,98,75,55,203,92,51,165,173,236,229,246,48,139,220,214,68,91,210,
          227,254,230,14,249,217,79,159,79,127,192,238,185,115,127,231,247,251,158,223,
          249,189,206,185,115,207,173,91,92,95,69,22,106,34,80,96,78,205,172,69,100,52,
          203,71,220,141,154,115,99,157,59,38,199,221,76,164,229,207,93,52,175,38,38,
          39,60,195,114,214,188,5,183,204,141,201,73,172,167,21,87,87,205,170,140,201,
          116,154,251,17,213,12,196,100,49,156,251,243,170,107,234,216,78,180,89,222,
          102,98,95,80,59,167,119,60,201,201,114,66,205,172,155,123,253,83,212,191,123,
          225,172,154,42,238,185,217,238,101,146,189,168,118,73,157,33,146,45,234,39,
          119,209,226,170,94,125,17,36,50,103,197,198,254,139,10,230,117,205,66,57,116,
          61,153,72,35,59,61,68,201,68,242,42,214,149,36,248,226,176,205,57,221,155,35,
          169,215,218,198,116,83,86,156,49,121,87,93,250,176,40,179,239,197,35,125,127,
          90,113,70,200,246,184,43,88,140,13,70,7,120,158,185,70,245,103,227,251,127,
          90,113,54,69,182,27,150,162,35,63,55,61,66,113,57,47,107,77,194,245,194,253,
          51,228,248,126,194,69,173,4,166,77,164,11,39,41,230,83,12,154,204,1,65,56,12,
          222,110,80,27,173,103,196,106,240,73,47,124,59,73,142,247,137,36,106,100,204,
          66,62,166,137,148,207,52,193,176,23,111,104,197,145,149,17,179,193,155,12,29,
          105,240,186,129,195,64,52,3,17,129,144,130,82,56,211,136,211,10,63,41,244,
          228,227,223,109,248,177,17,167,78,174,146,167,20,78,117,232,39,187,67,242,
          228,42,156,108,210,187,79,12,148,221,33,116,7,244,19,3,241,195,241,60,249,67,
          15,142,231,225,95,10,223,43,252,51,31,199,156,248,174,21,93,28,98,151,66,87,
          228,236,254,192,89,253,219,73,248,230,104,165,252,166,21,71,43,241,181,194,
          145,175,50,228,17,133,175,50,240,165,194,23,215,227,115,133,127,180,225,240,
          103,233,242,112,15,62,75,199,167,173,248,68,225,99,133,143,14,185,228,71,10,
          135,92,248,176,21,31,188,239,146,31,40,188,191,58,81,190,239,194,223,27,241,
          222,104,116,178,208,57,26,7,21,14,188,155,32,15,40,188,155,128,253,10,239,40,
          236,107,118,200,125,253,240,183,84,236,85,120,187,21,123,86,250,228,30,133,
          183,20,222,108,196,110,133,55,20,254,170,176,107,125,146,220,169,240,186,194,
          107,10,127,81,232,96,123,29,78,188,106,65,251,43,109,178,93,225,149,29,51,
          228,43,109,120,165,73,223,209,230,147,59,102,96,71,64,111,243,97,187,194,203,
          173,136,180,140,151,47,41,108,227,110,91,15,254,204,182,182,42,188,88,137,23,
          42,241,188,21,225,100,60,167,240,172,10,156,193,159,20,158,81,248,99,50,182,
          40,108,126,218,42,55,231,227,105,43,158,218,228,144,79,101,99,147,3,79,110,
          244,203,39,27,177,209,143,63,40,60,161,240,184,194,99,143,166,203,199,42,241,
          232,35,118,249,104,58,30,177,227,247,9,216,160,240,48,59,121,88,97,125,18,30,
          90,55,88,62,164,176,110,48,30,100,255,15,182,162,245,129,54,217,170,240,0,
          239,173,7,218,240,64,147,126,255,189,62,121,255,12,220,31,208,215,42,220,167,
          176,134,229,53,109,184,215,135,22,46,70,203,120,220,195,217,222,227,196,234,
          68,172,98,96,85,37,86,114,209,86,250,208,236,192,10,133,187,21,238,82,184,
          115,185,67,222,169,176,220,129,223,41,44,83,184,195,81,36,239,40,199,111,21,
          154,110,198,210,219,27,229,82,133,219,27,209,152,137,219,20,26,172,184,85,
          225,38,133,27,21,234,235,44,178,222,134,250,136,160,192,135,122,157,5,117,29,
          250,146,100,44,9,232,139,21,110,80,88,164,80,187,176,92,214,182,98,97,77,182,
          92,88,142,154,108,44,80,184,62,31,215,41,204,207,71,117,15,230,181,97,174,66,
          149,66,165,194,156,217,153,114,142,194,108,178,203,217,153,152,165,80,161,
          112,173,194,204,233,137,114,166,21,51,42,113,205,110,92,205,194,213,78,76,79,
          4,239,232,160,19,87,41,92,169,112,69,70,186,188,34,31,151,43,148,43,76,83,
          152,218,136,41,10,151,57,113,169,194,37,194,47,47,81,184,184,13,101,217,184,
          168,52,77,94,52,18,165,19,146,101,105,26,38,151,164,201,201,10,147,88,154,84,
          137,137,44,77,108,67,73,26,138,25,40,30,137,9,69,14,57,33,25,19,34,90,32,16,
          175,23,141,183,201,34,7,138,34,26,177,52,62,96,149,227,109,24,31,17,29,44,5,
          198,89,100,192,138,64,68,52,177,52,206,18,47,199,89,48,46,34,2,129,74,253,55,
          10,99,57,132,177,61,24,163,112,97,54,70,43,20,114,129,11,43,49,106,104,95,57,
          170,12,35,21,70,248,157,114,132,66,65,25,134,15,233,43,135,151,97,24,119,195,
          20,242,89,49,95,97,40,15,15,237,139,33,125,145,199,92,94,26,6,199,167,202,
          193,109,240,231,166,72,191,19,254,136,22,117,155,107,119,200,220,20,228,70,
          195,109,213,115,6,249,100,142,194,32,214,28,228,195,5,218,104,121,129,66,182,
          194,249,10,3,109,240,165,22,73,95,9,206,179,193,171,48,192,102,147,3,20,60,
          110,191,244,52,194,237,71,86,25,50,217,115,166,66,127,133,126,92,219,126,10,
          25,188,42,25,233,232,171,144,174,144,166,208,135,45,244,153,136,84,151,95,
          166,22,193,229,180,75,151,31,78,59,82,88,47,197,137,100,158,159,172,224,224,
          204,29,69,176,179,7,187,3,246,88,237,108,86,139,180,217,96,139,213,206,154,
          148,32,173,22,88,99,181,75,226,218,37,37,32,137,107,183,85,183,196,195,18,
          221,91,35,245,68,133,4,206,36,65,33,62,21,113,118,152,21,76,108,218,164,32,
          157,0,39,135,30,104,12,104,163,33,56,0,225,7,217,33,34,162,114,249,106,145,
          243,255,211,232,127,60,21,126,117,70,104,143,113,111,17,107,184,143,158,35,
          34,116,151,182,148,223,210,63,95,17,122,157,117,52,67,47,34,246,136,21,98,59,
          243,155,248,108,177,135,150,209,113,145,128,55,196,72,230,218,121,110,80,247,
          48,218,66,27,140,217,45,56,66,245,216,65,7,232,77,58,196,220,17,81,8,158,43,
          14,144,71,124,202,126,86,156,243,161,161,157,165,215,153,54,160,29,65,145,37,
          106,104,163,120,150,45,54,80,68,212,210,82,141,123,109,26,91,222,171,239,103,
          116,47,221,197,215,90,218,72,181,204,71,51,88,198,241,127,76,91,105,37,157,
          160,117,218,81,154,206,252,118,218,197,241,40,126,253,26,185,136,78,58,201,
          150,182,104,99,181,185,172,183,139,173,173,167,245,98,25,117,210,18,157,248,
          85,174,232,176,236,212,114,216,234,86,206,128,104,54,109,144,157,114,93,180,
          30,220,119,202,239,121,132,168,191,41,98,114,154,189,156,69,180,118,155,196,
          14,49,84,187,148,14,240,252,6,186,28,215,224,6,28,18,203,117,175,126,19,142,
          82,139,70,168,160,235,104,159,236,52,57,169,197,236,165,22,211,92,113,139,94,
          97,92,13,108,173,65,187,73,175,16,91,232,40,219,156,141,31,89,246,112,100,27,
          140,140,137,182,106,211,228,165,242,82,206,121,46,99,27,12,218,18,163,38,59,
          237,69,15,215,125,141,166,196,100,125,34,198,113,62,13,250,197,180,142,158,
          96,187,231,115,101,136,106,81,192,222,107,169,65,174,142,93,180,133,47,191,
          92,141,86,174,168,81,13,49,76,27,75,27,180,185,98,37,71,123,146,171,89,139,
          98,26,201,62,250,203,99,180,92,108,229,184,201,220,72,75,100,39,17,179,131,
          136,94,50,155,164,206,143,55,229,186,237,97,205,87,90,25,14,76,13,186,119,
          135,60,254,220,95,136,110,187,217,29,166,41,225,164,91,220,145,179,103,167,4,
          245,12,25,10,203,126,97,248,226,194,186,207,123,248,215,6,15,251,115,203,166,
          4,221,17,209,167,164,184,215,108,73,69,49,131,229,65,246,192,191,40,204,238,
          74,24,139,1,165,97,233,227,95,105,69,216,61,167,218,221,108,111,246,142,110,
          182,87,141,246,243,249,47,183,140,201,148,224,243,66,220,19,138,136,179,203,
          35,84,220,255,101,62,69,226,218,153,60,28,159,235,118,151,204,47,14,139,10,
          22,18,114,25,24,228,97,46,49,215,61,145,195,156,56,45,232,13,185,155,221,205,
          165,149,205,238,137,238,234,89,149,28,183,209,243,64,85,115,40,143,51,40,15,
          206,103,122,121,208,19,14,132,50,206,177,85,161,80,212,187,37,106,135,167,
          176,122,115,136,45,92,215,107,129,123,3,202,59,195,74,73,185,101,238,48,6,78,
          9,78,13,134,155,138,51,194,129,226,80,134,199,227,46,9,119,76,9,134,59,138,
          51,60,161,16,107,89,207,69,202,17,55,206,79,235,141,217,198,49,91,7,241,184,
          61,102,133,75,20,200,8,83,168,185,57,106,179,60,232,245,132,155,154,155,51,
          154,57,143,94,57,66,29,191,0,4,253,18,8,244,2,17,138,218,224,74,148,240,251,
          96,10,27,227,206,235,201,136,2,94,143,215,195,113,134,138,217,183,35,186,52,
          37,28,169,39,228,231,211,56,85,243,221,192,119,39,223,203,120,31,198,206,248,
          252,193,193,231,127,222,65,84,197,223,0,209,83,255,175,53,126,108,140,161,
          129,180,152,190,22,3,152,215,168,90,181,234,213,114,35,143,152,41,107,135,
          113,126,39,50,9,231,54,17,39,151,105,58,229,237,60,216,53,148,236,7,187,14,
          118,13,73,113,120,28,62,143,195,83,173,211,233,37,200,56,253,165,106,53,91,
          127,60,190,216,116,65,212,172,70,13,103,63,231,39,176,129,92,212,143,134,69,
          191,17,182,51,104,167,196,156,8,105,246,8,245,123,135,239,60,254,255,48,81,
          18,131,219,41,133,210,141,190,31,171,37,230,12,25,42,92,30,151,41,117,88,254,
          136,145,46,171,240,186,201,97,167,97,249,201,230,193,194,59,192,100,214,43,
          78,127,30,247,234,179,193,142,154,249,175,95,173,126,82,31,10,247,247,239,
          157,10,91,214,220,189,252,153,56,109,213,116,211,151,187,71,21,190,148,147,
          35,10,69,138,176,136,128,250,228,193,186,205,225,133,209,42,117,114,92,38,
          142,235,124,170,15,4,146,44,154,53,177,79,86,102,92,188,102,78,232,147,153,
          149,89,212,63,51,45,33,49,51,75,119,209,74,209,161,59,87,186,58,210,86,57,
          244,85,190,118,199,67,217,253,19,18,179,50,204,116,89,134,201,90,106,54,57,7,
          148,100,219,79,236,236,58,221,245,133,35,185,144,27,87,230,139,19,93,118,213,
          125,204,222,125,44,185,79,33,163,67,120,205,204,118,235,119,142,62,133,102,
          131,134,6,8,151,145,130,203,153,154,37,50,133,203,105,242,14,24,120,126,65,
          166,224,76,11,134,231,105,131,69,193,240,17,195,242,249,128,127,217,99,229,
          141,13,215,188,116,209,138,213,93,239,150,111,187,110,222,142,203,111,189,
          179,59,174,228,209,251,62,124,107,250,38,189,112,235,224,193,83,203,203,46,
          242,90,251,110,104,220,212,230,245,182,23,20,204,9,53,13,213,172,89,107,151,
          62,254,156,135,215,64,68,247,133,30,39,31,225,218,14,13,164,89,101,156,13,
          219,200,33,94,139,219,150,16,151,24,207,235,105,178,39,91,157,246,131,99,118,
          158,30,179,51,63,154,64,94,215,137,49,59,187,242,29,133,133,67,132,195,227,
          242,56,156,169,23,10,23,71,88,224,240,20,120,28,252,23,246,132,154,57,179,
          126,223,225,125,91,84,167,200,145,143,168,215,90,206,60,118,219,236,181,155,
          246,104,21,45,226,55,188,118,70,59,123,59,191,29,140,175,199,152,124,142,234,
          204,77,166,43,233,42,3,17,252,73,24,219,163,166,232,31,223,197,151,148,22,
          149,151,231,92,82,181,112,65,173,127,90,213,188,250,5,179,22,19,253,7,92,74,
          254,50,10,101,110,100,115,116,114,101,97,109,10,101,110,100,111,98,106,10,49,
          52,32,48,32,111,98,106,10,50,55,56,50,10,101,110,100,111,98,106,10,49,53,32,
          48,32,111,98,106,10,40,116,101,115,116,41,10,101,110,100,111,98,106,10,49,54,
          32,48,32,111,98,106,10,40,77,97,99,32,79,83,32,88,32,49,48,46,49,48,46,50,32,
          81,117,97,114,116,122,32,80,68,70,67,111,110,116,101,120,116,41,10,101,110,
          100,111,98,106,10,49,55,32,48,32,111,98,106,10,40,80,101,116,101,114,32,68,
          101,102,101,98,118,114,101,41,10,101,110,100,111,98,106,10,49,56,32,48,32,
          111,98,106,10,40,41,10,101,110,100,111,98,106,10,49,57,32,48,32,111,98,106,
          10,40,84,101,120,116,69,100,105,116,41,10,101,110,100,111,98,106,10,50,48,32,
          48,32,111,98,106,10,40,68,58,50,48,49,53,48,51,51,49,49,55,50,49,49,57,90,48,
          48,39,48,48,39,41,10,101,110,100,111,98,106,10,50,49,32,48,32,111,98,106,10,
          40,41,10,101,110,100,111,98,106,10,50,50,32,48,32,111,98,106,10,91,32,40,41,
          32,93,10,101,110,100,111,98,106,10,49,32,48,32,111,98,106,10,60,60,32,47,84,
          105,116,108,101,32,49,53,32,48,32,82,32,47,65,117,116,104,111,114,32,49,55,
          32,48,32,82,32,47,83,117,98,106,101,99,116,32,49,56,32,48,32,82,32,47,80,114,
          111,100,117,99,101,114,32,49,54,32,48,32,82,32,47,67,114,101,97,116,111,114,
          10,49,57,32,48,32,82,32,47,67,114,101,97,116,105,111,110,68,97,116,101,32,50,
          48,32,48,32,82,32,47,77,111,100,68,97,116,101,32,50,48,32,48,32,82,32,47,75,
          101,121,119,111,114,100,115,32,50,49,32,48,32,82,32,47,65,65,80,76,58,75,101,
          121,119,111,114,100,115,10,50,50,32,48,32,82,32,62,62,10,101,110,100,111,98,
          106,10,120,114,101,102,10,48,32,50,51,10,48,48,48,48,48,48,48,48,48,48,32,54,
          53,53,51,53,32,102,32,10,48,48,48,48,48,48,53,52,49,51,32,48,48,48,48,48,32,
          110,32,10,48,48,48,48,48,48,48,50,51,52,32,48,48,48,48,48,32,110,32,10,48,48,
          48,48,48,48,49,54,56,50,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,48,
          48,50,50,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,48,50,49,53,32,48,
          48,48,48,48,32,110,32,10,48,48,48,48,48,48,48,51,51,56,32,48,48,48,48,48,32,
          110,32,10,48,48,48,48,48,48,49,54,52,55,32,48,48,48,48,48,32,110,32,10,48,48,
          48,48,48,48,49,56,49,53,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,48,
          52,51,53,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,49,54,50,54,32,48,
          48,48,48,48,32,110,32,10,48,48,48,48,48,48,49,55,54,53,32,48,48,48,48,48,32,
          110,32,10,48,48,48,48,48,48,50,48,50,57,32,48,48,48,48,48,32,110,32,10,48,48,
          48,48,48,48,50,50,56,49,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,53,
          49,53,51,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,53,49,55,52,32,48,
          48,48,48,48,32,110,32,10,48,48,48,48,48,48,53,49,57,55,32,48,48,48,48,48,32,
          110,32,10,48,48,48,48,48,48,53,50,53,48,32,48,48,48,48,48,32,110,32,10,48,48,
          48,48,48,48,53,50,56,51,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,53,
          51,48,50,32,48,48,48,48,48,32,110,32,10,48,48,48,48,48,48,53,51,50,57,32,48,
          48,48,48,48,32,110,32,10,48,48,48,48,48,48,53,51,55,49,32,48,48,48,48,48,32,
          110,32,10,48,48,48,48,48,48,53,51,57,48,32,48,48,48,48,48,32,110,32,10,116,
          114,97,105,108,101,114,10,60,60,32,47,83,105,122,101,32,50,51,32,47,82,111,
          111,116,32,49,49,32,48,32,82,32,47,73,110,102,111,32,49,32,48,32,82,32,47,73,
          68,32,91,32,60,53,99,57,50,53,50,49,56,55,51,98,100,99,48,49,97,97,52,56,98,
          48,101,53,98,56,57,54,55,55,101,97,48,62,10,60,53,99,57,50,53,50,49,56,55,51,
          98,100,99,48,49,97,97,52,56,98,48,101,53,98,56,57,54,55,55,101,97,48,62,32,
          93,32,62,62,10,115,116,97,114,116,120,114,101,102,10,53,53,56,56,10,37,37,69,
          79,70,10
        >>,

    Base64Data = base64:encode(Data),
    InlineData = <<"data:application/pdf;base64,", Base64Data/binary>>,

    [?_assertEqual({'undefined', Data}, kz_attachment:decode_base64(Base64Data))
    ,?_assertEqual({<<"application/pdf">>, Data}, kz_attachment:decode_base64(InlineData))
    ].
