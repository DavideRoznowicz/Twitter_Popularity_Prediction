(function(d){function e(a,b){for(var c in b)b.hasOwnProperty(c)&&("object"===typeof b[c]&&a.hasOwnProperty(c)&&"object"===typeof a[c]?a[c]=e(a[c],b[c]):a[c]=b[c]);return a}if(!window._er_js_loaded){window._er_js_loaded=!0;window._er_config=e(d,window._overwriting_er_config);d=window._er_config;try{window.expertrec_config&&window.expertrec_config.redirect&&(d.redirect=window.expertrec_config.redirect)}catch(a){}if(d.config.conditional&&-1===location.href.indexOf("expertrec=true"))console.warn("%cConditional search requested, but condition not satisfied",
"color:red");else{var f={overlay:"ci_customSearch_overlay",separate_divs:"ci_customSearch_separate",ecom_overlay:"ci_ecomSearch_overlay",ecom_separate_divs:"ci_ecomSearch_separate",multi_separate_divs:"ci_multiSearch_separate"};(function(a){var b=document.createElement("script");b.type="text/javascript";b.src=a;a=document.getElementsByTagName("script")[0];a.parentNode.insertBefore(b,a)})((d.tsUrlPrefix||"https://cse.expertrec.com/js")+"/"+(f[d.static.template]||f.overlay)+"."+(d.tsVersion||"1611754757331")+
".min.js")}}})({"wordpress_config": {}, "siteurl": "https://www.speaklanguages.com", "features": {"images": true, "show_filters": false, "microdata": true, "advanced": false}, "attach_voice_next_to_form": true, "facetRenameList": [{"value": "Brand", "key": "brand"}, {"value": "Sub Brand", "key": "brand.name"}, {"value": "Type", "key": "DocPath"}, {"value": "File Type", "key": "type"}], "custom_suggestion_css": "\n.er_search_suggestions {\n    /* Style for suggestion container goes here */\n    border: 1px solid #ccc;\n    margin-top: 12px;\n    box-shadow: none;\n}\n\n.er_search_suggestions .er-search-result-box{\n  /* Style for each Search result suggestion goes here */\n  border-bottom: 1px solid #ccc;\n}\n\n.er_search_suggestions .er-search-result-box .suggestion-result-image {\n  /* style for Image Container goes here */\n  height: 78px;\n  /*width: 100px;*/\n}\n\n.er_search_suggestions .er-search-result-box .image{\n  /* Style for Image goes here */\n}\n/* Fix spacing around image for RTL content */\nhtml[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .image {\n    margin-left: 20px;\n    margin-right: 0;\n}\nhtml[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .text-container {\n    padding-left: 0;\n    padding-right: 10px;\n}\nhtml[dir=\"rtl\"] .er-search-result-box .image {\n    float: right;\n    clear: right;\n}\n\n.er_search_suggestions .er-search-result-box .text-container {\n  /* Style for the text section container goes here */\n  font-family: system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Ubuntu, \"Helvetica Neue\", sans-serif;\n  padding-left:10px;\n}\nhtml[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .text-container {\n  /* Style for the text section container goes here */\n  font-family: 'Noto Naskh Arabic', system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Ubuntu, \"Helvetica Neue\", sans-serif;\n}\n\n.er_search_suggestions .er-search-result-box .title {\n  /* Style for the suggestion title goes here */\n    color: #444;\n    font-size: inherit;\n    font-weight: 600;\n}\n/* Fix text alignment for RTL content */\nhtml[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .title {\n    text-align: right !important;\n}\n\n.er_search_suggestions .er-search-result-box .title .highlight {\n  /* Style for the title highlight goes here */\n}\n\n.er_search_suggestions .er-search-result-box .title .er_highlight {\n  /* Style for the title highlight goes here */\n}\n\n.er_search_suggestions .er-search-result-box .text {\n    /* Style for the suggestion snippet text goes here */\n    color: #444;\n    font-weight: 400;\n    font-size: inherit;\n    max-height: 61px;\n    line-height: 1.3;\n    height: unset;\n    margin: 0;\n}\n    html[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .text {\n    max-height: 74px;\n    text-align: right !important;\n}\n\n\n.er_search_suggestions .er-search-result-box .text .highlight {\n  /* Style for the suggestion snippet highlight text goes here */\n}\n.er_search_suggestions .er-search-result-box .text .er_highlight {\n  /* Style for the suggestion snippet highlight text goes here */\n    color: #444;\n    font-weight: 600;\n    border-bottom: none;\n}\n\n\n/* Improve style for \"No results found\" in search suggestions */\n.er_search_suggestions h3 {\n    float: left;\n}\nhtml[dir=\"rtl\"] .er_search_suggestions h3 {\n    float: right;\n}\n\n", "pre_hook_script": "\n// Your script goes here\n// This script will be executed before the search script is initialized\n\nif(window.location.hostname.indexOf(\"expertrec.com\") <0){\n    // our demo page will work\n    // other domains\n    if(\"ar.speaklanguages.com\" == window.location.hostname){\n        // arabic language\n        window._er_config_x =  { \"additional_params\" : { \"fq\": ['host:ar.speaklanguages.com'] }};\n        window._er_config.redirect.path = \"/%D8%A8%D8%AD%D8%AB\";\n        window._er_config.search_results_ui_config.properties.noResultText = \"\u0644\u0645 \u064a\u062a\u0645 \u0627\u0644\u0639\u062b\u0648\u0631 \u0639\u0644\u0649 \u0646\u062a\u0627\u0626\u062c\"\n        window._er_config.search_results_ui_config.properties.nResultText = \"\u0627\u0644\u0646\u062a\u0627\u0626\u062c\"\n        window._er_config.search_results_ui_config.properties.oneResultText = \"\u0627\u0646\u062a\u064a\u062c\u0629\"\n    }else if(\"it.speaklanguages.com\" == window.location.hostname){\n        // italian language\n        window._er_config_x =  { \"additional_params\" : { \"fq\": ['host:it.speaklanguages.com'] }};\n        window._er_config.redirect.path = \"/cerca\";\n        window._er_config.search_results_ui_config.properties.noResultText = \"Nessun risultato trovato\"\n        window._er_config.search_results_ui_config.properties.nResultText = \"risultati\"\n        window._er_config.search_results_ui_config.properties.oneResultText = \"risultato\"\n    }else if(\"vi.speaklanguages.com\" == window.location.hostname){\n        // viatnam language\n        window._er_config_x =  { \"additional_params\" : { \"fq\": ['host:vi.speaklanguages.com'] }};\n    }\n    else if(\"es.speaklanguages.com\" == window.location.hostname){\n        // spanish language\n        window._er_config_x =  { \"additional_params\" : { \"fq\": ['host:es.speaklanguages.com'] }};\n        window._er_config.redirect.path = \"/buscar\";\n        window._er_config.search_results_ui_config.properties.noResultText = \"No se han encontrado resultados\"\n        window._er_config.search_results_ui_config.properties.nResultText = \"resultados\"\n        window._er_config.search_results_ui_config.properties.oneResultText = \"resultado\"\n    }\n    else if(\"th.speaklanguages.com\" == window.location.hostname){\n        // Thai language\n        window._er_config_x =  { \"additional_params\" : { \"fq\": ['host:th.speaklanguages.com'] }};\n        window._er_config.redirect.path = \"/%E0%B8%84%E0%B9%89%E0%B8%99%E0%B8%AB%E0%B8%B2\";\n        window._er_config.search_results_ui_config.properties.noResultText = \"\u0e44\u0e21\u0e48\u0e1e\u0e1a\u0e1c\u0e25\u0e25\u0e31\u0e1e\u0e18\u0e4c\"\n        window._er_config.search_results_ui_config.properties.nResultText = \"\u0e1c\u0e25\u0e25\u0e31\u0e1e\u0e18\u0e4c\"\n        window._er_config.search_results_ui_config.properties.oneResultText =\" \"\n    }\n    else {\n        window._er_config_x =  { \n            \"additional_params\" : { \"fq\": ['host:www.speaklanguages.com'] }\n        };\n        window._er_config.redirect.path = \"/search\";\n    }\n}\n \n let ltr = document.querySelector('html')\n let langDir = ltr.getAttribute('dir')\n if(langDir === 'ltr') {\n      window._er_config.popper.placement = \"bottom-start\";\n } else {\n      window._er_config.popper.placement = \"bottom-end\";\n }\n\n\n", "append_to_response": {"page_size": 16, "suggestion_size": 5}, "no_form_add_ci_search": false, "redirect": {"location_reload": false, "path": "/search", "search_param": "q"}, "scrsht": true, "scroll_flag": true, "suggestion_fields": {"url": {"keys": [["url"], ["snippet", "resourcename", "0"], ["id"]], "type": "string"}, "text": {"keys": [["snippet", "content", "0"], ["url"]], "type": "string"}, "image": {"keys": [["image"], ["thumbnail"]], "type": "string"}, "id": {"keys": ["id"], "type": "string"}, "title": {"keys": [["snippet", "title", "0"], ["title"], ["id"]], "type": "string"}}, "dummy_search_config": {"dummy_back_button": {"mobile": {"custom": " "}}, "search_box_container": {"mobile": {"custom": " "}, "width": "auto", "padding": "0px", "margin": "0px"}, "showToolTip": true, "dummy_search_form": {"custom": " "}, "dummy_search_input_box": {"outline": "#efefef", "fontWeight": 300, "color": "#333", "border": "solid 1px #00205c", "height": "30px", "padding": "5px", "width": "auto", "fontSize": "13px", "mobile": {"custom": " "}, "margin": "0px", "borderRadius": "0px"}, "dummy_search_button": {"color": "white", "margin": "0px", "height": "30px", "padding": "0px", "width": "30px", "fontSize": "16px", "mobile": {"custom": " "}, "background": "#00205c", "custom": "display:inherit; justify-content:center; align-items:center", "border": "none", "borderRadius": "0px"}, "properties": {"back_button_icon": "<i class='fa fa-arrow-left'></i>", "toolTipMessage": "This search supports advanced operators like AND, NOT & OR", "disable_fontawesome": true, "placeholder_text": "Search", "search_button_icon": "<i class='fa fa-search'></i>", "clear_input_button": true}}, "sendAnalyticsQuery": true, "mid": "7234c44e-c2ba-11ea-87f6-0242ac130002", "theme": "square", "scrsht_img_prefix": "//lbr-searchv7-eusc.expertrec.com/img/", "config": {"autoFocusInput": false, "name": "Expertrec", "same_tab": true, "suggest": true, "conditional": false, "instant_search": true, "dashboard": {"display_background_color": "lightgrey"}, "redirectURL": "", "logo": false, "voice": true, "timeoutRedirect": 20000}, "pre_render_script": "\n/*  Your script goes here\n    This script will be executed after a user performs a search and before it is rendered\n    you will have the search data present in the variable 'data' and you can perform regular javascript operations\n    Modifying the variable 'data' will modify the value in results\n*/\n\n    // Let's say you want to track visits from your website's search\n    // for (var i in data.results) {\n    //    data.results.url += '?ref=search';\n    // }\n    \n    // Zero results tracking\n    if ((!data ||  (data && data.results.length === 0)) && typeof ga === 'function') {\n      // Put your GA code here for no results\n      ga('send', 'event', 'Search', 'ZeroResults', data['1query'] ? data['1query'] : 'Blank');\n  }\n  \n  let ltr = document.querySelector('html')\n  let langDir = ltr.getAttribute('lang')\n  let resultLength = data['0len']\n  if(resultLength === 0) {\n      document.querySelector('.er-top-head').style.display = \"none\"\n  }\n  \n  if(langDir === 'th' && data['res']['count'] === 1) {\n      data['res']['count'] = '\u0e1c\u0e25\u0e01\u0e32\u0e23\u0e04\u0e49\u0e19\u0e2b\u0e32 1 \u0e23\u0e32\u0e22\u0e01\u0e32\u0e23'\n  }\n", "search_results_ui_config": {"search_images": {"padding": "5px", "width": "115px", "height": "92px", "image_objectFit": "cover", "custom": "margin-top: 10px; margin-right: 0.8rem;"}, "search_title_highlight": {"color": "#3257ec", "background": "none", "custom": "font-weight: bold;"}, "search_url": {"color": "#057705", "padding": "2px 0", "fontWeight": "400", "fontSize": "inherit", "custom": ""}, "search_text": {"color": "#444", "border": "none", "height": "auto", "padding": "0", "fontSize": "inherit", "lineHeight": "1.5", "custom": "max-height: 136px;", "margin": "8px 0 0"}, "search_title": {"color": "#3257ec", "padding": "2px 0", "fontWeight": "normal", "fontSize": "1.3888888rem", "custom": "line-height: 1.5"}, "search_result_box_hover": {"background": "#eee", "custom": ""}, "search_result_box": {"padding": "10px", "margin": "10px", "border": "none", "background": "none", "custom": "max-width: 632px; min-height: 160px;"}, "search_text_highlight": {"color": "#444", "fontWeight": "600", "background": "none", "custom": ""}, "search_results_container": {"padding": "0", "minHeight": "800px", "custom": ""}, "properties": {"nResultText": "results", "noResultText": "No results found", "showResults": true, "showImages": true, "showUrl": true, "oneResultText": "result"}}, "AttachVoice": false, "overlay_input_config": {"search_result_input_form_hover": {"custom": ""}, "search_result_input_form": {"padding": "15px 0", "width": "80%", "margin": "auto", "display": "flex", "custom": "border-bottom: 2px solid #F2F2F2;"}, "er_search_button": {"outline": "none", "color": "#555", "margin": "0 0 0 -4px", "background": "#fdfdfd !important", "height": "50px", "padding": "0", "width": "4%", "fontSize": "30px", "boxShadow": "none", "custom": "", "border": "none"}, "search_box": {"padding": "20px 0px", "width": "100%", "background": "white", "custom": ""}, "er_clear_input_hover": {"custom": ""}, "er_search_input_hover": {"custom": ""}, "er_search_voice": {"custom": ""}, "er_search_input": {"outline": "0", "fontWeight": "lighter", "color": "black", "background": "#fdfdfd !important", "height": "50px", "padding": "0 10px", "width": "95%", "fontSize": "30px", "boxShadow": "none", "custom": "", "border": "none"}, "er_search_voice_hover": {"custom": ""}, "er_search_button_hover": {"custom": ""}, "er_clear_input": {"outline": "none", "color": "#333", "background": "#fdfdfd !important", "custom": "", "padding": "0 20px 0 0", "fontSize": "20px", "boxShadow": "none", "border": "none"}, "search_box_hover": {"custom": ""}, "config": {"voice_button_text": "<i class=\"fa fa-microphone\"></i>", "placeholder": "Search", "search_button_text": "<i class=\"fa fa-search\"></i>", "close_button_text": "<i class=\"fa fa-times\"></i>", "show_search_button": true}}, "custom_css": "\n     /* Customize everything with the css editor here */\n     #er_search_results .er-search-result-box .title a {\n        color: #04c !important;\n     }\n\n     .er-search-result-box .title a {\n         white-space: inherit !important;\n     }\n\n     #er_search_results .er-search-result-box:hover {\n        background: inherit !important;\n     }\n\n     .er_search_suggestions .er-search-result-box:hover {\n        background: #f0f0f0 !important;\n     }\n\n     .er_search_suggestions .er-search-result-box .title {\n        white-space: inherit !important;\n     }\n\n     ci-suggest *:not(style) {\n        font-size: inherit !important;\n     }\n\n\n    /* Fix text and image alignment in search results for RTL content */\n    html[dir=\"rtl\"] #er_search_results {\n        text-align: right !important;\n    }\n\n    html[dir=\"rtl\"] #er_search_results .url {\n        direction: ltr;\n    }\n\n    html[dir=\"rtl\"] .er-search-result-box .image {\n        float: right;\n        clear: right;\n    }\n\n    html[dir=\"rtl\"] #er_search_results .er-search-result-box .image {\n        margin-right: 0;\n        margin-left: 0.8rem;\n    }\n\n\n    /* Adjust styles based on screen width */\n    @media screen and (min-width: 700px) {\n        .er_search_suggestions .er-search-result-box .text {\n            max-height: 82px !important;\n        }\n        html[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .text {\n            max-height: 78px !important;\n        }\n         #er_search_results .er-search-result-box .text {\n            max-height: 148px !important;\n        }\n        /* N.B. RTL max-height is also 148px at this breakpoint so no special rule needed */\n     }\n     \n     @media screen and (min-width: 1260px) {\n        .er_search_suggestions .er-search-result-box .text {\n            max-height: 90px !important;\n        }\n        html[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .text {\n            max-height: 104px !important;\n        }\n        #er_search_results .er-search-result-box .text {\n            max-height: 154px !important;\n        }\n        html[dir=\"rtl\"] #er_search_results .er-search-result-box .text {\n            max-height: 150px !important;\n        }\n     }\n     @media screen and (min-width: 1600px) {\n        .er_search_suggestions .er-search-result-box .text {\n            max-height: 94px !important;\n        }\n        html[dir=\"rtl\"] .er_search_suggestions .er-search-result-box .text {\n            max-height: 110px !important;\n        }\n        #er_search_results .er-search-result-box .text {\n            max-height: 160px !important;\n        }\n        html[dir=\"rtl\"] #er_search_results .er-search-result-box .text {\n            max-height: 156px !important;\n        }\n    }", "popper": {"modifiers": {"flip": {"boundariesElement": "window", "behavior": ["bottom-start", "bottom", "top-start", "top-end", "top", "bottom-end"]}, "preventOverflow": {"priority": ["left", "right"], "boundariesElement": "window", "enabled": true}}, "placement": "bottom-end"}, "wordpress_client": false, "mdomain": "lbr-searchv7-eusc.expertrec.com", "hook_on_existing_input_box": true, "static": {"style": "separate", "template": "separate_divs"}, "useFacetRename": true, "endpoint": {"suggestion_url": "https://searchv7.expertrec.com/v6/search/7234c44e-c2ba-11ea-87f6-0242ac130002/", "collect_error_url": "//err.expertrec.com/v2/collect/7234c44e-c2ba-11ea-87f6-0242ac130002/", "collect_url": "//log.expertrec.com/v2/collect/7234c44e-c2ba-11ea-87f6-0242ac130002/", "search_url": "https://searchv7.expertrec.com/v6/search/7234c44e-c2ba-11ea-87f6-0242ac130002/"}, "language": {"lang": "en"}, "fields": {"url": {"keys": [["url"], ["snippet", "resourcename", "0"], ["id"]], "type": "string"}, "text": {"keys": [["snippet", "metatag.description", "0"], ["snippet", "content", "0"]], "type": "string"}, "image": {"keys": [["image"], ["thumbnail"]], "type": "string"}, "id": {"keys": ["id"], "type": "string"}, "title": {"keys": [["snippet", "title", "0"], ["title"], ["id"]], "type": "string"}}, "voice_config": {"custom_voice_style": {"top": "10px", "custom": "", "left": "-5%"}, "color": "white", "voice_button": "<i class='fa fa-microphone'></i>", "voice_overlay": {"outline": "none", "color": "#555", "custom": " ", "padding": "0px", "background": "#fdfdfd", "border": "none"}, "height": "30px", "padding": "0px", "mobile": {"custom": " "}, "background": "#00205c", "custom": "display:inherit; justify-content:center; align-items:center; width: 20px", "border": "none", "borderRadius": "0px", "voice_hover": {"mobile": {"custom": " "}, "custom": " "}}, "sortby": [{"displayLabel": "Relevance", "type": "numeric", "value": "score:desc", "key": "score"}], "element_selector": {"no_form_append_ci_tag": ["nav ul#primary-menu > li:last-child", "nav ul#menu-header > li:last-child", "nav ul:not(.sub-menu) > li:last-child", "section ul#main-menu  li:last-child", "section ul:not(.sub-menu) > li:last-child", "header", "section", "nav"], "reference_element_selector": ["form[method=get]:not(#adminbarsearch)", "form[role=search]"], "add_ci_search": [], "input_box_selector": ["form[method=get]:not(#adminbarsearch) input[type=text]", "form[method=get]:not(#adminbarsearch) input[type=search]", "form[role=search] input[type=text]", "form[role=search] input[type=search]"], "search_button_selector": ["form[method=get]:not(#adminbarsearch) button[type=submit]", "form[role=search] button[type=submit]", "form[method=get]:not(#adminbarsearch) input[type=submit]", "form[role=search] input[type=submit]"]}});