const {
	unmap,
	iunmap,
	vunmap,
	aceVimMap,
	mapkey,
	imap,
	imapkey,
	getClickableElements,
	vmapkey,
	map,
	cmap,
	addSearchAlias,
	removeSearchAlias,
	tabOpenLink,
	readText,
	Clipboard,
	Front,
	Hints,
	Visual,
	RUNTIME
} = api;

////settings
settings.modeAfterYank = 'Normal';
settings.tabsThreshold = 0;
//settings.scrollStepSize = 70;
//settings.omnibarPosition = 'bottom';
//settings.defaultSearchEngine = 's';
settings.tabsMRUOrder = false;
Hints.setCharacters("fldsieowbcnxhgqamz");
//settings.aceKeybindings = "emacs";
settings.smoothScroll = false;
settings.language = "zh-CN";
//settings.caretViewport = [0, 0, window.innerHeight / 2 + 100, window.innerWidth];

////map
api.mapkey('p', '#0enter ephemeral PassThrough mode to temporarily suppress SurfingKeys', function() {
    api.Normal.passThrough(1500);
});

api.mapkey('yo', 'copy org-mode link', function() {
    Clipboard.write(`[[${window.location.href}][${document.title}]]`)
});

map('<Ctrl-y>', 'ya');
map('<Alt-f>', 'cf');

unmap('u');
api.mapkey('uc', 'chatgpt ', function () {
    tabOpenLink("https://chat.openai.com")
});
api.mapkey('uo', 'outlook', function () {
    tabOpenLink("https://outlook.live.com/mail/0/")
});
api.mapkey('uw', '庐山天气', function () {
    tabOpenLink("https://m.weathercn.com/index.do?day=1&partner=&id=2333115")
});


////search
removeSearchAlias('b');
removeSearchAlias('d');
removeSearchAlias('e');
removeSearchAlias('g');
removeSearchAlias('h');
removeSearchAlias('s');
removeSearchAlias('w');
removeSearchAlias('y');

addSearchAlias('k', '小红书', 'https://www.xiaohongshu.com/search_result?keyword=', 's');
addSearchAlias('b', 'bili', 'https://search.bilibili.com/all?keyword=', 's');
addSearchAlias('i', 'github', 'https://github.com/search?q=', 's');
addSearchAlias('g', 'google', 'https://www.google.com/search?q=', 's');
addSearchAlias('x', 'x.com', 'https://x.com/search?q=', 's');
addSearchAlias('n', 'npm', 'https://www.npmjs.com/search?q=', 's');
addSearchAlias('m', 'mdn', 'https://developer.mozilla.org/en-US/search?q=', 's');
addSearchAlias('w', 'wikipedia', 'https://zh.wikipedia.org/w/index.php?search=', 's');
addSearchAlias('d', 'freedictionary', 'https://www.thefreedictionary.com/', 's');
addSearchAlias('y', 'youtube', 'https://www.youtube.com/results?search_query=', 's');
addSearchAlias('r', 'reddit', 'https://old.reddit.com/search?q=', 's')
addSearchAlias('p', 'pdd', 'https://mobile.pinduoduo.com/search_result.html?search_key=', 's')
addSearchAlias('t', 'taobao', 'https://s.taobao.com/search?page=1&q=', 's')
addSearchAlias('z', 'zhihu', 'https://www.zhihu.com/search?type=content&q=', 's')



//// theme
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #282828;
    color: #ebdbb2;
}
.sk_theme tbody {
    color: #b8bb26;
}
.sk_theme input {
    color: #d9dce0;
}
.sk_theme .url {
    color: #98971a;
}
.sk_theme .annotation {
    color: #b16286;
}
.sk_theme .omnibar_highlight {
    color: #ebdbb2;
}
.sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {
    background: #282828;
}
.sk_theme #sk_omnibarSearchResult ul li.focused {
    background: #d3869b;
}
#sk_status, #sk_find {
    font-size: 10pt;
}

:root {
    --theme-ace-bg:#282828ab; /*Note the fourth channel, this adds transparency*/
    --theme-ace-bg-accent:#3c3836;
    --theme-ace-fg:#ebdbb2;
    --theme-ace-fg-accent:#7c6f64;
    --theme-ace-cursor:#928374;
    --theme-ace-select:#458588;
}
#sk_editor {
    height: 50% !important; /*Remove this to restore the default editor size*/
    background: var(--theme-ace-bg) !important;
}
.ace_dialog-bottom{
    border-top: 1px solid var(--theme-ace-bg) !important;
}
.ace-chrome .ace_print-margin, .ace_gutter, .ace_gutter-cell, .ace_dialog{
    background: var(--theme-ace-bg-accent) !important;
}
.ace-chrome{
    color: var(--theme-ace-fg) !important;
}
.ace_gutter, .ace_dialog {
    color: var(--theme-ace-fg-accent) !important;
}
.ace_cursor{
    color: var(--theme-ace-cursor) !important;
}
.normal-mode .ace_cursor{
    background-color: var(--theme-ace-cursor) !important;
    border: var(--theme-ace-cursor) !important;
}
.ace_marker-layer .ace_selection {
    background: var(--theme-ace-select) !important;
} `;
settings.clickablePat = /(https?:\/\/|thunder:\/\/|magnet:)\S+/ig;
settings.interceptedErrors = ["*"];
mapkey(',o', 'Open detected links from text in new tab', () => {
  Hints.create(settings.clickablePat, el => {
    window.location.assign(element[2]);
  }, {statusLine: "Open detected links from text in new tab"});
});

// 自定义词典
api.Front.registerInlineQuery({
    url: function(q) {
        return `https://translate.googleapis.com/translate_a/single?client=gtx&sl=en&tl=zh-CN&hl=zh-CN&dt=t&dt=bd&dj=1&source=bubble&q=${q}`
    },
    parseResult: function(res) {
        const resObj = JSON.parse(res.text)

        if (resObj.dict && Array.isArray(resObj.dict) && resObj.dict.length > 0) {
            let con = document.createElement('div')
            con.style = "padding:0 15px;font-size:20px;line-height:1.2;min-height:300px;overflow:auto;"
            con.innerHTML = '<p>Google 翻译</p>'
            resObj.dict.forEach(d => {
                con.innerHTML += `<p><b>${d.pos}</b> ${d.terms.join(';')}<p>`
            })
            return con.outerHTML
        } else {
            let con = document.createElement('div')
            con.style = "padding:0 15px;font-size:20px;line-height:1.2;min-height:300px;overflow:auto;"
            con.innerHTML = '<p>Google 翻译</p>'
            con.innerHTML += `<p>暂无返回<p>`
            return con.outerHTML
        }
    }
});
