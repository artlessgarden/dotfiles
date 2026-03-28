local map = vim.keymap.set
local opts = { noremap = true, silent = true }
vim.g.mapleader = vim.keycode(" ")

------ Better default

vim.opt.termguicolors = true

--vim.opt.path:append("**")
--vim.opt.path = "**"
--vim.opt.wildmenu = true
--vim.cmd("set wildignore-=.*")
vim.opt.wildignore:append({ "*.o", "*.a", "*.class", ".cache/*" })
vim.opt.wildignorecase = true

--vim.o.number = true
vim.o.expandtab = true -- tab 转空格
vim.opt.list = true
vim.opt.listchars = {
	space = "·", -- 空格显示为点
	tab = "» ", -- tab 显示
	trail = "·", -- 行尾空格
}
vim.o.tabstop = 4 -- tab 显示为 4 空格
vim.o.shiftwidth = 4 -- 自动缩进 4
vim.o.softtabstop = 4 -- 按 tab 插入 4 空格
vim.o.smartcase = true
vim.o.ignorecase = true
vim.o.hlsearch = true
--vim.o.signcolumn = 'yes'
vim.opt.autochdir = true
vim.opt.shada = [['20,<50,s10,h]]
vim.opt.clipboard = "unnamedplus"

vim.o.grepprg = "rg --vimgrep"

vim.api.nvim_create_autocmd("BufWritePre", {
	callback = function(args)
		local dir = vim.fn.fnamemodify(args.file, ":p:h")
		vim.fn.mkdir(dir, "p")
	end,
})

vim.api.nvim_create_autocmd({ "TextYankPost" }, {
	pattern = { "*" },
	callback = function()
		vim.hl.on_yank({
			timeout = 300,
		})
	end,
})

---- yank file name
map("n", "<leader>yf", function()
	vim.fn.setreg('"', vim.fn.expand("%:t"))
end, opts)

---- auto-save
-- vim.opt.updatetime = 1000
-- local autocmd = vim.api.nvim_create_autocmd
-- local events = {  "CursorHold",  "CursorHoldI",  "FocusLost",  "BufLeave",  "WinLeave",  "BufEnter",  "InsertLeave",}
-- for _, event in ipairs(events) do
--   autocmd(event, {    pattern = "*",    command = "silent! update",  })
-- end

---- buffer
map("n", "<leader>l", ":bnext<CR>", opts)
map("n", "<leader>h", ":bprevious<CR>", opts)
map("n", "<leader>x", ":bd<CR>", opts)
map("n", "<leader>w", ":w<CR>", opts)

map("n", "<leader>e", ":e **/*", opts)
map("n", "<leader>d", ":Explore ~<CR>", opts)

---- color switch
vim.o.background = "dark"
vim.cmd.colorscheme("retrobox")

map("n", "<leader>b", function()
	if vim.o.background == "dark" then
		vim.o.background = "light"
		vim.cmd.colorscheme("retrobox")
	else
		vim.o.background = "dark"
		vim.cmd.colorscheme("retrobox")
	end
end, opts)

-- fcitx5
map("i", "<Esc>", [[<Esc>:lua os.execute("fcitx5-remote -c")<CR>]], opts)

-- 退出 terminal-mode 用 <Esc>
map("t", "<Esc>", [[<C-\><C-n>]], opts)
-- terminal 模式模拟 Ctrl-R 效果
map("t", "<C-R>", [[<C-\><C-N>"'.nr2char(getchar()).'pi']], opts)

-- backlink
map("n", "gb", function()
	vim.cmd("silent grep % .")
	vim.cmd("copen")
end)

-------------------------------
local function open_gdrive_web()
	local idx = vim.fn.expand("$HOME/googledrive-local/.search-text")
	local map = idx .. "/.drive-map.tsv"
	local f = vim.fn.expand("%:p")

	if not vim.startswith(f, idx .. "/") then
		print("not in .search-text")
		return
	end

	local rel = f:sub(#idx + 2):gsub("%.txt$", "")
	local id =
		vim.fn.system(string.format([[awk -F '\t' -v p='%s' '$1==p{print $2; exit}' '%s']], rel, map)):gsub("%s+$", "")

	if id == "" then
		print("drive id not found")
		return
	end

	local url
	if rel:match("%.docx$") then
		url = "https://docs.google.com/document/d/" .. id .. "/edit?tab=t.0"
	elseif rel:match("%.xlsx$") then
		url = "https://docs.google.com/spreadsheets/d/" .. id .. "/edit"
	elseif rel:match("%.pptx$") then
		url = "https://docs.google.com/presentation/d/" .. id .. "/edit"
	else
		url = "https://drive.google.com/file/d/" .. id .. "/view"
	end

	vim.fn.jobstart({ "xdg-open", url }, { detach = true })
end

vim.keymap.set("n", "<leader>o", open_gdrive_web, { desc = "Open Google Drive web file" })
--------------------------------------

require("config.lazy")


vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "server.tsv",
  callback = function()
    vim.opt_local.expandtab = false
    vim.opt_local.tabstop = 16
    vim.opt_local.wrap = false

    vim.cmd("highlight G1 guifg=wheat")
    vim.cmd("highlight G2 guifg=#ffd93d")
    vim.cmd("highlight G3 guifg=#6bcB77")
    vim.cmd("highlight G4 guifg=#4d96ff")
    vim.cmd("highlight G5 guifg=pink")
vim.fn.matchadd("G1", "^[^\t]*")
vim.fn.matchadd("G2", "^\\([^\\t]*\\t\\)\\zs[^\\t]*")
vim.fn.matchadd("G2", "^\\([^\\t]*\\t\\)\\{2}\\zs[^\\t]*")
vim.fn.matchadd("G3", "^\\([^\\t]*\\t\\)\\{3}\\zs[^\\t]*")
vim.fn.matchadd("G3", "^\\([^\\t]*\\t\\)\\{4}\\zs[^\\t]*")
vim.fn.matchadd("G3", "^\\([^\\t]*\\t\\)\\{5}\\zs[^\\t]*")
vim.fn.matchadd("G4", "^\\([^\\t]*\\t\\)\\{6}\\zs[^\\t]*")
vim.fn.matchadd("G4", "^\\([^\\t]*\\t\\)\\{7}\\zs[^\\t]*")
vim.fn.matchadd("G5", "^\\([^\\t]*\\t\\)\\{8}\\zs[^\\t]*")
vim.fn.matchadd("G5", "^\\([^\\t]*\\t\\)\\{9}\\zs[^\\t]*")
vim.fn.matchadd("G5", "^\\([^\\t]*\\t\\)\\{10}\\zs[^\\t]*")

    vim.cmd("highlight MyP guifg=wheat")
    vim.fn.matchadd("MyP", "P")

vim.keymap.set("n", "<leader>c", function()
  vim.cmd('normal! T\tv t\ty')
end)

    vim.opt_local.cursorline = true
    vim.opt_local.cursorcolumn = true

  end,
})

