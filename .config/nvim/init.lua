
local map = vim.keymap.set
local opts = { noremap = true, silent = true }
vim.g.mapleader = vim.keycode(' ')


------ Better default

--vim.opt.termguicolors = true

--vim.opt.path:append("**")
--vim.opt.path = "**"
--vim.opt.wildmenu = true
--vim.cmd("set wildignore-=.*")
vim.opt.wildignore:append({"*.o", "*.a", "*.class" ,".cache/*"})
vim.opt.wildignorecase = true

vim.o.number = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.smartcase = true
vim.o.ignorecase = true
vim.o.hlsearch = true
--vim.o.signcolumn = 'yes'
vim.opt.autochdir = true
vim.opt.shada = [['20,<50,s10,h]]

---- clipboard
map({'n', 'x', 'o'}, '<leader>y', '"+y', {desc = 'Copy to clipboard'})
map({'n', 'x', 'o'}, '<leader>p', '"+p', {desc = 'Paste clipboard text'})

vim.api.nvim_create_autocmd({ "TextYankPost" }, {
	pattern = { "*" },
	callback = function()
		vim.highlight.on_yank({
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
map('n', '<leader>l', ':bnext<CR>', opts)
map('n', '<leader>h', ':bprevious<CR>', opts)
map('n', '<leader>x', ':bd<CR>', opts)
map('n', '<leader>w', ':w<CR>', opts)

map('n', '<leader>e', ':e **/*', opts)
map('n', '<leader>d', ':Explore ~<CR>', opts)

---- color switch
vim.o.background = 'dark'
vim.cmd.colorscheme('retrobox')

map('n', '<leader>b', function()
  if vim.o.background == 'dark' then
    vim.o.background = 'light'
    vim.cmd.colorscheme('retrobox')
  else
    vim.o.background = 'dark'
    vim.cmd.colorscheme('retrobox')
  end
end, opts)

-- fcitx5
map('i', '<Esc>', [[<Esc>:lua os.execute("fcitx5-remote -c")<CR>]], opts)

-- 退出 terminal-mode 用 <Esc>
map('t', '<Esc>', [[<C-\><C-n>]], opts)
-- terminal 模式模拟 Ctrl-R 效果
map('t', '<C-R>', [[<C-\><C-N>"'.nr2char(getchar()).'pi']], opts)


