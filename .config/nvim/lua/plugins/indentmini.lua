return {
	{
		"nvimdev/indentmini.nvim",
		event = "BufReadPre",
		config = function()
			require("indentmini").setup({
				char = "╎", -- 不要用 │ 或 ▏
				only_current = true, -- 关键！避免一堆线干扰光标
				minlevel = 1, -- 关键
			})
			-- 重点：避免反色叠加
			vim.api.nvim_set_hl(0, "IndentLine", {
				fg = "#3b4261",
				nocombine = true,
			})
		end,
	},
}
