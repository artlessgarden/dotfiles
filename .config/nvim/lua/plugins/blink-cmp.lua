return {
	{
		"saghen/blink.cmp",
		version = "*",
		dependencies = {
			"rafamadriz/friendly-snippets",
		},
		opts = {
			keymap = {
				preset = "enter",
			},
			appearance = {
				nerd_font_variant = "mono",
			},
			completion = {
				documentation = {
					auto_show = true,
					auto_show_delay_ms = 200,
				},
				menu = {
					border = "rounded",
				},
				documentation = {
					window = {
						border = "rounded",
					},
				},
			},
			signature = {
				enabled = true,
			},
			sources = {
				default = { "lsp", "path", "snippets", "buffer" },
			},
			fuzzy = {
				implementation = "prefer_rust_with_warning",
			},
		},
		opts_extend = { "sources.default" },
	},
}
