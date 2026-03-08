return {
  url = "https://codeberg.org/andyg/leap.nvim",
  config = function()
    local leap = require("leap")

    -- 当前窗口
    vim.keymap.set({ "n", "x", "o" }, "s", function()
      leap.leap({
        target_windows = { vim.api.nvim_get_current_win() },
      })
    end, { desc = "Leap (current window)" })

    -- 所有可见窗口
    vim.keymap.set({ "n", "x", "o" }, "S", function()
      leap.leap({
        target_windows = vim.tbl_filter(
          function(win)
            return vim.api.nvim_win_get_config(win).focusable
          end,
          vim.api.nvim_tabpage_list_wins(0)
        ),
      })
    end, { desc = "Leap (all windows)" })
  end,
}
