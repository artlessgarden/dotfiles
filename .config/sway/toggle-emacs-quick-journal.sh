#!/bin/bash

NAME="emacs-quick-journal-xiang"

# 如果窗口未存在，就启动 Emacs 并让 for_window 把它移到 scratchpad
if ! swaymsg -t get_tree | grep -q "$NAME"; then
    emacsclient "~/agarden/inbox.org" -c -F "((name . \"$NAME\") (width . 100) (height . 30))" -a ""
    exit
fi

# 获取当前聚焦窗口的标题
FOCUSED=$(swaymsg -t get_tree | jq -r '
  recurse(.nodes[] + .floating_nodes[]) 
  | select(.focused) 
  | .name')

# 如果当前就是这个窗口，则隐藏
if [[ "$FOCUSED" == "$NAME" ]]; then
    swaymsg move scratchpad
else
    swaymsg "[title=\"$NAME\"] scratchpad show"
fi
