    #!/usr/bin/env bash

current=$(gsettings get org.gnome.desktop.interface color-scheme)

if [[ "$current" == "'prefer-dark'" ]]; then
  gsettings set org.gnome.desktop.interface color-scheme 'default'
  echo "→ 切换为浅色"
else
  gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
  echo "→ 切换为深色"
fi
