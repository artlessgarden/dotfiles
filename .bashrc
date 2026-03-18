#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


########################################################################################################
function parse_git_branch() {
  branch=$(git symbolic-ref --short HEAD 2>/dev/null) || return
  dirty=$(git status --porcelain 2>/dev/null)
  [[ -n "$dirty" ]] && echo " $branch*" || echo " $branch"
}
PS1='\n\[\e[38;5;223;48;5;238m\]\A \W\[\e[38;5;218m\]$(parse_git_branch)\[\e[0m\] '


########################################################################################################
# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion


########################################################################################################
# Set up fzf key bindings and fuzzy completion
eval "$(fzf --bash)"
export FZF_COMPLETION_OPTS='--info=inline'
export FZF_DEFAULT_OPTS='--bind "ctrl-y:execute-silent(printf {} | cut -f 2- | wl-copy --trim-newline)"'

#####################################################################
alias pacw="pacman -Qqdt | sudo pacman -Rns -"
alias pace="pacman --color always -Q | cut -f 1 -d ' ' | fzf --multi --ansi --preview 'pacman -Qi {1}' | xargs -ro sudo pacman -Rns"
pacr() {
    local pkg logline timestamp_str ts formatted
    # 生成“首次安装时间戳<TAB>包名”的列表
    while IFS= read -r pkg; do
        # 在 pacman.log 中查找包首次安装的记录
        # 日志行示例：[2020-12-15T22:23:45+0000] [ALPM] installed packagename (version)
        logline=$(grep -m1 -E "\] \[ALPM\] installed $pkg " /var/log/pacman.log)
        if [ -n "$logline" ]; then
            # 提取日志中第一对中括号里的内容，即时间戳字符串
            timestamp_str=$(echo "$logline" | awk -F'[][]' '{print $2}')
            # 将提取到的时间字符串转换为 Unix 时间戳
            ts=$(date -d "$timestamp_str" +%s 2>/dev/null)
            if [ -n "$ts" ]; then
                echo -e "$ts\t$pkg"
            fi
        fi
    done < <(pacman -Qe | cut -d ' ' -f1) | \
    # 按时间戳降序排序（最新的在最前面）
    sort -k1,1nr | \
    # 将时间戳转换为可读格式，并输出格式为：日期<TAB>包名
    while IFS=$'\t' read -r ts pkg; do
        formatted=$(date -d "@$ts" "+%Y-%m-%d %H:%M:%S")
        echo -e "$formatted\t$pkg"
    done | \
    # 通过 fzf 交互选择包，预览窗口显示 pacman -Qi 的详细信息
    fzf --multi --ansi --delimiter=$'\t' --preview 'pacman -Qi {2}' | \
    # 提取选中的包名并卸载（-Rns：卸载包及其不再需要的依赖）
    awk -F'\t' '{print $2}' | xargs -ro sudo pacman -Rns
}

alias pacs='pacman --color always -Sl | sed -e "s: :/:; /installed/d" | cut -f 1 -d " " | fzf --multi --ansi --preview "pacman -Si {1}" | xargs -ro sudo pacman -S'
alias pacd='paru --color always -Sl | sed -e "s: :/:; /installed/d" | cut -f 1 -d " " | fzf --multi --ansi --preview "paru -Si {1}" | xargs -ro paru -S'
alias pac="sudo pacman -Syu"

####################################################
# cd
shopt -s autocd
set -o noclobber
shopt -s checkwinsize

####################################################
alias vim='nvim'
alias ld='ls -Alh --color=auto'
alias cx='chmod +x'

alias gl='git clone --depth=1'
alias dot='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias lazydot='lazygit --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
dotadd () {
  cd "$HOME" || return 1

  # 1. add 白名单
  if [ -f "$HOME/.dotfiles-list" ]; then
    while IFS= read -r p; do
      # 去空白
      p="${p#"${p%%[![:space:]]*}"}"
      p="${p%"${p##*[![:space:]]}"}"

      # 跳过空行/注释
      [[ -z "$p" || "$p" == \#* ]] && continue

      if [ -e "$HOME/$p" ]; then
        echo "ADD  $p"
        dot add -- "$p"
      else
        echo "MISS $p"
      fi
    done < "$HOME/.dotfiles-list"
  fi

  # 2. add 你额外指定的
  if [ "$#" -gt 0 ]; then
    echo "ADD extra: $*"
    dot add -- "$@"
  fi

  echo
  dot status
}

    
paca() {
    expac -S "%-30n %d" | \
    fzf --multi --preview 'pacman -Si {1}' --layout=reverse --prompt='> ' | \
    awk '{print $1}' | \
    xargs -ro sudo pacman -S
}

####################################################
fg(){
  rg -n --hidden --smart-case \
     --max-columns 200 --max-columns-preview \
     -g '!.git' -g '!node_modules' -g '!dist' -g '!build' \
     "${*:-.}" |
  fzf --ansi -d: \
      --preview 'bat --style=numbers --color=always {1} --line-range {2}:+20' \
      --bind 'enter:execute(nvim {1} +{2})'
}
ff() {
  local dir="${1:-.}"
  dir="${dir/#\~/$HOME}"
  FZF_DEFAULT_COMMAND="fd --type f --hidden \"$dir\"" \
  fzf -m --preview 'bat --color=always --style=numbers {} 2>/dev/null || less {}' \
  | xargs -r "$EDITOR"
}
f() {
  local idx="$HOME/googledrive-local/.search-text"
  local map="$idx/.drive-map.tsv"

  rg -n --hidden --smart-case \
     --max-columns 200 --max-columns-preview \
     "${*:-.}" "$idx" |
  sed "s#^$idx/##" |
  fzf --ansi -d: \
      --preview "bat --style=numbers --color=always \"$idx\"/{1} --line-range {2}:+20" \
      --bind "enter:execute(nvim \"$idx\"/{1} +{2})" \
      --bind "alt-enter:execute(
        rel={1};
        rel=\${rel%.txt};
        id=\$(awk -F '\t' -v p=\"\$rel\" '\$1==p{print \$2; exit}' \"$map\");
        if [ -n \"\$id\" ]; then
          case \"\$rel\" in
            *.docx) xdg-open \"https://docs.google.com/document/d/\$id/edit?tab=t.0\" ;;
            *.xlsx) xdg-open \"https://docs.google.com/spreadsheets/d/\$id/edit\" ;;
            *.pptx) xdg-open \"https://docs.google.com/presentation/d/\$id/edit\" ;;
            *)      xdg-open \"https://drive.google.com/file/d/\$id/view\" ;;
          esac
        fi
      )"
}


####################################################
# ~/.bashrc 增强 bash 历史记录
# 1. 保留更多历史
HISTSIZE=10000000            # 当前 session 保留的命令数
HISTFILESIZE=20000000        # 写入 ~/.bash_history 的命令数

# 2. 清理重复，提升搜索可读性
HISTCONTROL=ignoredups:erasedups
HISTIGNORE="&:ls:[bf]g:exit:clear:pacu:pacs:pacr:paco:pacq:paca"   # 忽略重复、常用无意义命令

# 3. 添加时间戳（便于溯源）
HISTTIMEFORMAT="%F %T "
#HISTTIMEFORMAT='%Y-%m-%d %H:%M:%S  '
# __fzf_hist_with_time() {
#   # history 输出类似：  123  2026-01-04 11:02:33  curl ...
#   history | fzf --tac --no-sort
# }
# bind -x '"\C-r": "__fzf_hist_with_time"'

# 4. 支持多行命令记录
shopt -s cmdhist

# 5. 追加历史，不覆盖
shopt -s histappend

PROMPT_COMMAND='history -a; history -n'
shopt -s lithist


####################################################
d() {
    local tmp="$(mktemp)"
    yazi --cwd-file="$tmp"
    if [ -f "$tmp" ]; then
        cd "$(cat "$tmp")"
        rm -f "$tmp"
    fi
}

[ "$(tty)" = "/dev/tty1" ] && exec niri-session

fastfetch

export PATH="$HOME/.local/share/npm/bin:$PATH"

url2ip() {
  local host ip
  host=$(echo "$1" | awk -F/ '{print $3 ? $3 : $1}')
  ip=$(getent ahosts "$host" | awk '/STREAM/ {print $1}' | grep -E '^[0-9.]+$' | sort -u | head -n1)

  if [ -z "$ip" ]; then
    echo "❌ 未解析到 IPv4"
    return 1
  fi

  echo "$ip" | wl-copy
  echo "✅ $host -> $ip (已复制)"
}
