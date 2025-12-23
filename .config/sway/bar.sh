#!/bin/bash

date=$(date +'%Y-%m-%d %a')
time=$(date +'%H:%M')

if [ ! -f "/$HOME/.config/sway/weather.txt" ] || [ $(($(date +%s) - $(stat -c %Y /$HOME/.config/sway/weather.txt))) -ge 900 ]; then
    curl -s 'wttr.in/Lushan,Jiujiang?m&format=%t' | sed 's/^+//' > /$HOME/.config/sway/weather.txt
fi
weather=$(cat /$HOME/.config/sway/weather.txt)


get_brightness() {
    local max=$(cat /sys/class/backlight/*/max_brightness)
    local current=$(cat /sys/class/backlight/*/brightness)
    echo $(( current * 100 / max ))
}
get_volume() {
    volume=$(amixer get Master | grep -oP '\[(\d+)%\]' | head -n1 | sed 's/\[\([0-9]*\)%\]/\1/')
    mute=$(amixer get Master | grep -o '\[off\]')

    if [ -n "$mute" ]; then
        echo "0"  # 静音时返回0
    else
        echo "$volume"  # 非静音时返回音量数值
    fi
}
get_battery() {
    cat /sys/class/power_supply/BAT*/capacity 2>/dev/null || echo "N/A"
}
BRIGHTNESS=$(get_brightness)
VOLUME=$(get_volume)
BATTERY=$(get_battery)

echo "    ☀ $BRIGHTNESS 🔊 $VOLUME 🔋 $BATTERY    $weather    $date    $time    "
