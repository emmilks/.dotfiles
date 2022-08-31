#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi


## Polkit authentication
/usr/bin/mate-polkit &

xfce4-power-manager &
nm-applet &
volumeicon &
xrdb  ~/.Xresources
xmodmap ~/.Xmodmap
xrandr --mode 2560x1440 --rate 144
exec awesome
