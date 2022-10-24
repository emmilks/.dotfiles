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

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x5f5f5f --height 18 &

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr

# Fireup screensaver
xscreensaver -no-splash &
## Polkit authentication
# /usr/bin/mate-polkit &

#xfce4-power-manager &
#nm-applet &
volumeicon &
xrdb  ~/.Xresources
xmodmap ~/.Xmodmap
xrandr --mode 2560x1440 --rate 144
exec xmonad
