#!/bin/sh

echo "hi!" >> /tmp/foo
user=`who| grep " :0" | awk '{print $1}'`
export XAUTHORITY=/home/$user/.Xauthority
export DISPLAY=:0

grep -q closed /proc/acpi/button/lid/*/state
LID_OPEN=$?
xrandr | grep VGA | grep -q disconnected
VGA_CONNECTED=$?

xrandr --output TV --off
if [ $LID_OPEN == 1 -a $VGA_CONNECTED == 1 ]; then
    xrandr --output VGA --mode 1920x1200
    xrandr --output LVDS --auto --right-of VGA
elif [ $LID_OPEN == 1 ]; then 
    xrandr --output VGA --off
    xrandr --output LVDS --auto
elif [ $VGA_CONNECTED == 1 ]; then
    xrandr --output VGA --mode 1920x1200
    xrandr --output LVDS --off
else
    xrandr --output LVDS --auto
fi
