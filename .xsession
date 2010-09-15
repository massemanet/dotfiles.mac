#!/bin/bash

#icewm-session & wm_pid=$!
icewm & wm_pid=$!
icewmtray &
icewmbg&

/home/masse/bin/randr

/home/masse/bin/Spotify&
amarok&
gkrellm&
klipper&
xscreensaver -no-splash&
nm-applet&

#icewmbg
wait $wm_pid
