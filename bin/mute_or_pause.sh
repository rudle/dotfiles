MPC_PLAYING="`mpc status | grep -Ec "(playing|paused)"`"

if [ "$MPC_PLAYING" -eq "1" ]
then
  `mpc toggle`
else
  `mute`
fi
