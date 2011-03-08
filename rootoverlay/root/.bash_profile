. $HOME/.bashrc

#*#+schlim+#*#
if [[ ! -f /tmp/_nox_ ]] && [[ "${XRUN}" == "y" ]]; then
    unset XRUN
    echo Starting 'schlim' login manager
    exec /opt/schlim/xlogin.py
fi
#*#-schlim-#*#
