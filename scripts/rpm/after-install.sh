ROOT=/opt/kazoo
CONFIG_ROOT=/etc/kazoo

# Add sup / sup bash completion
ln -s $ROOT/sup.bash /etc/bash_completion.d/
ln -s $ROOT/core/sup/priv/sup-dev /usr/bin/sup
chmod +x /usr/bin/sup

# Kazoo user
/usr/sbin/useradd -r -g daemon -M -d /opt/kazoo -s /sbin/nologin kazoo

# Systemd and launch
ln -s $CONFIG_ROOT/system/sbin/kazoo-applications /usr/sbin/kazoo-applications
cp $CONFIG_ROOT/system/systemd/kazoo-applications.service /etc/systemd/system/
systemctl daemon-reload

