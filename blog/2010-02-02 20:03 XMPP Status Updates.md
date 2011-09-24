Yesterday I had the idea of writing a cronjob to, every five minutes say, check things like the CPU load and temperature, and send me a message via Jabber is anything was amiss. I then realised that I could have this script send me a list of new posts in my RSS feeds, and a list of package updates, and even a notification when a torrent is done.

Today, I found [sendxmpp](http://sendxmpp.platon.sk/), which, according to the website, “is a perl-script to send xmpp (jabber), similar to what mail(1) does for mail.” This seemed perfect for my needs, so I installed it, wrote the configuration file, ~/.sendxmpprc, which is in the following format and must be mode 0600:

    # A comment
    username@hostname password

The command itself is invoked as follows:

    echo "message goes here" | sendxmpp jid

A subject can be given, but for my purposes that was unnecessary. I wrote a script to run be run by cron at 5 minute intervals to alert me if the fan speed, CPU and HDD temperatures, or load average crossed certain thresholds, as well as checking on my torrents and telling me if any had finished since the last message:

    #!/bin/zsh
    
    memmax=993.0
    
    fanspeed=`sensors | grep fan1 | sed -e "s/fan1:\s*\([0-9]*\).*/\\1/"`
    cputemp=`sensors | grep temp3 | sed -e "s/temp3:\s*+\([0-9]*\).*/\\1/"`
    hddtemp=`sudo hddtemp -n /dev/sda`
    memusage=`free -m | grep "buffers/cache" | sed -e "s/-\/+ buffers\/cache:\s*\([0-9]*\)\s*\([0-9]*\).*/\\1/"`
    loadavg=`uptime | sed "s/.*load average: \(.*\)/\\1/"`
    
    memcheck=$[ `free -m | grep 'buffers/cache' | sed 's/.*\([0-9][0-9][0-9]\).*[0-9].*/\1/'` / $memmax * 100 ]
    loadcheck=`echo "$[\`uptime | sed "s/.*load average: \([0-9\.]*\)/\\1/" | sed "s/, .*//"\` * 100]" | sed "s/\..*//"`
    
    # Add notifications about the system not acting as it should - 2 spaces after the ] to line up with torrent messages.
    [ $fanspeed  -ge 4750 ] && echo "[status]  Northbridge fan speed too high: $fanspeed RPM" >> /tmp/xmppmessages
    [ $cputemp   -ge 50   ] && echo "[status]  CPU temperature too high: $cputemp °C"         >> /tmp/xmppmessages
    [ $hddtemp   -ge 45   ] && echo "[status]  HDD temperature too high: $hddtemp °C"         >> /tmp/xmppmessages
    [ $memcheck  -gt 33    ] && echo "[status]  Memory usage too high: $memusage / $memmax MB" >> /tmp/xmppmessages
    [ $loadcheck -ge 50   ] && echo "[status]  Load averages too high: $loadavg"              >> /tmp/xmppmessages
    
    # Only show torrents whose status has changed
    if [[ ! -f /tmp/torrentstatus ]]; then
        w3m -dump "http://localhost/rtorrentinfo.php?seed=1" | awk '{print "[torrent] " $0}' > /tmp/torrentstatus
        cat /tmp/torrentstatus >> /tmp/xmppmessages
    else
        w3m -dump "http://localhost/rtorrentinfo.php?seed=1" | awk '{print "[torrent] " $0}' > /tmp/torrentstatus2
        comm -1 -3 /tmp/torrentstatus /tmp/torrentstatus2 >> /tmp/xmppmessages
        mv /tmp/torrentstatus2 /tmp/torrentstatus
    fi
    
    # Send and clear the messages
    sendxmpp mike@barrucadu.co.uk < /tmp/xmppmessages
    echo > /tmp/xmppmessages

The reason I use a file to store the messages and then send them all in one go is because various other scripts append text to it—such as my hourly pacman database sync to notify me of new updates, and my hourly RSS sync, to notify me of new messages.

**rtorrentinfo.php** is a simple script I wrote to list seeding or leeching torrents, along with how much is complete (for leeching packages). I don't think it's overly relevent to the topic of the post, so if you want it, ask in the comments or send me an email.
