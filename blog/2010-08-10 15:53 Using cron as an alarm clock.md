Yesterday I left my Archos at work, which I usually use as an alarm, so I had to improvise. I settled on using cron to play some music very loudly in the morning. My first thought was to use mplayer for this, but then I realised that I didn't have it installed on my server, so I went for MPD instead.

The first thing to do, an entry in the crontab, looks innocent enough:

    15 08 * * *  /usr/local/bin/alarmsound

The **alarmsound** script is also pretty simple, but with a devastating effect...

    #!/bin/zsh
    
    mpc pause
    mpc clear
    mpc add "Music/DragonForce/Inhuman Rampage/Through the Fire and Flames.ogg"
    amixer set Master 100%
    mpc play
    mpc | grep -q "repeat: off" && mpc repeat

Now simply mute the sound with **amixer set Master 0%**, turn up the volume on your speakers (I have pretty good speakers, so I only went for 50% or so), and fall asleep. The next morning, you will be pleasantly awakened by a storm of sound that will cause you to fly out of your bed to mute it.

I don't think any alarm has ever gotten me out of bed faster.
