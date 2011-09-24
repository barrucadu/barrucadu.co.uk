I've known for some time that when I go to university I'll have one wired network port in my room, and that's it. However, I have two computers, Azathoth and Eihort. Today, my new case for Eihort arrived (pictures will be taken at some point…) which, unlike the old one, actually has expansion ports, and my mini-ITX motherboard has one PCI slot. This coincidence isn't accidental :P

So I stuck in a network card, went to Google, and learned how to set up connection sharing. My set-up now looks like this:

    Azathoth -> Eihort -> Nyarlathotep -> Internet

Where Nyarlathotep is the router/modem I use to connect to the Internet.

My room has it's own little LAN in it, connected to the bigger LAN of my house through Eihort. At university the set up will be very similar except I doubt that their router(s) will be named Nyarlathotep (alas). In terms of interfaces, that's:

    Azathoth (eth0) -> Eihort (eth1)
    Eihort (eth0) -> Nyarlathotep -> Internet

### Eihort

Firstly, you need iptables installed, and then you can set up one rule, just one! One rule to, err, rule them all…

    pacman -S iptables
    /etc/rc.d/iptables start
    iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
    /etc/rc.d/iptables save

That's almost it. All to do now is give Eihort a static IP and tell the kernel it's OK to forward packets:

    ifconfig eth1 10.1.1.1
    echo 1 > /proc/sys/net/ipv4/ip_forward

To avoid having to do that echo on every boot, add **net.ipv4.ip_forward = 1** to /etc/sysctl.conf. I also added the **ifconfig** command to /etc/rc.local (not really worth bothering with a netcfg profile over).

### Azathoth

Azathoth needs to be assigned a static IP and told that Eihort is its gateway. Fortunately, using netcfg2, this is incredibly simple:

    CONNECTION="ethernet"
    DESCRIPTION="Shared network between desktop and server"
    INTERFACE=eth0
    IP="static"
    ADDR="10.1.1.2"
    GATEWAY="10.1.1.1"

Simple. Now running **netcfg2 shared** sets up Azathoth for my little set up. I did also have to edit my /etc/resolv.conf.head and /etc/hosts files to point to the new IP address of Eihort, 10.1.1.1.

And now, like magic, you have network sharing.
