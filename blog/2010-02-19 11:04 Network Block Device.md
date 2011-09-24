I discovered this wonderful thing yesterday, nbd is, simply, a way of sharing block devices over a network. I currently have my server rsyncing to a backup HDD attached to my laptop.

NBD comes in two parts, the client and the server. The server sits on the host machine, listens for NBD network requests, and translates them into standard filesystem calls. The client sits on the client machine and is used to control the **nbd** kernel module. It's remarkably simple to share a device, for example:

    # On the server:
    nbd-server $port /dev/sdb1
    
    # On the client:
    modprobe nbd
    nbd-client $ip $port /dev/nbd0

And voila, /dev/sdb1 on the server is now accessable on the client as /dev/nbd0. I used this method to partition my new HDD over the network using gparted, unfortunately parted gives an error message after every change complaining that the kernel hasn't re-read the partition table, so you can only do one thing at a time. I should really learn how to use parted properly…

I wrote a helper script to share devices between my laptop and server:

    function nbd()
    {
        device=""
        port=2000
        host=""

        if [[ -z "$3" ]]; then
            if ifconfig wlan0 &>/dev/null; then # On my laptop
                host=eihort
            else                                # Not on my laptop
                host=192.168.1.64
            fi
        else
            host=$3
        fi

        # Allow specifying a device file, name, label, or UUID.
        if [[ -e "$2" ]]; then
            device=$2
        elif [[ -e "/dev/$2" ]]; then
            device="/dev/$2"
        elif [[ -e "/dev/disk/by-label/$2" ]]; then
            device="/dev/disk/by-label/$2"
        elif [[ -e "/dev/disk/by-uuid/$2" ]]; then
            device="/dev/disk/by-uuid/$2"
        else
            echo "Unknown device '$2'" 1>&2
            return 1
        fi

        if [[ "$1" == "share" ]]; then
            sudo nbd-server $port $device
        elif [[ "$1" == "gain" ]]; then
            sudo nbd-client $host $port $device
        elif [[ "$1" == "free" ]]; then
            sudo nbd-client -d $device
        else
            echo "Unknown method '$1'." 1>&2
            return 2
        fi
    }

So, with a simple **nbd share M2HDb** and **nbd gain nbd0**, I can share my backup device :)
