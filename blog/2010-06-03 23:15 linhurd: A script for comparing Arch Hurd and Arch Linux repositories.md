I've just finished writing linhurd which is, as the title says, a tool for comparing the Arch Hurd and Arch Linux repositories. linhurd can show packages in the Arch Linux repos but not the Arch Hurd ones (and vice versa), and package version differences between the two.

I expect *I* will be mainly using linhurd for checking if any of my packages have fallen behind and are out of date (checking against Arch Linux is easier than checking upstream, lazy, I know), and it'll probably be useful when deciding what to package next.

### The Script

    #!/bin/bash
    
    LINHURDDIR=/var/cache/linhurd
    
    function getpaccfg()
    {
        if [[ "$1" == "linux" ]]; then
            core="ftp://ftp.archlinux.org/core/os/i686"
            extra="ftp://ftp.archlinux.org/extra/os/i686"
        else
            core="http://files.archhurd.org/repo/core"
            extra="http://files.archhurd.org/repo/extra"
        fi
        
        echo "[options]"                           >  $LINHURDDIR/$1/pacman.conf
        echo "DBPath  = $LINHURDDIR/$1/"           >> $LINHURDDIR/$1/pacman.conf
        echo "LogFile = $LINHURDDIR/$1/pacman.log" >> $LINHURDDIR/$1/pacman.conf
        
        echo "[core]"                              >> $LINHURDDIR/$1/pacman.conf
        echo "Server = $core"                      >> $LINHURDDIR/$1/pacman.conf
        
        echo "[extra]"                             >> $LINHURDDIR/$1/pacman.conf
        echo "Server = $extra"                     >> $LINHURDDIR/$1/pacman.conf
    }
    
    function syncrepos()
    {
        if [[ "$UID" != "0" ]]; then
            echo "ERROR: This script must be run as root when synchronising packages."
            exit 1
        fi
        
        if [[ ! -e $LINHURDDIR/linux/pacman.conf ]] || [[ ! -e $LINHURDDIR/hurd/pacman.conf ]]; then
            # Make sure we have nothing weird lying around
            rm -r $LINHURDDIR/
            mkdir -p $LINHURDDIR/{linux,hurd}/
            
            # Make pacman configs
            getpaccfg linux
            getpaccfg hurd
        fi
        
        # Synchronise and get package lists
        pacman -Sy --config $LINHURDDIR/linux/pacman.conf
        pacman -Sy --config $LINHURDDIR/hurd/pacman.conf
        pacman -Sl core extra --config $LINHURDDIR/linux/pacman.conf | sort | sed 's/-[0-9]*$//' > $LINHURDDIR/linux/packages.list
        pacman -Sl core extra --config $LINHURDDIR/hurd/pacman.conf  | sort | sed 's/-[0-9]*$//' > $LINHURDDIR/hurd/packages.list
        
        < $LINHURDDIR/linux/packages.list cut -d' ' -f2 | sort > $LINHURDDIR/linux/names.list
        < $LINHURDDIR/linux/packages.list cut -d' ' -f3 | sort > $LINHURDDIR/linux/versions.list
        
        < $LINHURDDIR/hurd/packages.list cut -d' ' -f2 | sort > $LINHURDDIR/hurd/names.list
        < $LINHURDDIR/hurd/packages.list cut -d' ' -f3 | sort > $LINHURDDIR/hurd/versions.list
    }
    
    function linux()
    {
        comm $LINHURDDIR/linux/names.list $LINHURDDIR/hurd/names.list -2 -3
    }
    
    function hurd()
    {
        comm $LINHURDDIR/linux/names.list $LINHURDDIR/hurd/names.list -1 -3
    }
    
    function versions()
    {
        comm $LINHURDDIR/linux/names.list $LINHURDDIR/hurd/names.list -1 -2 | \
            while read pkg; do
            linux=`grep " $pkg " $LINHURDDIR/linux/packages.list | sed 's/^\(core\|extra\) //' | cut -d' ' -f2`
            hurd=`grep " $pkg " $LINHURDDIR/hurd/packages.list   | sed 's/^\(core\|extra\) //' | cut -d' ' -f2`
            
            if [[ "$linux" != "$hurd" ]]; then
                echo "$pkg $hurd [$linux]"
            fi
        done
    }
    
    function help()
    {
        echo "linhurd: script to compare Arch Linux and Arch Hurd repositories."
        echo
        echo "Usage:"
        echo "    linhurd [--sync|-S] [--linux|-L] [--hurd|-H] [--versions|-V]"
        echo
        echo "Options:"
        echo "    --sync      -S: Synchronise the local Arch Linux and Arch Hurd pacman databases."
        echo "    --linux     -L: Show packages in Arch Linux core and extra that are missing from Arch Hurd."
        echo "    --hurd      -H: Show packages in Arch Hurd core and extra that are missing from Arch Linux."
        echo "    --versions  -V: Show package version differences between Arch Hurd and Arch Linux."
        echo "    --help      -h: Show this text."
        echo
        echo "Author: Michael Walker (Barrucadu) <mike@barrucadu.co.uk>"
    }
    
    if [[ "$1" == "" ]]; then
        help
    else
        while [[ "$1" != "" ]]; do
            case "$1" in
                "--sync" | "-S")
                    syncrepos;;
                "--linux" | "-L")
                    linux;;
                "--hurd" | "-H")
                    hurd;;
                "--versions" | "-V")
                    versions;;
                "--help" | "-h")
                    help;;
            esac
            shift
        done
    fi

### Using

First, generate pacman configs and sync databases by running:

    sudo linhurd --sync

You should sync occasionally so the data doesn't fall too far behind what's actually in the repositories. This has to be run as root because (a) it edits files in /var/linhurd and (b) pacman complains when not run as root doing a database sync. However, the rest of linhurd is perfectly fine running as a normal user.

After syncing, you can check packages which are present in the Arch Linux repositories but not in the Arch Hurd ones by using the **--linux** flag (or **-L**), packages which are in the Arch Hurd repos but not the Arch Linux ones by passing the **--hurd** flag (or **-H**), and packages in both with version differences by using **--versions** (or **-V**). Multiple flags can be passed at once (*exempli gratia* **linhurd -L -V**).

In the future I may add an option to specify which repositories you wish to compare (though I don't suppose comparing any other than core and extra will give meaningful results).
