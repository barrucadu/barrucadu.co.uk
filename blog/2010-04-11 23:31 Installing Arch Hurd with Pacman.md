Arch Hurd has progressed to the point where it can easily be installed with pacman! Admittedly, the creation of the emu image is a bit of a pain still, but it's not so bad. However, once you've got that done, the actual installation takes a mere three commands!

Now, let's assume that you just happen to have, as you're reading this, an ext2 partition owned by hurd mounted at /mnt. What do you need to go from there? Well, a simple pacman.conf for Arch Hurd is one thing, and this one will do just fine:

    [options]
        RootDir     = /mnt/
        DBPath      = /mnt/var/lib/pacman/
        CacheDir    = /mnt/var/cache/pacman/pkg/
        LogFile     = /mnt/var/log/pacman.log
        SyncFirst   = pacman
        XferCommand = /usr/bin/wget --passive-ftp -c -O %o %u
        CleanMethod = KeepCurrent
    
    [core]    
        Server = http://files.archhurd.org/repo/core/
    
    [extra]
        Server = http://files.archhurd.org/repo/extra/

Now that you have your pacman.conf (which I saved as pachurd.conf), you can install with pacman!

    mkdir /mnt/var/lib/pacman -p
    pacman --config pachurd.conf -Sy
    
    # it's easiest just to install everything in core:
    pacman --config pachurd.conf -S acl attr autoconf bash binutils bzip2 cloog-ppl coreutils db diffutils expat file filesystem findutils gawk gcc gcc-libs gdbm gettext glibc gmp gnumach grep grub gzip hurd libarchive libfetch m4 make mpfr nano ncurses openssl pacman patch pcre perl ppl sed tar wget xz-utils zlib

It is (at the time of writing) about a 50MB download, so it doesn't take long, and if there is anything in core you really don't want, you can remove it yourself later.

And that, ladies and gentlemen, is how to install Arch Hurd using pacman!
