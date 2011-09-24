[Download](http://github.com/Barrucadu/home/blob/master/bin/mkahurd)

Over the past couple of days I've been working on a script to produce Arch Hurd LiveCDs, and then decided to expand it to support QEMU images (and soon HDD installs). It is called, unsurprisingly, **mkahurd**, using my usual talent for names.

    % mkahurd --help
    USAGE: mkahurd [-v] (--livecd | --qemu) /path/to/working/directory
    
    Options:
        -v:       Verbose output (must be first argument)
        --livecd: Generate a livecd
        --qemu:   Generate a 10GB qemu image
    
    mkahurd is written and maintained by Michael Walker.

Currently it's a bit picky about the order of the parameters (in that **-v** *must* be the first parameter, if specified), though I plan to work on that. Producing both ISOs and images is pretty simple:

### ISO

    % time mkahurd --livecd ~/hurd/tmp/live
    ==> Downloading contents of core...
    ==> Installing packages...
    ==> Installing setup script
    ==> Building LiveCD...
    ==> Done. ISO built as /home/barrucadu/hurd/tmp/live/ahurd.iso.
    > For official livecds, do the following:
    >     * Add documentation (installation guide & translator intro)
    >     * Set up helpful /etc/motd file
    Real: 876.35s User: 12.69s System: 3.64s Percent: 1% Cmd: mkahurd --livecd ~/hurd/tmp/live

### QEMU

    % time mkahurd --qemu ~/hurd/tmp/qemu
    ==> Creating image...
    Password: 
    ==> Installing 'base'...
    ==> Installing 'base-devel'...
    ==> Finishing off...
    ==> Done. Image built as /home/barrucadu/hurd/tmp/qemu/ahurd.img.
    > You will need to install GRUB yourself using a GRUB boot image.
    > No configuration has been performed.
    > First boot will fail. You will have to MAKEDEV hd0 and hd0s1 when dropped to a recovery console.
    Real: 421.67s User: 14.60s System: 2.24s Percent: 3% Cmd: mkahurd --qemu ~/hurd/tmp/qemu

The commands do take a little while to run, as a lot of packages have to be downloaded. In addition, to update a LiveCD, you can just run `mkahurd --livecd` again with the same working directory, and it will run much faster (as not so many packages need to be downloaded).
