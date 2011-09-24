Arch Hurd has hit a big milestone, it can be booted! We can all bask in microkernally, if not particularly Archy yet, goodness. Now we need to improve our bootable environment (it has no config files, /dev is empty, et cetera) and start work on a toolchain, autotools, pacman, and all the other things we need to build packages. Currently, it's a pretty useless system—but not for long :)

### Building

Firstly we need to build Arch Hurd with Allan's crosshurd scripts:

    mkdir hurd
    wget http://repo.archhurd.org/crosshurd/crosshurd-20100116.tar.gz
    tar xvf crosshurd-20100116.tar.gz
    
    # You might need to edit ./scripts/prepare first.
    
    source ./scripts/makeall
    
    mkdir $HURD_DIR/servers
    touch $HURD_DIR/servers/exec
    mkdir $HURD_DIR/servers/socket
    
    mkdir $HURD_DIR/tmp
    chmod 1777 $HURD_DIR/tmp

Now you can either install to a partition or to a virtual hard disk for use with a virtualisation program such as qemu. GNU Mach doesn't support SATA drives yet, so I opted for qemu:

    # Create the image
    qemu-img create -f raw hurd.img 1GB
    
    # Make and format partition
    losetup /dev/loop0 hurd.img
    fdisk /dev/loop0
    losetup -d /dev/loop0
    # partition should start at sector 63. Check with fdist -ul hurd.img
    losetup -o 32256 /dev/loop0 hurd.img
    mkfs.ext2 -b 4096 -I 128 -o hurd -F /dev/loop0
    
    # mount image
    mkdir $ROOT/mnt
    mount -o loop /dev/loop0 /mnt
    
    # Then, build hurd and copy to /mnt
    cp -a /path/to/hurd /mnt
    
    # Unmount
    umount /mnt
    losetup -d /dev/loop0
    
    # Download a grub boot image
    wget http://www.dolda2000.com/~fredrik/grub.img
    
    # Note: Networking doesn't seem to work in qemu 0.12.1, downgrade to 0.11.1
    qemu -boot a -fda grub.img -hda hurd.img -net nic,vlan=1 -net user,vlan=1

And now, you have a minimally bootable Hurd system.

### After booting

I imagine one of the first things you'll notice is the complaint about the lack of /dev/console. So, let's set up some devices.

    settrans -c /servers/socket/1 /hurd/pflocal
    cd /dev
    ./MAKEDEV console null zero time hd0 hd0s1

And, you have no config files. Let's start to create those:

    echo "/dev/hd0s1 / ext2 defaults 1 1" > /etc/fstab
    echo "root:x:0:0:root:/root:/bin/bash" > /etc/passwd

If you opted for qemu, we can also set up the network at this point:

    settrans -afgp /servers/socket/2 /hurd/pfinet -i eth0 -a 10.0.2.15 -g 10.0.2.2 -m 255.255.255.0
    echo "nameserver 10.0.2.3" > /etc/resolv.conf

### Extra Software

I have started writing build scripts for extra software, so first set the appropriate *_VER= lines in your ./scripts/prepare file:

    SED_VER=4.2.1
    NCURSES_VER=5.7
    NANO_VER=2.2.1

#### sed

    #!/bin/bash
    
    cd $SOURCE_DIR
    if [ ! -f sed-$SED_VER.tar.bz2 ]; then
      wget ftp://ftp.gnu.org/gnu/sed/sed-$SED_VER.tar.bz2
    fi
    
    cd $BUILD_DIR
    rm -rf sed-$SED_VER
    tar -xf $SOURCE_DIR/sed-$SED_VER.tar.bz2
    
    rm -rf sed-build
    mkdir -p sed-build
    cd sed-build
    
    ../sed-$SED_VER/configure \
      --host=$TARGET \
      --prefix=/usr
    make DESTDIR=$HURD_DIR install
    
    cd $ROOT

#### ncurses

    #!/bin/bash
    
    cd $SOURCE_DIR
    if [ ! -f ncurses-$NCURSES_VER.tar.gz ]; then
      wget ftp://ftp.gnu.org/pub/gnu/ncurses/ncurses-$NCURSES_VER.tar.gz
    fi
    
    cd $BUILD_DIR
    rm -rf ncurses-$NCURSES_VER
    tar -xf $SOURCE_DIR/ncurses-$NCURSES_VER.tar.gz
    
    rm -rf ncurses-build
    mkdir -p ncurses-build
    cd ncurses-build
    
    ../ncurses-$NCURSES_VER/configure \
      --host=$TARGET \
      --prefix=/usr \
      --mandir=/usr/share/man
      --with-shared \
      --with-normal \
      --without-debug \
      --without-ada \
      --enable-widec
    
    make
    make DESTDIR=$HURD_DIR install
    
    cd $ROOT

#### nano

    #!/bin/bash
    
    cd $SOURCE_DIR
    if [ ! -f nano-$NANO_VER.tar.gz ]; then
      wget http://www.nano-editor.org/dist/v2.2/nano-$NANO_VER.tar.gz
    fi
    
    cd $BUILD_DIR
    rm -rf nano-$NANO_VER
    tar -xf $SOURCE_DIR/nano-$NANO_VER.tar.gz
    
    rm -rf nano-build
    mkdir -p nano-build
    cd nano-build
    
    ../nano-$NANO_VER/configure \
      --host=$TARGET \
      --prefix=/usr \
      --sysconfdir=/etc \
      --enable-color \
      --enable-nanorc \
      --enable-multibuffer \
      --disable-wrapping-as-root
    
    make
    make DESTDIR=$HURD_DIR install
    
    cd $ROOT
