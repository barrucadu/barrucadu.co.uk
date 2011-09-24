Well, my FSF membership card arrived today. It's pretty incredible really — fits in a wallet, and yet has 2GB of storage. It's, obviously, thicker than the average card, but it's not fat. If there is one thing flash memory is good for, it's things like this.

![FFS Membership Card](files/fsfcard-small.jpg)
[View Full Size](files/fsfcard.jpg)

Naturally, I immediately wanted to try the copy of Trisquel Live, a completely FOSS GNU/Linux distribution ASAP but, much to my chagrin, it didn't actually work on my computer, though it did in qemu. Rather than think "Great, it doesn't work", I instead thought "Great, an opportunity to learn how to use syslinux" :)

So I made three partitions, a 600MB fat32 partition for sharing files, a 1GB ext2 partiton for syslinux to live on, and a 250MB partition for a writable home folder to live on. Setting things up with unetbootin was incredibly easy, I just downloaded the latest Trisquel ISO, mounted my ext2 partition, and ran unetbootin. Unfortunately, though, unetbootin tried to use syslinux which only works on vfat partitions, so I had to manually install extlinux (with the command **extlinux -i**), and it booted!

Once I knew it worked, I decompressed the squashfs image and made a few changes — mounting the ext4 and fat32 partitions — remade the image, and rebooted to check it still worked. Well, it's now working wonderfully, and I've put some GNU stuff on the fat32 partition if ever I'm in need at a Windows machine, currently I've added

 * The **Freedom Fry** video
 * **Free Software, Free Society: Selected Essays of Richard M. Stallman** PDF
 * The latest edition of the **FSF Bulletin** (I plan to add new bulletins as and when they come out)

Can't think of anything else, but that should be enough for now. Oh, and my geek points have shot through the roof now that I have this thing in my wallet :P
