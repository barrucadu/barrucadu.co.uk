I've been having some problems with the old(ish) 80GB HDD I've been using with my server—it developed bad sectors which made any disk-related operation (even (un)mounting partitions on different physical devices, for some reason) incredibly slow. I'm not exaggerating when I say that rebooting took about 45 minutes to an hour. Also, the bad sectors caused loads of error output to be logged and, as I thought a 5GB root partition would be more than enough, that quickly filled causing more problems...

So, yesterday I ordered a 160GB 2.5" SATA HDD, and it arrived today. I barely used any of the 80GB one, so I'm not sure what I'll do with 160GB… It took me about six hours to migrate from the old HDD to the new. If it wasn't so slow on (un)mounting, it'd have taken me about an hour, needless to say, today has been *very* frustrating.

...but, dear gods! I can't believe how much that HDD was slowing things down! I only really noticed it in mounting and doing large read/writes, but the whole thing is just so much faster! Of course, some of that will be due to changing to a SATA HDD from an old ATA one, and perhaps from using ext4 on most of the partitions instead of JFS, but the difference is really amazing. Things I had attributed to a limitation of my network speed—such as copying large numbers of files back and forth—now soar along. It's incredible really.

With a large disk, I decided to split up the whole into many partitions, so it's less likely for, say misbehaving log files, to grind the system to a halt. Also, I wanted to play with ext4, I've used JFS as my filesystem of choice for so long now I don't remember my initial reasons for doing so, but when I did settle on it, ext4 hadn't entered the mix so JFS might not be the best for me any more… But, I couldn't bring myself to fully leave JFS behind, so I settled on this partitioning scheme:

  * **/boot:** 128MiB, ext2
  * **/:** 15GiB, ext4
  * **/home:** 15GiB, ext4
  * **/usr:** 15GiB, ext4
  * **/var:** 15GiB, ext4
  * **/srv:** 100GiB, JFS

Of course, /srv isn't *really* 100GiB, but you know what I mean. The remaining space after 60ish GiB had been used for other things.

A short while ago I was looking at the Arch wiki article for installing a system with software RAID and LVM, it looks pretty simple. I've always imagined RAID and LVM as incredibly complex beasts only understandable by server admins. But, well, I *am* a server admin (albeit for a server with about three users), and it didn't seem too complex. So, at some point in the future when I have money and time to waste, I'll consider getting another two HDDs and setting up RAID 5 or something. I'll probably be on a new motherboard before then, though, as the one I'm using at the moment has only two SATA ports (and one PCI port, which is likely to be a more imminent limitation).

I have designs to, by the end of the year, have transformed my server into a much more powerful beast. Watch this space...
