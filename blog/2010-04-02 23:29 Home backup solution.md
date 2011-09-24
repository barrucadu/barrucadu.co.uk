Today I spent a little time thinking about how I backup my data, currently I occasionally ssh into my server and rsync it to an external HDD, as well as my desktop however this has a couple of problems. The major problem is that restoring from those backups is possibly impossible, and difficult at best; the backups are taken whilst the systems are running, during which time the filesystem is in an inconsistent state. That's not good for taking backups. Also, secondly, if, say, my house were to burn down (admittedly, initially my data would be the least of my worries if it did) I'd lose my backup. So I devised a solution to protect against these things.

### RAID 1

I've decided to add an extra HDD to both my server and desktop and set up a RAID1 array on both, thus the failure of one HDD in either computer wouldn't result in loss of use and, unless there were some greater underlying cause, the other HDD should last long enough for me to get a replacement for the broken one.

### External HDD

Semimonthly, I have decided to boot both computers from a livecd (a little tricky for the server, which has no CD drive), and rsync the hard drives to an external HDD. Thus, even if *both* HDDs were somehow to fail within quick succession, I'd still have a copy of the data. Additionally, some data (such as my school work) exists as a copy on my Archos 5, so at this point all my data exists in three or four devices.

### Online backup

Monthly, I shall rsync my external HDD backup to an encfs-protected online backup in my masses of spare Dreamhost space. England could be levelled by a meteor strike, but my data would be secure. Also, my code and configs are version controlled by git, and thus synced to github. At this point, all my data exists in four to six devices (not counting the backups made of the online stuff by Dreamhost and Github).

#### encfs

encfs is a brilliant little tool, and a good how-to can be [found here](http://www.movingtofreedom.org/2007/02/21/howto-encfs-encrypted-file-system-in-ubuntu-and-fedora-gnu-linux/), however, if you just want to get started:

    mkdir ~/crypto
    mkdir ~/plaintext
    encfs ~/crypto ~/plaintext

You can copy files to ~/plaintext, and see the encrypted stuff appear in ~/crypto. I mount my dreamhost stuff with sshfs, and use encfs on top of that; the only problem being that it's rather slow.

### Loss of data?

Highly unlikely. For my data to be lost, I'd need to have six HDDs fail, and both Dreamhost and Github shut down within so short a time period so that I either wouldn't notice, or would notice but wouldn't have the time to do anything about it. Short of a world war, or alien invasion, I don't think that is likely to happen.

### Paranoia?

Well, maybe. It is a pretty robust and sophisticated backup solution for a home user but, on the other hand, I don't want to lose my data in any circumstances. On the gripping hand, if I were paranoid, I'd get a big external HDD and store my own off-site backup in a safety deposit box or something.
