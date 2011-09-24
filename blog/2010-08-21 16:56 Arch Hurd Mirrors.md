The Arch Hurd LiveCD has been surprisingly popular, and we have (at the time of writing) consumed 80% of our monthly bandwidth quota for this month. I plan to release the next version of the LiveCD next month, and there's a real chance we'd go over the limit without mirrors. Thus, I am happy to announce that there is now an Arch Hurd LiveCD mirror (which may also become a package mirror in the future), hosted by [Narf Hosting](http://www.narfhosting.com/).

![Narf Hosting](files/narfbig.png)
[Narf Hosting](http://www.narfhosting.com)

The hosting was negotiated by [gtklocker](http://www.gtklocker.com). Thanks to Narf Hosting and gtklocker! :)

Now, if you try to download the LiveCD through the direct HTTP download, you get redirected to a random mirror (so files.archhurd.org and archhurd.narfhosting.com currently), to cut down our bandwidth usage. One mirror should be more than enough for the forseeable future, but if you want to provide some bandwidth, please contact me. Currently we only have packages on the main server, so a package mirror would be great.

We use rsync for our mirror-able content, so as long as you have some spare bandwidth and the ability to run rsync through cron, you too can be an Arch Hurd mirror. A full list of mirrors, which will be updated automatically, can be found on the new [mirrors page](http://www.archhurd.org/mirrors.php).
