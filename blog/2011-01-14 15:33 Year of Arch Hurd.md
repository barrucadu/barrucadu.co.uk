![Arch Hurd](files/archhurd.png)

Earlier this year, Arch Hurd became one year old! A momentous achievement for something I never intended to start in the first place. In this past year we've had people come and go, more and more packages ported, overcome problems, released LiveCDs and, most importantly, progressed from merely an idea to a reality. It's hard to think now that Arch Hurd had [such humble beginnings](https://bbs.archlinux.org/viewtopic.php?pid=682472).

Some notable achievements over the past year have included:

 * **Getting Arch Hurd to be self-hosting**

  When we first started with Arch Hurd, we cross-compiled it from Arch Linux with a variety of scripts initially provided by [Allan](http://www.archhurd.org/team.php#allan). From that base we scripted and compiled, 'til we had a native toolchain running within Arch Hurd along with pacman! We started to package all the cross compiled stuff and now all Arch Hurd packaging is done from *within* Arch Hurd.

 * **Porting the Arch Linux initscripts**

  Once upon a time every Arch Hurd user booted to single user mode and didn't have niceties such as a VGA console or passwords. [Melpo](http://www.archhurd.org/team.php#melpo) decreed that this was not to be! And so set about the task of porting the Arch Linux initscripts, with all their goodness, to Arch Hurd. Admittedly, there was probably more rewriting than porting, and they still don't work *perfectly*, but we have Arch Linux-like goodness, complete with rc.conf—which has to be a good thing.

 * **Producing a LiveCD**

  In May, the first ever Arch Hurd LiveCD saw public release after much confusion on my part after researching how LiveCDs actually work, though it was only an early prototype and didn't feature a setup program; however, in June an updated LiveCD with installer was made available for download. There have now been three LiveCDs, with another on the way once a few outdated packages have been updated. Additionally, the next LiveCD might be the first ever bug free one (though I'm not holding my breath).

 * **Packaging Xorg**

  [Giselher](http://www.archhurd.org/team.php#giselher), in May, succeeded in getting all the packages required for Xorg and Openbox in extra. This was a big step forward as the Hurd terminal tends to freeze after a time, a fate to which X seems immune.

 * **Attended Software Freedom Day**
  
  [I](http://www.archhurd.org/team.php#barrucadu) and [hayashi](http://www.archhurd.org/team.php#hayashi) attended a Software Freedom Day event in Manchester in September. Though not as fun as I thought it would be, it was still nice to have an Arch Hurd box running for people to have a look at (if many of the looks were slightly amused).
  
 * **Packaging everything for GHAMP (GNU/Hurd, Apache, MySQL, and PHP)**

  Again done by giselher (our packaging overlord), in August everything was packaged which we need for GHAMP. A brilliant achievement, as the Hurd website runs on a Hurd (Debian, I assume) server, though I don't know of many servers running Arch Hurd in the wild (I think our mirror on [hackthehardware.net](http://www.hackthehardware.net) does).

Of course, that's only the things I consider most notable. We have also got a lot of packages (mostly owned by giselher), a (what I consider) [nice website](http://www.archhurd.org), users hanging out in IRC, the same users caring enough to host [mirrors](http://www.archhurd.org/mirrors.php)... but most of all, we've had fun. The Hurd isn't the easiest system to work with, and a lot of stuff needs patching to run properly (or even at all), it's a complete pain sometimes but, fundamentally, we do this because it's fun.
