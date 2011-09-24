The credit for this idea goes to Arne Babenhauserheide, who submitted it to the bug-hurd mailing list. Basically, using a couple of Hurd translators (hostmux with httpfs, unionfs, and hgmerger), as well as a version control system (such as mercurial, git, or subversion), you can create a read/write version of the World Wide Web, in stark contrast to the mainly read-only one we have currently.

The full email can be seen in the [bug-hurd archives](http://www.mail-archive.com/bug-hurd@gnu.org/msg19236.html).

The goal is to be able to make local edits of the world wide web and share those edits with others. We shall accomplish this using the translators **hostmux** with **httpfs**, **unionfs**, and **hgmerger**.

Now, using the hostmux translator, we can access websites at the path /http://..., this can be used to obtain the files to edit. On top of that, we shall use unionfs, to show our edited files in place over the original ones. For files in our local edit tree, all reads are directed to those, otherwise to the remote file. On top of *that*, we have hgmerger. hgmerger gets writes, merges them with the original files, and saves the changes to our local tree. Let's use this directory structure for reference:

    /http://* - the real web
    ~/.http://* - the overlay
    ~/.bare-http://* - incoming changes. 

When reading a site, hgmerger copies it from /http:// to ~/.bare-http and merges in to ~/.http://. In the case of a merge conflict, the original remote file will win. Our unionfs set up from earlier will ensure that the modified files are now seen when you request them through your browser, rather than the original files. This is getting pretty cool now.

Now, the repository in ~/.http:// can be shared online so other people can access your changes. And behold, a read/write web using the power of the Hurd and translators.

When I get my Hurd qemu image working again, I'll almost certainly be trying this and report back with my progress.
