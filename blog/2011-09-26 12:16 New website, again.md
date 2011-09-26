I looked at my website the other day and thought to myself, "I haven't rewritten it from scratch for a while." And so I did. Whilst the appearence of the website is almost the same, it's now entirely done with static HTML files, as I realised I didn't need the functionality of a full blogging program such as Habari just to produce my website. So, I now have a series of scripts, XML template files, and markdown to produce my website.

The source code is available [on github](https://github.com/Barrucadu/barrucadu.co.uk).

### Structure

/archive/
: This is where generated archive files (now set up to be one for each year) will live. There is no code in here (beyond a .gitignore file).

/blog/
: Where blog posts live, this contains both the Markdown source (which is used when rendering the website), and the compiled HTML when the site is built.

/errors/
: 404, 410, and 500 error handling. Displays a random Touhou character and a random quote.

/files/
: Files which are linked to from blog posts or website pages. I decided to start hosting these on the same server as the website, as some of the earlier files have since vanished.

/openid/
: phpMyID, which I use for OpenID authentication.

/pages/
: Markdown source for website pages, however these live in / when compiled to HTML.

/screenshots/
: Thumbnails, info text, and compiled HTML screenshots of my computers.

/scripts/
: The main part of the system. I will explain these later.

/template/
: XML template files (currently only HTML and Atom), along with images and CSS used in the layout.

/.gitignore, /.htaccess, /robots.txt
: These are fairly self-explanatory

/rsync.exclude
: Files and directories to be excluded when uploading.

You may have noticed that no HTML, other than the template files, are stored in git. That's because they are not needed - the scripts can just rebuild them.

### Scripts

These scripts are rather messy and will be rewritten to be much nicer at some point.

make.sh
: This builds the entire website, calling the other scripts as appropriate.

makearchive.sh
: Takes an output filename, links to the next and previous archive entries, and a list of blog posts, and will then produce an archive page. This is also used to render the index page.

makefeed.sh
: Produces an Atom feed containing all of the posts given on the command line. Essentially, makearchive but using different template files.

makegallery.sh
: Produces the screenshot gallery page, currently this is hard-coded to produce a gallery of *all* screenshots.

makepage.sh
: Turn one page or blog post into a full HTML page.

makepagelinks.sh
: Used to generate the navigation in the sidebar

makescreenshot.sh
: Takes an output filename, computer name, and date, and produces a screenshot page for that.

markdown.php
: Compiles a markdown file to HTML, without doing any additional template stuff.

php-markdown-extra.php
: PHP Markdown Extra, the Markdown processing tool I am using

upload.sh
: Upload the website via rsync

### Usage

When I make or edit a page or blog post, I just need to call two scripts to rebuild the entire website and upload changes:

    ./scripts/make.sh
    ./scripts/upload.sh

And there you have it, my new website system! It's currently a bit messy, but that can be improved over time.


