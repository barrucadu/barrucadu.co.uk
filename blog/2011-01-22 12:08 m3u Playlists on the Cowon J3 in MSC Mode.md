On Thursday my Cowon J3 arrived, it has epic sound quality (makes my headphones sound better than my speakers, and the equaliser is brilliant) and battery life (64 hours of music playback) and, importantly for me supports Vorbis and Flac, and has MSC (UMS) mode, so I can use it with GNU/Linux.

In short, it's brilliant.

Less brilliantly, the player itself has no functionality to make new playlists, this has to be done with a computer and the playlist copied over to the device. I'm told PLA playlists work great in MTP mode, though I'd rather use MSC so I set about investigating how to make m3u playlists. Turns out it's not so simple, they have to be in a specific format, so I don't think Cowon can even claim to support m3u (properly, at least).

In short, the playlists have to have certain metadata in comments, and use relative paths (I'm told absolute paths work, but I couldn't get it to behave). Thus, assuming your playlists are stored in \Playlists\, this format should work:

    #EXTM3U
    #EXTINF:539,Die Verbannten Kinder Evas - 05 - Beneath The Veil Of Ocean
    ..\Music\Music\Die Verbannten Kinder Evas\1995 - Die Verbannten Kinder Evas\Beneath The Veil Of Ocean.ogg
    #EXTINF:175,Die Verbannten Kinder Evas - 11 - Craving Dreams
    ..\Music\Music\Die Verbannten Kinder Evas\1995 - Die Verbannten Kinder Evas\Craving Dreams.ogg
    #EXTINF:344,Die Verbannten Kinder Evas - 03 - Darkened Skies
    ..\Music\Music\Die Verbannten Kinder Evas\1995 - Die Verbannten Kinder Evas\Darkened Skies.ogg
    #EXTINF:92,Die Verbannten Kinder Evas - 14 - Das Letze Kapitel
     ..\Music\Music\Die Verbannten Kinder Evas\1995 - Die Verbannten Kinder Evas\Das Letze Kapitel.ogg

And another example:

    #EXTM3U
    #EXTINF:93,Rozen Maiden OST - 01 - Kinjirareta Asobi (TV SIZE)
    ..\Music\Soundtracks\Rozen Maiden\Rozen Maiden\Rozen Maiden OST\01. ALI PROJECT - Kinjirareta Asobi (TV SIZE).ogg
    #EXTINF:140,Rozen Maiden OST - 02 - Battle of Rose
    ..\Music\Soundtracks\Rozen Maiden\Rozen Maiden\Rozen Maiden OST\02. Mitsumune Shinkichi - Battle of Rose.ogg
    #EXTINF:90,Rozen Maiden OST - 03 - Komatta Shumi
    ..\Music\Soundtracks\Rozen Maiden\Rozen Maiden\Rozen Maiden OST\03. Mitsumune Shinkichi - Komatta Shumi.ogg
    #EXTINF:133,Rozen Maiden OST - 04 - Atatakana Kokoro
    ..\Music\Soundtracks\Rozen Maiden\Rozen Maiden\Rozen Maiden OST\04. Mitsumune Shinkichi - Atatakana Kokoro.ogg

That is, it has to start with an EXTM3U comment, telling it that there is metadata. Each file is then specified with two lines, one containing the play length in seconds and the album/track/name. The second line is a relative path to the file. I currently have loads of playlists for most combinations of artists/albums I'll want to listen to, as I don't know how to do that with the J3 itself (yet), though I'm sure that's just me failing to figure out the player. However, I shall continue hoping for a firmware update which makes playlists easier (well, possible) to manage with the player itself, as that would be awesome and I would then have no complaints with the player.
