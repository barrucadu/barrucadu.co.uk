---
title: Minecraft
---
I am running a minecraft server on carcosa.barrucadu.co.uk, it is currently whitelisted and if you think you should have access, let me know.

## Map
<iframe src="http://www.barrucadu.co.uk/map/" width="600" height="400" />

[Full-screen](http://www.barrucadu.co.uk/map)

## Plugins and Commands
### [dynmap](http://dev.bukkit.org/server-mods/dynmap/)
This is what's providing the pretty map above.

### [MondoChest](http://dev.bukkit.org/server-mods/mondochest/)
Allows you to place items in a master chest that will be sorted into an arbitrary number of slave chests. This obviously makes depositing lots of items in a large storage room significantly faster, for a working example see the basement of the the Ayuyama pagoda.

### [Multiverse-Core](http://dev.bukkit.org/server-mods/multiverse-core/)
Adds support for multiple worlds and the teleportation between worlds. Players have access to the following commands:

 - `/mvenv`  - list the valid environments that can be created
 - `/mvwho`  - list the players in each world
 - `/mvlist` - list the worlds

### [Multiverse-Portals](http://dev.bukkit.org/server-mods/multiverse-portals/)
Adds support for making physical portals which can travel between worlds and portals. Here is a [quick tutorial](https://github.com/Multiverse/Multiverse-Portals/wiki/Basic-portal-tutorial). Players have access to the following commands:

 - `/mvp list` - list all portals
 - `/mvp select {portal}` - select the named portal} for future edits
 - `/mvp wand` - give yourself a magic selection wand
 - `/mvp info {portal}` - display information about the named portal
 - `/mvp create {name} [destination]` - make a portal going to the destination, if specified.
 - `/mvp remove {portal}` - delete the named portal
 - `/mvp modify {property} [value] -p [portal]` - Display the current value of, or edit the named property. The portal must be specified if it has not been selected.

A complete command reference is [available here](https://github.com/Multiverse/Multiverse-Portals/wiki/Command-Reference).

### [Multiverse-SignPortals](http://dev.bukkit.org/server-mods/multiverse-signportals/)
In theory this would let us make portals to another world by putting a sign on a nether portal. It doesn't seem to work though, perhaps a craftbukkit version incompatability.

### [Multiverse-NetherPortals](http://dev.bukkit.org/server-mods/multiverse-netherportals/)
Allows each world to have its own nether, and allows changing which world nether portals go to.

### [PermissionsEx](http://dev.bukkit.org/server-mods/permissionsex/)
Lets me control who can do what, in a more fine-grained way than "users can't do anything, ops can do everything". This is used to grant access to the plugins. Permissions are based on a hierarchy of groups, currently there are two groups, "default" and "admin". The permissions are as follows:

    default:
     - runecraft.* (own)
     - simplespawn.home.use (own)
     - simplespawn.use (own)
     - multiverse.core.list.* (own)
     - multiverse.portal.* (own)
     - multiverse.access.* (own)
    
    admin (extends default):
     - simplespawn.*
     - permissions.*
     - multiverse.*
     - modifyworld.*
     - bukkit.*

### [Runecraft](http://www.minecraftwiki.net/wiki/Mods/Runecraft)
Allows the building of special shapes, runes, which do awesome things when used. We've mostly been using this for teleporters and waypoints so far.

### [SimpleSpawn](http://dev.bukkit.org/server-mods/simplespawn/)
Lets me change the spawn point for new players, and also grants the following commands to players:

 - `/spawn` - go to the spawn point
 - `/home` - go to your bed

### [SkylandsPlus](http://dev.bukkit.org/server-mods/skylandsplus/)
An awesome map generator, used to generate our "skylands" world.
