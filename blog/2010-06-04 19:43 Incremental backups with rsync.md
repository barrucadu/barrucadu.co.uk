Over the past couple of days I've been working on a backup script for both Azathoth and Eihort (desktop and server) that can perform full and incremental backups, either over SSH or locally, and this is what I've come up with.

    #!/bin/zsh
    
    # Backup script:
    #   Takes full and incremental backups, to a remote or local directory, using
    #   rsync. SSH transfer requires root login be allowed on the server, and SSH
    #   keys be exchanged for passwordless login. This script should be run as root.
    
    ################################## VARIABLES ###################################
    
    # Generla-purpose variables:
    #  COMPUTER:   hostname of current computer (used in the backup dest path)
    #  BACKUPHOST: username/hostname of the backup destination (if using ssh)
    #  BACKUPDIR:  directory to store the backup in
    #  TYPE:       the type of backup (full, inc)
    #  SSH:        whether backing up over SSH or not (ssh, nosh)
    COMPUTER=`hostname`
    BACKUPHOST=root@eihort
    BACKUPDIR=/media/BackupHDD
    TYPE=$1
    SSH=$2
    
    # rsync-related variables:
    #  FULLDIR:   directory for last full backup
    #  DIR:       directory to store current backup
    #  EXCLUDE:   path of rsync excludes file
    #  RSYNCARGS: arguments passed to rsync
    #  FULLARGS:  arguments passed to rsync in a full backup
    #  INCARGS:   arguments passed to rsync in an incremental backup
    #  SSHARGS:   arguments passed to rsync when using ssh
    #  RSYNCDIRS: dirs to sync
    FULLDIR=`date +%Y-%m`
    DIR=`date +%Y-%m-%d`
    
    if [[ "$TYPE" == "full" ]]; then
        DIR=$FULLDIR
    fi
    
    EXCLUDE=/etc/backup.exclude
    RSYNCARGS=(-avz --numeric-ids --delete --exclude-from=$EXCLUDE --stats)
    FULLARGS=()
    INCARGS=(--link-dest=../$FULLDIR/)
    SSHARGS=()
    RSYNCDIRS=(/ $BACKUPDIR/$COMPUTER/$DIR/)
    
    if [[ "$SSH" == "ssh" ]]; then
        SSHARGS=(-e ssh)
        RSYNCDIRS[2]=$BACKUPHOST:$BACKUPDIR/$COMPUTER/$DIR/
    fi
    
    ################################# MAIN SCRIPT ##################################
    
    function startbackup()
    {
        if [[ "$SSH" == "ssh" ]]; then
            ssh $BACKUPHOST "mkdir -p $BACKUPDIR/$COMPUTER/$DIR/"
            ssh $BACKUPHOST "touch /tmp/backup-$COMPUTER"
        else
            mkdir -p $BACKUPDIR/$COMPUTER/$DIR/
            touch /tmp/backup-$COMPUTER
        fi
    }
    
    function endbackup()
    {
        if [[ "$SSH" == "ssh" ]]; then
            ssh $BACKUPHOST "rm /tmp/backup-$COMPUTER"
        else
            rm /tmp/backup-$COMPUTER
        fi
    }
    
    if [[ $UID != 0 ]]; then
        echo 'Must be root!'
        return 1
    fi
    
    if [[ "$TYPE" == "full" ]]; then
        # Take a full backup
        startbackup
        rsync $RSYNCARGS $FULLARGS $SSHARGS $RSYNCDIRS
        endbackup
    elif [[ "$TYPE" == "inc" ]]; then
        # Take an incremental backup
        startbackup
        rsync $RSYNCARGS $INCARGS $SSHARGS $RSYNCDIRS
        endbackup
    else
        # wtf?
        echo "Usage: backup [full|inc] [ssh|nossh]"
        return 1
    fi

A simple script, but it makes use of a very powerful feature of rsync I only discovered recently.

### --link-dest

**--link-dest** specifies a directory, relative to the destination directory of the sync, for unchanged files to be hardlinked to. So, I can have two full backups of my entire filesystem, with the second only taking up as much space as there are changed files.

I'm making use of this by having incremental backups link against the last full backup (done at the beginning of the month), and each backup stored in a directory named after the date and computer. I take a full backup on the 1st of every month at 5PM, and an incremental backup every Sunday at 8PM — I chose those times so that, if the 1st is a Sunday, both backups will work just fine, and I usually have my computers on at those times so the chance of an automated backup being missed or failing and needing to be done manually is rather low.

So, for example, the full backup for Azathoth in August will be stored in **/media/BackupHDD/azathoth/2010-08** on Eihort, and the incremental backup for the third Sunday of September for Eihort will be stored in **/media/BackupHDD/eihort/2010-09-05**

### Keeping backups safe

Obviously, having automated backups by cron is great — but only if my backup HDD is protected. And it is. It is mounted as read-only by the fstab entry and, every Sunday and 1st of the month, five minutes before the backups are scheduled to begin, is remounted read-write.

Then, one hour after the backups have started, a script is run which checks every few seconds if the backups have finished (that's the purpose of the /tmp/backup-$COMPUTER file made by the script) and, when they have, syncs the backup HDD and remounts it read-only.

Thus, my backup HDD is only writable, if the backups finish in time, for one hour and five minutes a week (two hours and ten minutes for the first week). Of course, it's easy to remount it read-write manually and play with it, but this should prevent any accidental system-borks from messing up my backups too.
