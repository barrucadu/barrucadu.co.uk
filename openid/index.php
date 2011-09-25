<?php
// IF YOU HAVE NOT DONE SO, PLEASE READ THE README FILE FOR DIRECTIONS!!!

/**
 * phpMyID - A standalone, single user, OpenID Identity Provider
 *
 * @package phpMyID
 * @author CJ Niemira <siege (at) siege (dot) org>
 * @copyright 2006-2008
 * @license http://www.gnu.org/licenses/gpl.html GNU Public License
 * @url http://siege.org/projects/phpMyID
 * @version 2
 */


/**
 * User profile
 * @name $profile
 * @global array $GLOBALS['profile']
 */
$GLOBALS['profile'] = array(
	// Basic Config - Required
	'auth_username'	=> 	'barrucadu',
	'auth_password' =>	'be32f292c3f51fcf277b534c4300678e',

	// Optional Config - Please see README before setting these
	'microid'	=>	array('mailto:mike@barrucadu.co.uk', 'http://www.barrucadu.co.uk'),
	'pavatar'	=>	'http://www.gravatar.com/avatar/62ba3621b81e0602aa32b5312c74b98d?s=175',

	// Advanced Config - Please see README before setting these
//	'allow_gmp'	=>	false,
//	'allow_test'	=> 	false,
//	'allow_suhosin'	=>	false,
//	'auth_realm'	=>	'phpMyID',
//	'force_bigmath'	=>	false,
//	'idp_url'	=>	'http://your.site.com/path/MyID.config.php',
//	'lifetime'	=>	1440,
//	'paranoid'	=>	false, // EXPERIMENTAL

	// Debug Config - Please see README before setting these
//	'debug'		=>	false,
//	'logfile'	=>	'/tmp/phpMyID.debug.log',
);

/**
 * Simple Registration Extension
 * @name $sreg
 * @global array $GLOBALS['sreg']
 */
$GLOBALS['sreg'] = array (
	'nickname'		=> 'barrucadu',
	'email'			=> 'mike@barrucadu.co.uk',
	'fullname'		=> 'Michael Walker',
	'dob'			=> '1991-09-09',
	'gender'		=> 'M',
	'postcode'		=> 'HU5 3RA',
	'country'		=> 'UK',
	'language'		=> 'en',
	'timezone'		=> 'Europe/London'
);

require('MyID.php');
?>
