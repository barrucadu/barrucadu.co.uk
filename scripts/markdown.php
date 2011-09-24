<?php

/* Convert a markdown file to an HTML file.
 *
 * argv[argc-2] is the markdown file
 * argv[argc-1] is the html file
 *
 * The HTML file will be overwritten.
 */

require_once 'php-markdown-extra.php';

if (PHP_SAPI !== 'cli')
  die ('Not running from a command-line!');

$input  = $argv[$argc - 2];
$output = $argv[$argc - 1];

if (!is_readable ($input))
  die ('Input file is not readable!');

$markdown = file_get_contents ($input);
$html     = Markdown ($markdown);

$fh = fopen ($output, 'w');
fwrite ($fh, $html);
fclose ($fh);
?>