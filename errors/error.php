<?php

$errors = array('404' => 'File Not Found',
                '410' => 'Gone',
                '500' => 'Internal Server Error');

$error = $_GET['error'];
$name  = $errors[$error];

$errors = array (array ('character' => 'Patchouli Knowledge',
                        'image'     => 'patchouli.jpg',
                        'messages'  => array ('
<strong>Patchouli:</strong> ...By the way, who are you?',

                                              '
<strong>Patchouli:</strong> The books here are worth all the donations to your shrine for the past five years!<br/>
<strong>Reimu:</strong> My shrine rarely has any visitors throughout the year.<br/>
<strong>Patchouli:</strong> Oh. Then I guess it\'s worth nothing.',

                                              '
<strong>Patchouli:</strong> Don\'t take any books, please.')));

shuffle($errors);
shuffle($errors[0]['messages']);

$src   = $errors[0]['image'];
$alt   = "{$errors[0]['character']} - Touhou";
$quote = $errors[0]['messages'][0];

?><!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en" dir="ltr">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"/>
    <meta name="author" content="Michael Walker"/>
    <meta name="robots" content="FOLLOW,NOINDEX"/>
    
    <title><?php echo "{$error}: {$name}"; ?></title>
    
    <link rel="stylesheet" href="/errors/style.css" type="text/css" />
    <link rel="shortcut icon" type="application/ico" href="/favicon.ico" />
    <base href="http://www.barrucadu.co.uk/"/>
  </head>

  <body>
    <h1><?php echo $error; ?></h1>
    <h2><?php echo $name; ?></h2>
    <img src="/errors/<?php echo $src; ?>" alt="<?php echo $alt; ?>"/>
    <div>
      <blockquote><?php echo $quote; ?></blockquote>
    </div>
    <p><a href="http://www.barrucadu.co.uk" title="Return to Website">Return to Website</a></p>
  </body>
</html>
