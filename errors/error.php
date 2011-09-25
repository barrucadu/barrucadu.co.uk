<?php

$errors = array('404' => 'File Not Found',
                '410' => 'Gone',
                '500' => 'Internal Server Error');

$error = $_GET['error'];
$name  = $errors[$error];

$characters = array('Konata Izumi', 'Kagami Hiiragi', 'Tsukasa Hiiragi');

$images = array('Konata Izumi'    => array('src' => 'konata.jpg',  'width' => '552', 'height' => '391'),
                'Kagami Hiiragi'  => array('src' => 'kagami.jpg',  'width' => '400', 'height' => '250'),
                'Tsukasa Hiiragi' => array('src' => 'tsukasa.jpg', 'width' => '370', 'height' => '396'));

$quotes = array('Konata Izumi'    => array("The flatness is awesome!",
                                           "So yeah, how's about that study group?",
                                           "I don't know, but suddenly I'm thinkin' that a few of these cookies aren't as tasty as some of the other cookies are. Is this one of your cookies, Kagami?",
                                           "Kay, I'm here, can I copy your homework now?",
                                           "I've been lamenting my lack of a chest for a while but then this game said \"A flat chest is a status symbol! A rarity!\" Word for Word. And when you think about it there is a demand, right? Which means I''m valuable!",
                                           "Bleh! Sounds like a lotta work to me. I think I'll pass, I'd rather play, and I'll ask for help when I get stuck!"),

                'Kagami Hiiragi'  => array("Can you stop guessing if they got the question right based on the amount of time left when they cut to commercial!",
                                           "Oh yeah, I'm sure you'd know all about that! ...Wait a minute, you're in the eleventh grade, how do you know about erotic game senarios?!",
                                           "What'd you say?!",
                                           "Woah. That's a big butt. and a layered belly.",
                                           "No! Like I said, Konata's not a normal girl. WE'RE the normal ones, lady!",
                                           "Do your own homework!",
                                           "Ah! Too intense for me!",
                                           "Your just going to copy my answers aren't you?!",
                                           "At least pretend to be guilty!",
                                           "Mission failed",
                                           "I have a name,you know",
                                           "WOULD YOU CUT IT OUT ALREADY!?!"),

                'Tsukasa Hiiragi' => array("I'm not having fun; I'm really not having fun!",
                                           "I don't have anything funny to say now...",
                                           "Good morning...",
                                           "Oh, homework!",
                                           "Balsamic Vinegar!"));

shuffle($characters);
$character = $characters[0];
shuffle($quotes[$character]);

$src   = $images[$character]['src'];
$w     = $images[$character]['width'];
$h     = $images[$character]['height'];
$alt   = "{$character} - Lucky Star";
$cite  = $character;
$quote = $quotes[$character][0];

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
    <img src="/errors/<?php echo $src; ?>" alt="<?php echo $alt; ?>" width="<?php echo $w; ?>" height="<?php echo $h; ?>"/>
    <div>
      <span><?php echo $cite; ?></span>
      <blockquote><?php echo $quote; ?></blockquote>
    </div>
    <p><a href="http://www.barrucadu.co.uk" title="Return to Website">Return to Website</a></p>
  </body>
</html>
