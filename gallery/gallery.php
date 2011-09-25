<?php
error_reporting (E_ERROR | E_PARSE);
ob_start ('gz_handler');
date_default_timezone_set ('Europe/London');
header('Content-type: text/html; charset=UTF-8');

function do_page($vars, $echo = True)
{
    $xhtml = file_get_contents("template.tpl");

    foreach($vars as $key => $var)
    {
        $xhtml = str_replace("{{$key}}", $var, $xhtml);
    }

    $xhtml = str_replace(array('...',      '---',     '--'),
                         array('&hellip;', '&mdash;', '&ndash;'),
                         $xhtml);

    if($echo)
    {
        echo $xhtml;
    } else {
        return $xhtml;
    }
}

function run_function($query, $functions)
{
    foreach($functions as $pattern => $function)
    {
        if(preg_match($pattern, $query))
        {
            call_user_func($function);
        }
    }
}

$URLs = array (':^/gallery/$:'    => 'Gallery',
               ':^/gallery/.*/$:' => 'Screenshot');

run_function($_SERVER['REQUEST_URI'], $URLs);

function Gallery()
{
    $screenshots = array('Azathoth' => array(),
                         'Randolph' => array());

    foreach(array_keys($screenshots) as $host)
    {
        $dir_handle  = @opendir("../screenshots/{$host}");
        
        while($file = readdir ($dir_handle))
        {
            if(stristr ($file, '.png'))
            {
                $name = str_replace('.png', '', $file);
                $screenshots[$host][intval (str_replace ('-', '', $name))] = array('name' => $name,
                                                                            'file' => $file);
            }
        }
        
        closedir($dir_handle);
        
        ksort($screenshots[$host]);
        $screenshots[$host] = array_reverse($screenshots[$host]);
    }
    
    foreach($screenshots as $host => $subscreenshots)
    {
        $content .= "<h2 class=\"gallery\">{$host}</h2>\n";
        $content .= "<ol class='gallery'>\n";
        foreach($subscreenshots as $screenshot)
        {
            $content .= "\t    <li><a href='/gallery/{$host}/{$screenshot['name']}/'><img src='/screenshots/{$host}/thumb/{$screenshot['file']}' alt='{$screenshot['name']}'/></a></li>\n";
        }
        $content .= "\t  </ol>\n";
    }

    do_page(array('title'    => 'Gallery',
                  'content'  => $content,
                  'identica' => file_get_contents('http://www.barrucadu.co.uk/user/themes/barrucadu/inc/identica.php')));
}

function Screenshot()
{
    $id   = explode('/', $_SERVER['REQUEST_URI']);
    $host = $id[2];
    $id   = $id[3];

    $info    = file_get_contents("../screenshots/{$host}/info/{$id}.txt");
    $content = "<a href='/screenshots/{$host}/{$id}.png'><img src='/screenshots/{$host}/thumb-big/{$id}.png' alt='Click to view full size.'/></a>
<ol id='info'>{$info}</ol>";
    
    do_page(array('title'    => "Screenshot: {$id}",
                  'content'  => $content,
                  'identica' => file_get_contents('http://www.barrucadu.co.uk/user/themes/barrucadu/inc/identica.php')));
}

?>