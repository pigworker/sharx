<?php
require 'auth.php';

$attributes = $authenticationSource->getAttributes();
$user_id = escapeshellarg($attributes[uid][0]);
$page_id = escapeshellarg($_GET["page"]);

$descriptorspec = array(
   0 => array("pipe", "r"),  // stdin is a pipe that the child will read from
   1 => array("pipe", "w"),  // stdout is a pipe that the child will write to
   2 => array("file", "/tmp/error-output.txt", "a") // stderr is a file to write to
);

$cmd = "./sharx $user_id $page_id | pandoc";

$cwd = NULL;
$env = NULL;

$process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {
    // $pipes now looks like this:
    // 0 => writeable handle connected to child stdin
    // 1 => readable handle connected to child stdout
    // Any error output will be appended to /tmp/error-output.txt

    fwrite($pipes[0], serialize($_POST));
    fwrite($pipes[0], serialize($_GET));
    fclose($pipes[0]);

    echo stream_get_contents($pipes[1]);
    fclose($pipes[1]);

    // It is important that you close any pipes before calling
    // proc_close in order to avoid a deadlock
    $return_value = proc_close($process);
};

?>
