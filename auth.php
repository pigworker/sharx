<?php

# We start off with loading a file which registers the simpleSAMLphp classes with the autoloader.
require_once('/usr/share/simplesamlphp/lib/_autoload.php');

# We select our authentication source:
$authenticationSource = new SimpleSAML_Auth_Simple('cis-ldap');

# We then require authentication:
$authenticationSource->requireAuth();

?>
