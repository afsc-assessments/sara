<<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title> Test</title>
</head>
<body>
<form action='pm.php' method='get'>
Number values to generate: 
  <input type='text' name='N' />
  <input type='submit' />
</form>
<?php>
if(isset($_GET['N']))
{
  $N = $_GET['N'];
 
  // execute R script from shell
  // this will save a plot at temp.png to the filesystem
  exec("Rscript pm.R $N");
 
  // return image tag
  $nocache = rand();
  echo("<img src='temp.png?$nocache' />");
}

?>
 
</body>
</html>