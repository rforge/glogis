<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://R-Forge.R-project.org/themes/rforge/';
$rs_pkg = db_query("
		   SELECT *
	       FROM plugin_rforge_package_db
	       WHERE unix_group_name='$project_name'
			  AND pkg_name='$pkg_name';");
	       if (db_numrows($rs_pkg)!=1) {
		       echo "<h2>Package $pkg_name not found in project $project_name!<h2>";
		       return false;
	       }
	       $pkg = db_fetch_array($rs_pkg);


echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?>: Fitting and Testing Generalized Logistic Distributions</title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<h1>glogis: Fitting and Testing Generalized Logistic Distributions</h1>
<h2>Thomas Winderberger, Achim Zeileis</h2>

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<p>
  <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name;?>/">Project summary page</a>.
</p>


<h1>Structural Breaks in Inflation Dynamics within the European Monetary Union</h1>
<h2>Thomas Winderberger, Achim Zeileis</h2>

<ul>
  <li><a href="https://R-Forge.R-project.org/scm/viewvc.php/*checkout*/pkg/paper/Slides/slides.pdf?root=glogis">Slides</a></li>
  <li><a href="https://R-Forge.R-project.org/scm/viewvc.php/*checkout*/pkg/paper/Paper/draft.pdf?root=glogis">Manuscript draft</a></li>
  <li><a href="https://R-Forge.R-project.org/src/contrib/glogis_<?php echo $pkg['version'];?>.tar.gz">R package</a>
</ul>

</body>
</html>
