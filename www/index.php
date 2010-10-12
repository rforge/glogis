<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://R-Forge.R-project.org/themes/rforge/';

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


<h2>glogis: Fitting and Testing Generalized Logistic Distributions</h2>
<h3>Thomas Winderberger, Achim Zeileis</h3>

<p>R package for fitting and testing generalized logistic distributions (type I).</p>

<ul>
  <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name;?>/">R-Forge summary page</a>.
  <li>R package: [<a href="https://R-Forge.R-project.org/src/contrib/glogis_0.0-4.tar.gz">source package</a>]
      [<a href="https://R-Forge.R-project.org/bin/windows/contrib/latest/glogis_0.0-4.zip">Windows binary</a>]
      [<a href="https://R-Forge.R-project.org/bin/macosx/leopard/contrib/latest/glogis_0.0-4.tgz">MacOS X binary</a>]
</ul>


<h2>Structural Breaks in Inflation Dynamics within the European Monetary Union</h2>
<h3>Thomas Winderberger, Achim Zeileis</h3>

<p>Investigation of European inflation rates based on Harmonized Index of Consumer Prices (HICP).
Inflation series are estimated by generalized logistic distribution model, tested for structural breaks, and (if any) breakpoints
are determined.</p>

<ul>
  <li><a href="https://R-Forge.R-project.org/scm/viewvc.php/*checkout*/pkg/paper/Slides/slides.pdf?root=glogis">Slides</a></li>
  <li><a href="https://R-Forge.R-project.org/scm/viewvc.php/*checkout*/pkg/paper/Paper/draft.pdf?root=glogis">Manuscript draft</a></li>
</ul>

</body>
</html>
