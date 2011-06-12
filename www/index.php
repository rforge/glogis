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
<a href="http://R-Forge.R-project.org/"><img src="<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<h2>glogis: Fitting and Testing Generalized Logistic Distributions</h2>
<h3>Thomas Windberger, Achim Zeileis</h3>

<p>R package for fitting and testing generalized logistic distributions (type I).</p>

<ul>
  <li>Stable release version: <a href="http://CRAN.R-project.org/package=glogis">[CRAN]</a></li>
  <li>Development version: <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name;?>/">[R-Forge]</a></li>
</ul>


<h2>Structural Breaks in Inflation Dynamics within the European Monetary Union</h2>
<h3>Thomas Windberger, Achim Zeileis</h3>

<p>To assess the effects of the EMU on inflation rate dynamics of
  its member states, the inflation rate series for 21 European countries
  are investigated for structural changes. To capture changes in mean,
  variance, and skewness of inflation rates, a generalized logistic
  model is adopted and complemented with structural break tests and
  breakpoint estimation techniques. These reveal considerable differences
  in the patterns of inflation dynamics and the structural changes
  therein. Overall, there is a convergence towards a lower mean inflation rate
  with reduced skewness, but it is accompanied by an increase in variance.</p>

<ul>
  <li>Working paper: <a href="http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-12">[pdf]</a></li>
  <li>Replication files: See <tt>demo(package = "glogis")</tt>.</li>
<!--  <li><a href="https://R-Forge.R-project.org/scm/viewvc.php/*checkout*/pkg/paper/Slides/slides.pdf?root=glogis">Slides</a></li> -->
</ul>

</body>
</html>
