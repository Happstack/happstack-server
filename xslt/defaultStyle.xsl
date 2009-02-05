<!DOCTYPE xsl:stylesheet PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xslt2.dtd">
<xsl:stylesheet 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
 >

<!--
have all the xml representing functions styled by a stylesheet that includes this one
override parts for each one

-->

<!--<xsl:include href="forms.xsl"/>-->

<xsl:template match="/">
  <xsl:apply-templates select="/" mode="page">
	<xsl:with-param name="staticURL" select="$index/@staticURL"/>
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="/*" mode="page">
  <xsl:param name="onload"/>
  <html xmlns="http://www.w3.org/1999/xhtml">
	<xsl:apply-templates select="." mode="head"/>
	<body  onload="{$onload}">
	  <div id="body">
		<!--<xsl:value-of select="name(current())"/>-->
		<div id="header">
		  <xsl:apply-templates mode="header" select="."/>
		</div>
		<div id="content">
		  <xsl:apply-templates mode="content" select="."/>
		</div>
		<div id="footer">
		  <xsl:apply-templates mode="footer" select="."/>
		</div>

	  </div>
	</body>
  </html>
</xsl:template>

<xsl:template match="*" mode="title">
Default Title
</xsl:template>

<xsl:template match="*" mode="inHead">
  <!--<base href="$staticURL"/>-->
</xsl:template>

<xsl:template match="*" mode="head">
  <title>
	<xsl:apply-templates mode="title" select="." />
  </title>
  <xsl:apply-templates mode="inHead" select="." />
  <link rel="stylesheet" href="{$staticURL}style.css" 
		type="text/css" media="all" />
  <script src="{$staticURL}script.js" type="text/javascript"/>

  <meta name="MSSmartTagsPreventParsing" content="TRUE" />
  <!--<meta http-equiv="content-type" content="text/html; charset=iso-8859-1" />-->
  <meta http-equiv="expires" content="-1" />
  <meta http-equiv=" pragma" content="no-cache" />
  <meta name="robots" content="all" />  
</xsl:template>

<xsl:template name="titleBlock">
  <xsl:param name="siteName"/>
  <xsl:param name="homeURL"/>
  <xsl:param name="tagLine"/>
  <div id="titleBlock">
	<div id="siteName">  <a href="{$homeURL}"><xsl:value-of select="$siteName"/></a>	</div>
	<h3 id="tagLine"><xsl:value-of select="$tagLine"/></h3>
  </div>

</xsl:template>


<xsl:include href="defaultXML.xsl"/>
<xsl:template match="*" mode="content">
  <xsl:apply-templates select="/" mode="xml"/>
</xsl:template>

<xsl:include href="forms2.xsl"/>

</xsl:stylesheet>