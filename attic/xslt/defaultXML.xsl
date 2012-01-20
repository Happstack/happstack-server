<!DOCTYPE xsl:stylesheet PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xslt2.dtd">
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html" omit-xml-declaration="yes" 
			doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN" 
			doctype-system="http://www.w3.org/TR/html4/loose.dtd" 
			encoding="ISO-8859-1"/>


<xsl:strip-space
  elements = "*" />
<xsl:template name="root" mode="xml" match="/">
  <!--<html xmlns="http://www.w3.org/1999/xhtml">
	<body>
	  this is the top
first: <xsl:value-of select="name(./*[1])"/>-->
	  <hr/>
	  <xsl:apply-templates mode="xml" select="*"/>
	  <hr/>
<!--	  this is the end
	</body>
  </html>-->
</xsl:template>


<xsl:template mode="xml" match="html">
  <xsl:copy-of select="."/>
</xsl:template>
<xsl:template name="node" mode="xml" match="*">

  &lt;<xsl:value-of select="name(current())"/>
  &nbsp;  
  <xsl:apply-templates mode="xml" select="@*"/>

  <xsl:choose>
	<xsl:when test="count(*|text())>0">
	  &gt;
	  <ul>	<xsl:apply-templates mode="xml" select="*|text()"/>  </ul>
	  &lt;/<xsl:value-of select="name(current())"/>&gt;<br/>
	</xsl:when>
	<xsl:otherwise>
	  &nbsp;
	  /&gt;<br/>
	</xsl:otherwise>
  </xsl:choose>

  
</xsl:template>


<xsl:template name="text" mode="xml" match="text()">
  <xsl:value-of select="."/>
   <br/>

</xsl:template>


<!--[name(current())!='']-->
<xsl:template name="attr" mode="xml" match="@*">
	<!--<br/>&nbsp;&nbsp;  &nbsp;&nbsp;  &nbsp;  &nbsp;  -->
	&nbsp;
	<xsl:value-of select="name(current())"/>=&quot;<a href="{current()}"><xsl:value-of select="current()"/>&quot;
	
	</a>

</xsl:template>


</xsl:stylesheet>