<!DOCTYPE xsl:stylesheet PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xslt2.dtd">
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<!--generic forms implementation -->


<xsl:template match="form" mode="xml">
  <form id="{@id}"  action="{@action}" method="{@method}">
	<xsl:apply-templates select="*" mode="form"/>
  </form>
</xsl:template>


<xsl:attribute-set name="form">
  <xsl:attribute name="action"><xsl:value-of select="@action"/></xsl:attribute>
  <xsl:attribute name="method"><xsl:value-of select="@method"/></xsl:attribute>
  <xsl:attribute name="onsubmit">return verify(this)</xsl:attribute>
</xsl:attribute-set>

<xsl:template match="*" mode="input">
<xsl:param name="label"/>
<xsl:param name="cols">50</xsl:param>
<xsl:param name="size"/>
<xsl:param name="id"/>

<xsl:variable name="name" select="substring(concat(name(),@name), (string-length(name())+1)* round(string-length(@name) div (string-length(@name)+1)) )"/>
<xsl:variable name="id_" select="substring(concat($name,$id), (string-length($name)+1)* round(string-length($id) div (string-length($id)+1)) )"/>
<xsl:variable name="size_" select="substring(concat(@maxlength,$size), (string-length(@maxlength)+1)* round(string-length($size) div (string-length($size)+1)) )"/>

<label xstyle="vertical-align: top" for="{$id_}"><xsl:copy-of select="$label"/>:&nbsp;&nbsp;</label>

<xsl:choose>
<xsl:when test="@type='bool'">
<input type="radio" required="{@required}" name="{$name}" value="1"/> Yes
<input type="radio" required="{@required}" name="{$name}" value="0"/> No
</xsl:when>

<xsl:when test="@type='page'">
<!--<label for="{$name}"><xsl:value-of select="$label"/>: <br/></label>-->
<br/>
<textarea cols="{$cols}" rows="20" name="{$name}">
<xsl:copy-of select="./text()"/>
</textarea>
</xsl:when>


<xsl:when test="@type='para'">
<!--<label for="{$name}"><xsl:value-of select="$label"/>: <br/></label>-->
<br/>
<textarea cols="{$cols}" rows="6" name="{$name}">
<xsl:copy-of select="./text()"/>
</textarea>
</xsl:when>


<xsl:when test="@type='sentence'">
<br/>
<textarea cols="{$cols}" rows="3" name="{$name}">
<xsl:copy-of select="./text()"/>
</textarea>
</xsl:when>


<xsl:otherwise>
<input name="{$name}" id="{$id_}" required="{@required}" size="{$size_}" maxlength="{@maxlength}" value="{text()}"/>
</xsl:otherwise>
</xsl:choose>


</xsl:template>

<xsl:template match="*" mode="textarea">
<xsl:param name="label"/>
<xsl:param name="id"/>

<xsl:variable name="hasNameAttr" select="round(string-length(@name) div (string-length(@name)+1))"/>
<xsl:variable name="offset" select="$hasNameAttr*(string-length(name())+1)"/>
<xsl:variable name="name" select="substring(concat(name(),@name),$offset)"/>

<label for="{$name}"><xsl:value-of select="$label"/>: <br/></label>
<textarea cols="{@cols}" rows="{@rows}" name="{$name}">
<xsl:value-of select="text()"/>
</textarea>

</xsl:template>

<xsl:template match="*" mode="yes-no">
<xsl:param name="label"/>
<xsl:param name="id"/>
<xsl:variable name="name" select="substring(concat(name(),@name), (string-length(name())+1)* round(string-length(@name) div (string-length(@name)+1)) )"/>
<xsl:variable name="id_" select="substring(concat($name,$id), (string-length($name)+1)* round(string-length($id) div (string-length($id)+1)) )"/>

<label for="{$id_}"><xsl:value-of select="$label"/>: </label>

</xsl:template>

<!--

<xsl:element name="script">
<xsl:attribute name="language">javascript</xsl:attribute>
<xsl:attribute name="type">text/javascript</xsl:attribute>
//!-<script language="javascript" type="text/javascript">-
//   alert("")
<![CDATA[
//   alert("h2")
]]>

//-</script>
</xsl:element>
  <xsl:value-of select="$index/@regURL"/>

Types:
input
select
check
how to handle multilevel select?
-->



</xsl:stylesheet>

