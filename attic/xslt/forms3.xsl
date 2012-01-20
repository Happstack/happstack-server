<!DOCTYPE xsl:stylesheet PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xslt2.dtd">
<xsl:stylesheet 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
 xmlns:html='http://www.w3.org/TR/REC-html40'
 >
  
<xsl:template match="html:form" >
  <form id="{@id}"  action="{@action}" method="{@method}" >
	<xsl:apply-templates select="*" />
  </form>
</xsl:template>

<xsl:template match="input" >
  <xsl:apply-templates select="." mode="inputBefore"/>
  <xsl:variable name="name" select="@name"/>
  <xsl:variable name="form" select="ancestor::form/@name"/>
  <xsl:variable name="info" 
				select="$formDict/locale/form[@name=$form]/field[@name=$name]/*[name()=$locale]"/>
  <!--select="$formDict/locale/*[name()=$name]/*[name()=$locale]"/>-->
  <div class="row">
	<span class="formLabel">
	  <xsl:choose>
		<xsl:when test="count($info/*)!=0">
		  <xsl:copy-of select="$info/n/*|$info/n/text()"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="@name"/>
		</xsl:otherwise>
	  </xsl:choose>
	</span>
	<span class="formEl">
	  <xsl:apply-templates select="." mode="formEl">
		<xsl:with-param name="info" select="$info"/>
	  </xsl:apply-templates>
	</span>
	<span class="instructEl">
	  <xsl:choose>
		<xsl:when test="$info/note!=''">
		  (<xsl:copy-of select="$info/note"/>)
		</xsl:when>
		<xsl:when test="count($info/*)=0">
		  (<xsl:value-of select="@type"/>
		  <xsl:apply-templates select="option" mode="instruct"/>
		  )
		</xsl:when>
		<xsl:otherwise/>
	  </xsl:choose>
	</span>
  </div>
  <xsl:apply-templates select="." mode="fieldAfter"/>
</xsl:template>




</xsl:stylesheet>