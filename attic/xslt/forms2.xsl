<!DOCTYPE xsl:stylesheet PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xslt2.dtd">
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<!--generic forms implementation -->


<xsl:template match="form" >
  <form id="{@id}"  action="{@action}" method="{@method}" >
	<xsl:apply-templates select="*" />
  </form>
</xsl:template>


<xsl:template match="field[@type='Submit']"  >
  <div class="row">
	<span class="formLabel"/>
	<span class="formEl">
	  <xsl:variable name="form" select="ancestor::form/@name"/>
	  <xsl:variable name="name" select="@name"/>
	  <xsl:variable name="info" 
					select="$formDict/locale/form[@name=$form]/field[@name=$name]/*[name()=$locale]"/>
	  <xsl:variable name="value">
		<xsl:choose>
		  <xsl:when test="count($info)=0"><xsl:value-of select="@name"/></xsl:when>
		  <xsl:otherwise><xsl:value-of select="$info/n/text()"/></xsl:otherwise>
		</xsl:choose>
	  </xsl:variable>
	  <input type="submit" name="{@name}" id="{ancestor::form/@id}{@name}" 
			 value="{$value}" />
<!--$info/n/text()-->
	</span>
  </div>
</xsl:template>



<xsl:template match="field" >
  <xsl:apply-templates select="." mode="fieldBefore"/>
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

<xsl:template match="option" mode="instruct">
  [<xsl:value-of select="@value"/>]
</xsl:template>

<xsl:template match="field" mode="formEl" >
  <xsl:param name="info"/>
  <input type="text" name="{@name}" id="{ancestor::form/@id}{@name}" 
		 value="{@value}"/>
</xsl:template>


<xsl:template match="field[@type='Name']" mode="formEl" >
  <xsl:param name="info"/>
  <input type="text" name="{@name}" id="{ancestor::form/@id}{@name}" 
		 value="{@value}"/>
</xsl:template>

<xsl:template match="field[@type='Phrase']" mode="formEl" >
  <xsl:param name="info"/>
  <input type="text" name="{@name}" id="{ancestor::form/@id}{@name}" 
		 value="{@value}"/>
</xsl:template>

<xsl:template match="field[@type='Word']" mode="formEl" >
  <xsl:param name="info"/>
  <input type="text" name="{@name}" id="{ancestor::form/@id}{@name}" 
		 value="{@value}" size="10"/>
</xsl:template>

<!--
<xsl:template match="field[@type='Select']" mode="formEl">
  <xsl:param name="info"/>
  <xsl:choose>
	<xsl:when test="count(option)&lt;4">
	  <xsl:apply-templates select="option" mode="formElRadio"/>
	</xsl:when>
	<xsl:when test="count($info/option)&lt;4">
	  <xsl:apply-templates select="option" mode="formElRadio"/>
	</xsl:when>
	<xsl:otherwise>
	</xsl:otherwise>
  </xsl:choose>
</xsl:template>
-->
<xsl:template match="field[@type='Bool']" mode="formEl">
  <xsl:param name="info"/>
  <xsl:variable name="opt1Info">
	<xsl:copy-of select="$info/option[1]/*|$info/option[1]/text()"/>
  </xsl:variable>
  <label><xsl:value-of select="$opt1Info"/>
	<input type="radio" name="{@name}" id="{ancestor::form/@id}{@name}_1" 
		 value="{$info/option[1]/@value}" />
  </label>
  <label><xsl:copy-of select="$info/option[2]/*|$info/option[2]/text()"/>
	<input type="radio" name="{@name}" id="{ancestor::form/@id}{@name}_2" 
		 value="{$info/option[2]/@value}" />
  </label>
</xsl:template>


<xsl:template match="field[@type='Email']" mode="formEl">
  <input type="text" name="{@name}" id="{ancestor::form/@id}{@name}" 
		 value="{@value}" size="30"/>
  <!--
	add javascript here later to validate email format!
	need interface for javascript events?
	or just put javascript inline that associates actions?
  -->
</xsl:template>

<xsl:template match="field[@type='Password']" mode="formEl">
  <input type="password" name="{@name}" id="{ancestor::form/@id}{@name}" 
		 value="{@value}" size="10"/>
</xsl:template>

<xsl:template match="field[@type='URL']" mode="formEl">
  <input type="text" name="{@name}" 
		 id="{ancestor::form/@id}{@name}" 
		 value="{@value}" size="30"/>
</xsl:template>

<xsl:template match="field[@type='Date']" mode="formEl">
  <input type="text" name="{@name}" 
		 id="{ancestor::form/@id}{@name}" 
		 value="{@value}" size="10" maxlength="10"/>
</xsl:template>

<xsl:template match="field[@type='Country']" mode="formEl">
  <xsl:param name="info"/>
  <select name="{@name}" id="{ancestor::form/@id}{@name}">
	<xsl:copy-of select="$info/option"/>
  </select>
</xsl:template>


<xsl:template match="@*" name="info">
  <xsl:variable name="div" select="ancestor::div/@name"/>
  <xsl:variable name="name" select="name()"/>
  <xsl:variable name="label" 
				select="$formDict/locale/div[@name=$div]/attr[@name=$name]/*[name()=$locale]"/>
  <div  class="row">
	<span style="font-weight:bold;" class="formLabel">
	  <xsl:choose>
		<xsl:when test="not($label)">
		  <xsl:value-of select="name()"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:copy-of select="$label"/>
		</xsl:otherwise>
	  </xsl:choose>
	</span>
	<span class="formEl"><xsl:value-of select="."/>
	</span>
  </div>  
</xsl:template>



</xsl:stylesheet>