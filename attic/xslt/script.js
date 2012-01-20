//if(frames){if(top.frames.length>0)
//top.location.href=self.location;}

function visi(nr)
{
	if (document.getElementById)
	{
	    startVal=document.getElementById(nr).style.visibility
		vista = (startVal == 'hidden' || startVal=='') ? 'visible' : 'hidden';
		//el=document.getElementById(nr).style
		document.getElementById(nr).style.visibility = vista;
		alert(startVal+" "+nr)

	}
	else if (document.layers)
	{
	    el=document.layers[nr]
		vista = (document.layers[nr].visibility == 'hide') ? 'show' : 'hide'
		document.layers[nr].visibility = vista;
	}
	else if (document.all)
	{
	    el=document.all[nr].style
		vista = (document.all[nr].style.visibility == 'hidden') ? 'visible'	: 'hidden';
		document.all[nr].style.visibility = vista;
	}

	//el.display=el.display=='none'?'block':'none';
}

function getStyle(nr) {
	return (document.layers?document.layers[nr]:document.all?document.all[nr].style:document.getElementById(nr).style);
}

function setBlocking(nr,b){
	style=getStyle(nr);
	//	style=document.layers?document.layers[nr]:document.all?document.all[nr].style:document.getElementById(nr).style;
	style.display=b?'block':'none';
	//alert(nr+" "+b+" "+style.display);

}
function blocking(nr)
{
	style=document.layers?document.layers[nr]:document.all?document.all[nr].style:document.getElementById(nr).style;

	if (style.display!='none' && style.display!='block') 
	{ style.display='none';}
	if (style.display=='none') {style.display='block'; 	}
	else {style.display='none';}

	return;


	if (document.layers)
	{
		current = (document.layers[nr].display == 'none') ? 'block' : 'none';
		document.layers[nr].display = current;
	}
	else if (document.all)
	{
		current = (document.all[nr].style.display == 'none') ? 'block' : 'none';
		document.all[nr].style.display = current;
	}
	else if (document.getElementById)
	{
		current = (document.getElementById(nr).style.display == 'none') ? 'block' : 'none';
		document.getElementById(nr).style.display = current;
	}
}

function getStyle(nr)
{
   if (document.getElementById) {return document.getElementById(nr).style}
   if (document.all) {return document.all[nr].style;}
   if (document.layers) {return document.layers[nr];}


}
function blockingOld(nr) { 
   style=getStyle(nr)
   current=style.display
   getStyle(nr).display=(current=='none')?'block':'none';
}


//-->

//--cookie stuff
var expDays = 1; // number of days the cookie should last

var page = "only-popup-once.html";
var windowprops = "width=300,height=200,location=no,toolbar=no,menubar=no,scrollbars=no,resizable=yes";

function GetCookie (name) {  
var arg = name + "=";  
var alen = arg.length;  
var clen = document.cookie.length;  
var i = 0;  
while (i < clen) {    
var j = i + alen;    
if (document.cookie.substring(i, j) == arg)      
return getCookieVal (j);    
i = document.cookie.indexOf(" ", i) + 1;    
if (i == 0) break;   
}  
return null;
}
function SetCookie (name, value) {  
var argv = SetCookie.arguments;  
var argc = SetCookie.arguments.length;  
var expires = (argc > 2) ? argv[2] : null;  
var path = (argc > 3) ? argv[3] : null;  
var domain = (argc > 4) ? argv[4] : null;  
var secure = (argc > 5) ? argv[5] : false;  
document.cookie = name + "=" + escape (value) + 
((expires == null) ? "" : ("; expires=" + expires.toGMTString())) + 
((path == null) ? "" : ("; path=" + path)) +  
((domain == null) ? "" : ("; domain=" + domain)) +    
((secure == true) ? "; secure" : "");
}
function DeleteCookie (name) {  
var exp = new Date();  
exp.setTime (exp.getTime() - 1);  
var cval = GetCookie (name);  
document.cookie = name + "=" + cval + "; expires=" + exp.toGMTString();
}
var exp = new Date(); 
exp.setTime(exp.getTime() + (expDays*24*60*60*1000));
function amt(){
var count = GetCookie('count')
if(count == null) {
SetCookie('count','1')
return 1
}
else {
var newcount = parseInt(count) + 1;
DeleteCookie('count')
SetCookie('count',newcount,exp)
return count
   }
}
function getCookieVal(offset) {
var endstr = document.cookie.indexOf (";", offset);
if (endstr == -1)
endstr = document.cookie.length;
return unescape(document.cookie.substring(offset, endstr));
}

function checkCount() {
var count = GetCookie('count');
if (count == null) {
count=1;
SetCookie('count', count, exp);

window.open(page, "", windowprops);

}
else {
count++;
SetCookie('count', count, exp);
   }
}


//  End -->


function toggleAdvancedConfig()
{
	visi('advancedConfig');
}


