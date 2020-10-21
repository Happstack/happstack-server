7.7.0
=====

 - added support for specifying SameSite attribute

7.6.0
=====

 - updated to support network 3.* and GHC 8.8.1

 - removed Happstack.Server.Client and Happstack.Server.Proxy under the
   belief that no one uses them. This makes the upgrade to network 3
   easier. If you used these, let us know.

7.5.1
=====

 - anyone using base < 4.10 should upgrade to 7.5.1 or higher
 - disallow path separator in filename for POST data (reported by Hamid Ebadi)

7.4.6
=====

 - Allow transformers 0.5.* and transformers-compat 0.5.*

