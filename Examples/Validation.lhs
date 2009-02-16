> module Validation where

> import Control.Concurrent
> import Happstack.Server
> import Text.XHtml hiding (dir,method)

HAppS-Server has support for validating output on-the-fly. There are a
few different ways you can use this system, depending on what your
needs are.

QuickStart
----------

The easiest option is to just use validateConf instead of nullConf:

> ex1 = simpleHTTP validateConf [ dir "valid"   [method GET $ ok (toResponse validPage)]
>                               , dir "invalid" [method GET $ ok (toResponse invalidPage)]
>                               , anyRequest $ seeOther "/valid" (toResponse ())
>                               ]

You will need WDG HTML Validator installed. It must be named
'validate' and it must be in the default PATH. On Debian systems you
can just do: 

  apt-get install wdg-html-validator.

(NOTE: You must restart GHCi between runs because simpleHTTP forks
off threads which don't get stopped when you hit ^C. These threads
will contain to serve the old page instead of the new page.)

The error page looks something like this:

---->
ExitCode: ExitFailure 1
stdout:
Checking with XHTML 1.0 Transitional document type...
*** Errors and warnings: ***
-:5:3:E: end tag for "head" which is not finished

stderr:

input:
           1         2         3         4         5         6         7         8         9         0         1         2
1 <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
2 <html xmlns="http://www.w3.org/1999/xhtml"
3 ><head
4   ></head
5   ><body
6   >Hello, World!</body
7   ></html
8 >
<--

How The System Works
--------------------

 Goals:

  1. provide an easy way to have validation automatically enabled with no extra work from the developer
  2. provide an easy way to disable validation for live sites
  3. provide an easy way to selectively enable and disable validation for specific pages
  4. privade an easy way to validate different content-types with different validators
  5. provide an easy way to add new validators


 The solution involves two pieces working together:

  1. the Conf datatype is extended to include an option for enabling validation and
     providing a default validator:

     data Conf 
        = Conf { ...
               , validator  :: Maybe (Response -> IO Response)
               , ...
               }

  2. the Response datatype is also extended to include a validator
     field:

     data Response  
        = Response { ...
                   , rsValidator:: Maybe (Response -> IO Response)
                   , ...
                   }


 To enable validation, we must supply a default validator in the Conf
 we pass to simpleHTTP. If validator is Nothing, no validation will occur.

 The developer can just use validateConf instead of nullConf, and all
 of their HTML pages will be automatically validated.

 It is also easy to disable validation before making the site go
 live. You can either provide a command-line flag which sets the
 validator option in Conf to Nothing, or you can use CPP or Template
 Haskell to do it at compile time.

 Specifying a Specific Validator for a Specific Page
 ---------------------------------------------------

 Setting the validator to use for a particular Response is done using
 the 'setValidator' function. In this example, we enable validation,
 but then we turn it off for the invalidPage. (Do not forget to
 restart GHCi before running this example):

> ex2 = simpleHTTP validateConf [ dir "valid"   [method GET $ ok (toResponse validPage)]
>                               , dir "invalid" [method GET $ ok (setValidator noopValidator (toResponse invalidPage))]
>                               , anyRequest $ seeOther "/valid" (toResponse ())
>                               ]


 We can use setValidateSP to set the validator at the ServerPart
 level. This is useful if you have a whole subdirectory you wish to
 change the validator for.

> ex3 = simpleHTTP validateConf [setValidatorSP noopValidator $ multi
>                                  [ dir "valid"   [method GET $ ok (toResponse validPage)]
>                                  , dir "invalid" [method GET $ ok (toResponse invalidPage)]
>                                  , anyRequest $ seeOther "/valid" (toResponse ())
>                                  ]]

 Instead of validating all pages by default, and selectively disabling
 validation on some, we could enable validation, but only validate a
 few pages.

 We set the default validator to noopValidator, and then explicitly
 mark the pages we do want validated. In this example, /invalid will
 fail, but /invalid2 pass.

> ex4 = simpleHTTP (nullConf { validator = Just noopValidator})
>          [ dir "valid"    [method GET $ ok (toResponse validPage)]
>          , dir "invalid"  [method GET $ ok (setValidator wdgHTMLValidator (toResponse invalidPage))]
>          , dir "invalid2" [method GET $ ok (toResponse invalidPage)]
>          , anyRequest $ seeOther "/valid" (toResponse ())
>          ]

 Per content-type validation
 ---------------------------

 Each validator should only attempt to validate content-types it
 understands, passing all other content-types through
 unmodified. Because the validator type is:

   Response -> IO Response

 This means we can simply chain validators together using >>=. 

> ex5 = simpleHTTP (nullConf { validator = Just $ \r -> noopValidator r >>= wdgHTMLValidator })
>       [ dir "valid"   [method GET $ ok (toResponse validPage)   ]
>       , dir "invalid" [method GET $ ok (toResponse invalidPage) ]
>       , anyRequest $ seeOther "/valid" (toResponse ())
>       ]

 There is one caveat. Because we use >>= to chain the validators
 together, error messages produced by earlier validators will be
 validated by later validators. This is, perhaps, a good
 thing, as it can be used to detect errors in your error messages.

 Adding new validators
 ---------------------

 Adding a new validator is fairly straight forward:

  1. it must have the type signature, Response -> IO Response

  2. it should do, (getHeader "content-type" response), and only
     attempt to validate content-types it understands, passing all
     other types through unmodified.

  3. it should leave the Response unmodified on success, or return a
     new Response with the error on the failure.

 If you intend to use an external program for validation, the easiest
 method is to use lazyProcValidator. For example, wdgHTMLValidator is
 essentially:

 wdgHTMLValidator :: Response -> IO Response
 wdgHTMLValidator = lazyProcValidator "validate" ["-w","--verbose"] Nothing Nothing handledContentTypes
    where
      handledContentTypes (Just ct) = elem (B.unpack ct) [ "text/html", "application/xhtml+xml" ]
      handledContentTypes Nothing = False

 Sample Pages
 ------------

 These are just for use in the above examples.
 
> validPage :: Html
> validPage =
>          ((header <<
>                thetitle (toHtml "Hello, World!")
>           ) +++
>          (body <<
>                (p << toHtml "Hello, World!" +++
>                 p << anchor ! [href "/invalid"] << (toHtml "invalid")
>                )
>           ))

> invalidPage :: Html
> invalidPage =
>          ((header << noHtml
>           ) +++
>          (body <<
>                toHtml "Hello, World!" +++
>                p << anchor ! [href "/valid"] << (toHtml "valid")
>           ))
