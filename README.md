# servant-examples

This repo hosts a bunch of modules that show how to do anything using Haskell's Servant web framework.
The modules here are

1. Self contained. Each module exports a complete servant web application. You can just load the indvidual modules 
using GHCI and call the 'mainFn' function to start the server.

2. Consise. Each module only contain stuff that is absolutly required to show one and only one thing.

3. Each module also contains a sample output from a curl command that communicates with the server in that module
that demostrates the respective behavior.

This is only meant as a quick refresher on how to do anything using Servant, and not an in depth tutorial on
its workings. Following is a listing of the modules and a short summary that describes each 

1. HelloWorld.hs -- This is a minimal Servant server with two GET endpoints that respond in plain text.
2. HtmlContent.h -- This module shows how you can send HTML content with an "text/html" content type header.
3. JsonOutput.hs -- Shows how to output Json.
4. CustomOutput.hs -- Shows how to output data in a user defined format.
5. PostData.hs -- Shows how an endpoint can accept json content in request body.
6. CustomPostData.hs -- Shows how an endpoint can accept user defined data in a json format in request body.
7. CustomPostFormat.hs -- Shows how an endpoint can accept data in request body in a user defined format.
8. CustomPostMultipleFormats.hs -- Shows how the same endpoint can accept data in more than one format.
9. UrlParams.hs -- Shows how to accept query params in url.
10. TypedUrlParams.hs -- Shows how to accept values of custom types via url parameters.
11. AnotherMonad.hs -- Shows how to make your handlers run in a custom monad.
12. BasicAuthentication.hs -- Shows how to protect your endpoints using HTTP Basic Authentication.
13. GeneralAuthentication.hs -- Shows how to protect your endpoints using custom authentication method with authentication data derived from the wai Request.
14. ErrorHandling.hs -- Show how to throw errors from Servant handlers.
15. HeaderInput.hs -- Shows how to get values from HTTP headers in your handler functions.
16. HeaderOutput.hs -- Shows how  to add HTTP headers to your response from your handlers.
