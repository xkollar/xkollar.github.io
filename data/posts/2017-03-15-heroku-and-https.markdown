---
title: Heroku and HTTPS
author: xkollar
tags: Haskell
---

[Heroku](https://www.heroku.com/) is a [Platform as a Service][wiki:PaaS] provider
we have used in work for prototyping some small services. What bugged me though was
that even though they provide you with free HTTPS certificate for your `*.herokuapp.com`
site, the they do not provide simple way to enforce HTTPS.

So I have wrote simple [wai][hackage:wai] [Middleware][hackage:wai:Middleware],
which will take care of redirecting users to secured version of your site and
also set [HTTP\_Strict\_Transport\_Security][wiki:HSTS].

``` haskell
herokuHttps :: Middleware
herokuHttps = strictTransportPolicy . herokuHttpsRedirect

isHerokuHttps :: Request -> Bool
isHerokuHttps = (Just "https" ==) . fmap CI.mk
    . lookup "X-Forwarded-Proto" . requestHeaders

strictTransportPolicy :: Middleware
strictTransportPolicy = ifRequest isHerokuHttps . modifyResponse
    $ mapResponseHeaders ((header, value):)
  where
    header = "Strict-Transport-Security"
    value = "max-age=31536000; includeSubDomains"

herokuHttpsRedirect :: Middleware
#if MIN_VERSION_wai(3,0,0)
herokuHttpsRedirect app r respond
#else
herokuHttpsRedirect app r
#endif
    | isHerokuHttps r = runApp
    | Just host <- hostHeader r = res . redir
        $ "https://" <> host <> rawPathInfo r <> rawQueryString r
    | otherwise = runApp
  where

    -- Heroku does not use "X-Forwarded-Host", as it simply preserves "Host"
    hostHeader :: Request -> Maybe ByteString
    hostHeader = lookup "Host" . requestHeaders

    redir :: ByteString -> Response
    redir url = responseLBS HTTP.status301 [("Location", url)] ""

#if MIN_VERSION_wai(3,0,0)
    res = respond
    runApp = app r respond
#else
    res = return
    runApp = app r
#endif
```

Many relevant Haskell libraries use wai ([wai reverse
dependencies][wai-reverse] so you can use it for example with your
[servant][hackage:servant]-based service.

PS: You really do not need this should you have proxy configuration under your
control, as all major reverse proxies can do this for you.

[wiki:PaaS]: https://en.wikipedia.org/wiki/Platform_as_a_service
[hackage:wai]: https://hackage.haskell.org/package/wai
[hackage:wai:Middleware]: https://hackage.haskell.org/package/wai-3.2.1.1/docs/Network-Wai.html#t:Middleware
[wiki:HSTS]: https://en.wikipedia.org/wiki/HTTP_Strict_Transport_Security
[wai-reverse]: http://packdeps.haskellers.com/reverse/wai
[hackage:servant]: https://hackage.haskell.org/package/servant-server
