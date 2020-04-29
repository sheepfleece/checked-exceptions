
{- |
A quick motivating example:

> {-# LANGUAGE DataKinds                  #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE RebindableSyntax           #-}
> {-# LANGUAGE StandaloneDeriving         #-}
> {-# LANGUAGE TypeApplications           #-}
> {-# LANGUAGE TypeOperators              #-}
>
> -- Our own exceptions
> data ExampleException = Example1
>     | Example2
>     deriving (Show, Typeable)
>
> data LovelyException = Heartbroken
>     deriving (Show, Typeable)
>
> instance Exception ExampleException
> instance Exception LovelyException
>
> -- Handlers
> hArith :: Handler ArithException ()
> hArith = Handler (putStrLn . show)
>
> hArray :: Handler ArrayException ()
> hArray = Handler (putStrLn . show)
>
> hLovely :: Handler LovelyException ()
> hLovely = Handler (putStrLn . show)
>
> hExample :: Handler ExampleException ()
> hExample = Handler (putStrLn . show)
>
> -- Type synonyms, so you shouldn't write the same exceptions all the time
> type SillyExceptions = '[ ArithException, ArrayException ]
>
> -- A type can be inferred
> action :: Throws (SillyExceptions :++: '[ LovelyException, ExampleException ]) IO ()
> action = do
>   throws @ManyExceptions externalAction
>   throw Heartbroken
>   throw Example1
>   throw Example2
>   where
>     (>>) = (>!) -- @Throws | Type application would be needed though.
>
>
> externalAction :: IO ()
> externalAction = do
>   putStrLn "I'm quite a naughty function"
>   putStrLn "Shouldn't you be scared using me?"
>   E.throw ArithException
> -- It is a type error to not handle every exception.
> -- Though we can use `ignore` to drop handling of exceptions
> -- we do not care about.
> safety :: IO ()
> safety = catches
>           (ignore @'[ArrayException] action)
>           (hArith :&&: hLovely :&&: hExample :&&: Nil)

-}
