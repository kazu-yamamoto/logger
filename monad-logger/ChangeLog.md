## 0.3.10.1

Remove unnecessary extra newline in log messages.

## 0.3.10

Introduce the `MonadLoggerIO` typeclass.

## 0.3.9

Add missing `MonadError NoLoggingT` instance #49

## 0.3.8

Simplify constraint on `MonadLogger (NoLoggingT m)` from `MonadIO m` to `Monad m` [Github issue #48](https://github.com/kazu-yamamoto/logger/issues/48).
