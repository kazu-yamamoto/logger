## 0.3.18

* Added logTHShow and logDebugSH, logInfoSH, etc. Accepts an argument of `Show a => a` instead of just `Text`.

## 0.3.17

* log to a chan [#74](https://github.com/kazu-yamamoto/logger/pull/74)

## 0.3.16

* Provide default monadLoggerLog implementation [#72](https://github.com/kazu-yamamoto/logger/pull/72)

## 0.3.15

* Expose Loc constructor [#70](https://github.com/kazu-yamamoto/logger/pull/70)

## 0.3.14

* Don't include source location for defaultLoc [#69](https://github.com/kazu-yamamoto/logger/issues/69)

## 0.3.13.1

* Allow fast-logger 2.3

## 0.3.13

* Re-export LogStr from fast-logger [#56](https://github.com/kazu-yamamoto/logger/pull/56)
* Added `filterLogger`

## 0.3.12

* Use transformers-compat to provide universal ExceptT support [#53](https://github.com/kazu-yamamoto/logger/pull/53)

## 0.3.11.1

Add support for monad-control 1.0 [#52](https://github.com/kazu-yamamoto/logger/pull/52)

## 0.3.11

Add missing `MonadState` and `MonadWriter` instances for `NoLoggingT` [#51](https://github.com/kazu-yamamoto/logger/pull/51)

## 0.3.10.1

Remove unnecessary extra newline in log messages.

## 0.3.10

Introduce the `MonadLoggerIO` typeclass.

## 0.3.9

Add missing `MonadError NoLoggingT` instance #49

## 0.3.8

Simplify constraint on `MonadLogger (NoLoggingT m)` from `MonadIO m` to `Monad m` [Github issue #48](https://github.com/kazu-yamamoto/logger/issues/48).
