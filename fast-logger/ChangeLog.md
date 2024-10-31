## 3.2.5

* Giving names to threads.

## 3.2.4

* Avoid unnecessary copy for Text values with text-2.0
  [#219](https://github.com/kazu-yamamoto/logger/pull/219)

## 3.2.3

* Ensuring flush for single logger.
  [#214](https://github.com/kazu-yamamoto/logger/pull/214)

## 3.2.2

* Corrected handling of messages at the buffer boundary in the SingleLogger
  [#211](https://github.com/kazu-yamamoto/logger/pull/211)

## 3.2.1

* Fixing a bug where a single logger is not killed

## 3.2.0

* newFastLogger1 ensures the ordering of logs
  [#207](https://github.com/kazu-yamamoto/logger/pull/207)

## 3.1.2

* Require unix-compat >= 0.2
  [#206](https://github.com/kazu-yamamoto/logger/pull/206)
* Remove Safe if directory >= 1.3.8
  [#199](https://github.com/kazu-yamamoto/logger/pull/199)

## 3.1.1

* More time-ordered logging functions
  [#199](https://github.com/kazu-yamamoto/logger/pull/199)

## 3.1.0

* Having a single Buffer in LoggerSet for locking [#197](https://github.com/kazu-yamamoto/logger/pull/197.
  This would have performance penalty. So, the major version bumps up. If you see performance regression, please register an issue on github.

## 3.0.5

* recovering backward compatibility for newFileLoggerSet.

## 3.0.4

* New API: `newFastLogger1` which use only one capability.
* Making `FD` safer with `invalidFD`.

## 3.0.3

* Dropping support of GHC 7.x.
* Add `ToLogStr` instance for `ShortByteString`. Add lower bound on
  `bytestring` dependency to ensure that `bytestring` exports
  `Data.ByteString.Short`.

## 3.0.2

* Fixing documentation.

## 3.0.1

* Creating the `Internal` module.
  [#185](https://github.com/kazu-yamamoto/logger/pull/185)

## 3.0.0

* Allowing the callback logger to be generic. [#182](https://github.com/kazu-yamamoto/logger/pull/180) This is a BREAKING CHANGE. Users should do:
  1. Importing `LogType'` and related constructors because `LogType` is now a type alias.
  2. Using `{-# LANGUAGE GADTs #-}`, even if you aren't using anything new, any time you try and `case` over values of type `LogType'`.

## 2.4.17

* Obtaining a fresh fd from IORef just before writing. [#180](https://github.com/kazu-yamamoto/logger/pull/180)

## 2.4.16

* Using strict language extensions.

## 2.4.15

* Rescuing GHC 7.8.

## 2.4.14

* Add `ToLogStr` instances for the following types: signed integers, unsigned integers, floating-point numbers. These instances all use decimal encodings. [#177](https://github.com/kazu-yamamoto/logger/pull/177)

## 2.4.11

* Give an explicit definition for (<>) in LogStr's Semigroup instance. [#155](https://github.com/kazu-yamamoto/logger/pull/155)

## 2.4.10

* Fix Windows build on GHC 7.8. [#121](https://github.com/kazu-yamamoto/logger/pull/121)

## 2.4.9

* Fixing build on Windows. [#118](https://github.com/kazu-yamamoto/logger/pull/118)

## 2.4.8

* Add Semigroup instance to LogStr [#115](https://github.com/kazu-yamamoto/logger/pull/115)
* Added note on log message ordering [#116](https://github.com/kazu-yamamoto/logger/pull/116)

## 2.4.7

* Fixing interleaved log output when messages are larger than buffer size. [#103](https://github.com/kazu-yamamoto/logger/pull/103)

## 2.4.6

* Ensuring that stdio is flushed. [#92](https://github.com/kazu-yamamoto/logger/pull/92)

## 2.4.5

* Bringing backward compatibility back.

## 2.4.4

* New API: newFastLogger and newTimedFastLogger.
* LogType and date cache are transferred from wai-logger.

## 2.4.3

* Opening files in the append mode on Windows.

## 2.4.2

* Fixing a buf of long log messages [#80](https://github.com/kazu-yamamoto/logger/pull/80)
* Log rotation support for Windows [#79](https://github.com/kazu-yamamoto/logger/pull/79)
* Unsupporting GHC 7.4.

## 2.4.1

* Restore compatibility with bytestring < 0.10
* Mark fast-logger modules as Safe/Trustworth [#68](https://github.com/kazu-yamamoto/logger/pull/68)

## 2.4.0

* Providing pushLogStrLn. [#64](https://github.com/kazu-yamamoto/logger/pull/64)

## 2.3.1

* No changes.

## 2.3.0

* Move from blaze-builder to `Data.ByteString.Builder` [#55](https://github.com/kazu-yamamoto/logger/pull/55)
