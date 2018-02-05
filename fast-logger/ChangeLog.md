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
