# ChangeLog for wai-logger

## 2.5.1

* Security: request-derived fields (IP from headers, user, method,
  path, Referer, User-Agent) are now escaped like Apache's
  `ap_escape_logitem` to prevent log injection.
* The IP taken from X-Real-IP or X-Forwarded-For is now the first
  token only.
