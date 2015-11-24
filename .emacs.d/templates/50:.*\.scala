// Copyright (C) `(format-time-string "%Y")` `yatemplate-owner`
// License: `yatemplate-license`
`(let ((package (mvn-package-for-buffer)))
  (when (and package (not (string= "" package)))
     (concat "package " package)))`

import Predef.{ any2stringadd => _ }

$0