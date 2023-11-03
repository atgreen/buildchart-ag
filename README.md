# buildchart-ag
Render interesting charts from CI/CD and infrastructure logs

Usage
-----

`buildchart-ag` is a command line tool for generating SVG images from
log files.  Currently, it expects Jenkins console log file output in
HTML form (saved directly from the Jenkins web UI).  The HTML logs are
required in order to properly thread parallel task output.  You can
also feed it kubernetes event logs, and buildchart-ag will attempt to
correlate the events with the logs visually.  Generate the event logs
like so: `kubectl get event -n NAMEPACE -o json`.

By default, `buildchart-ag` will not render any tasks that take less
than 3 seconds.  You can change this with the `--filter` option.
Using `--filter 0` will have `buildchart-ag` include everything.

Here is the default CLI output:
```
buildchart-ag - copyright (C) 2023 Anthony Green <green@redhat.com>

Usage: buildchart-ag [-v|--verbose] [-s|--scale SCALE] [-t|--timestamps]
                     [-f|--filter FILTER] logs-file [log-file]*

Available options:
  -v, --verbose            produce verbose output
  -s, --scale SCALE        scale multiplier (default: 10)
  -t, --timestamps         show timestamps
  -f, --filter FILTER      filter seconds (default: 3)

Distributed under the terms of the MIT license.
```

Requirements
------------
buildchart has been tested with [`sbcl`](https://www.sbcl.org/) and
uses [`ocicl`](https://github.com/ocicl/ocicl) for dependency
management.  Run `ocicl install` to download dependencies, and `make`
to build.

Sample Output
-------------

![alt text](./test/example.svg)

Author
------

`buildchart-ag` was written by Anthony Green.

* email    : green@redhat.com
* linkedin : http://linkedin.com/in/green
* twitter  : [@antgreen](https://twitter.com/antgreen)
