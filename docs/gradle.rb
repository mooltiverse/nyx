#!/bin/sh

## Hack our GEM_HOME to make sure that the `rubygems` support can find our
# jars and unpacked gems in the given GEMFOLDER
export GEM_HOME="C:\Development\m\nyx\docs\build\.gems"
export GEM_PATH="C:\Development\m\nyx\docs\build\.gems"
export JARS_HOME=$GEM_HOME/jars
export JARS_LOCK=$GEM_HOME/Jars.lock

exec java -cp C:\Users\flelli\.gradle\caches\modules-2\files-2.1\org.jruby\jruby-complete\9.2.9.0\8e44ce7a1417966d89957bd766a148601e28828b\jruby-complete-9.2.9.0.jar org.jruby.Main -rjars/setup -S $@

