#!/bin/sh
acme -v wiz500.b
diff -s wiz500.rom original/wizard.rom
cmp wiz500.rom original/wizard.rom