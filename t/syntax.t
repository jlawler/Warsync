## test that modules load.
use strict;
use Test::More tests => 8;

## warsync-config
BEGIN { use_ok('Warsync::Util'); }

## warsync-add-client
BEGIN { use_ok('Warsync::Server'); }

## warsync
BEGIN { use_ok('Warsync::Client'); }


ok(!system('perl -cTIblib/lib blib/script/warsync 2>&1'), 'warsync');
ok(!system('perl -cTIblib/lib blib/script/warsync-config 2>&1'), 'warsync-config');
ok(!system('perl -cTIblib/lib blib/script/warsync-add-client 2>&1'), 'warsync-add-client');
ok(!system('perl -cTIblib/lib blib/script/warsync-client-push 2>&1'), 'warsync-client-push');
ok(!system('perl -cTIblib/lib blib/script/warsync-server 2>&1'), 'warsync-server');


