use ExtUtils::MakeMaker;

use strict;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Warsync',
    'VERSION_FROM' => 'lib/Warsync.pm', # finds $VERSION
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
    'EXE_FILES'	=> [qw(warsync warsync-add-client warsync-client-push
                       warsync-config warsync-server)],
    'PREREQ_PM' => {
        'Getopt::Long'   => 0,
        'Pod::Usage'     => 0,
        'Carp'           => 0,
    },
    'DISTNAME' => 'warsync',

);

