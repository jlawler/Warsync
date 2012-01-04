package Warsync::Host;

##############################################################################
## Warsync::Host - Warsync                                                  ##
##                                                                          ##
## Copyright (C) 2001-2006 Paul J. Baker                                    ##
##                                                                          ##
## This program is free software; you can redistribute it and/or modify     ##
## it under the terms of the GNU General Public License version 2 as        ##
## published by the Free Software Foundation.                               ##
##                                                                          ##
## This program is distributed in the hope that it will be useful,          ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of           ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            ##
## GNU General Public License for more details.                             ##
##                                                                          ##
## You should have received a copy of the GNU General Public License        ##
## along with this program; if not, write to the Free Software              ##
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                ##
##                                                                          ##
## You may contact the author via electronic mail at warsync@paulbaker.net  ##
##############################################################################

## $Id: Host.pm 174 2006-09-20 22:39:21Z pbaker-guest $

=head1 NAME

Warsync - Wrapper Around Rsync

=head1 SYNOPSIS

  package Warsync::Foo;

  use Warsync::Host;

  our @ISA = qw( Warsync::Host );

=head1 DESCRIPTION

Common methods for Warsync hosts.

=head1 AUTHOR

Paul Baker, pbaker@where2getit.com

=head1 SEE ALSO

warsync(8).

=cut

#############################################################################

require 5.6.0;
use strict;
use warnings;

use Carp;
use Data::Dumper;
use Getopt::Long ();
use Pod::Usage;
use Sys::Hostname;
use POSIX 'strftime';

use Warsync::Util;

require Exporter;

our @ISA = qw( Exporter );

sub dryrun
{
    my $self = shift->new;

    return $self->option('n') || 0;
}

sub option
{
    my $self = shift->new;
    my $key  = shift;

    $self->options->{$key};
}

sub options
{
    my $self = shift->new;

    if (@_) {
        $self->{options} = shift;
        return $self;
    }

    return $self->{options};
}

sub display
{
    my $self  = shift->new;

    ## make sure they gave us at least two arguments.
    croak 'usage: $OBJECT->display(LEVEL, LIST)' unless @_ > 1;

    ## check for timestamp.
    my $time = \time;
    if (ref($_[0]) eq 'SCALAR') {
        $time = shift;
    }

    if (my $pat = $self->timestamp) {
        print STDERR strftime($pat, localtime $$time),' ', @_ if $self->verbose >= shift;
    }
    else {
        print STDERR @_ if $self->verbose >= shift;
    }
}

sub packages
{
    my $self = shift->new;

    $self->{_packages} ||= $self->debian_packages;

    return deep_copy $self->{_packages};
}

sub parse_cli_params
{
    my $self = shift->new;

    my %option = ();

    ## set options for host object.
    $self->options(\%option);

    ## set option defaults.
    Getopt::Long::Configure("bundling");

    Getopt::Long::GetOptions(
        \%option,
        'help|H|?',
        'man',
        'version|V',
        'v|verbose+',
        'q|quiet',
        'n|dry-run',
        't|timestamp',
        @_
    ) or pod2usage(2);

    pod2usage(1) if $option{help};
    pod2usage(-exitstatus => 0, -verbose => 2) if $option{man};

    # display version number if --version set.
    if ($option{version}) {
        $self->display(0, version);
        exit;
    }

    ## always be at least little verbose.
    ++$option{v};
    ## but be quiet if asked.
    $option{v} = 0 if $option{'q'};
    
    ## blow out skip-*-cmds options.
    if ($option{'skip-cmds'}) {
        $option{'skip-mod-cmds'}  = 1;
        $option{'skip-pre-cmds'}  = 1;
        $option{'skip-post-cmds'} = 1;
    }

    return $self;
}

sub name
{
    my $self = shift->new;

    return Sys::Hostname::hostname;
}

sub replication
{
    my $self = shift->new;
    if (tty) {
        $self->display(0, "This is a Warsync client. Warsync replication must be\n");
        $self->display(0, "initiated from the Warsync server only.\n");
        $self->exit_code(1);
    }
    return;
}

sub run
{
    my $self = shift->new;

    $self->parse_cli_params(
        'a|all-filepacks',
        'include-skipped-filepacks|i',
        'client=s@',
        'debian',
        'skip-mod-cmds|m',
        'skip-pre-cmds',
        'skip-post-cmds',
        'skip-cmds',
        'debian-closest-version',
        'debian-ignore=s@',
        'debian-snapshot=s',
        'debian-from-snapshot=s',
        'check-clients',
        'accept-unknown-hosts',
        'client-offset=s',
    );

    ## if --debian-snapshot argument given, do that.
    if ($self->option('debian-snapshot')) {
        $self->debian_snapshot;
        exit($self->exit_code);
    }

    ## if --debian-from-snapshot argument given, do that.
    if ($self->option('debian-from-snapshot')) {
        $self->debian_from_snapshot;
        exit($self->exit_code);
    }

    #print STDERR Data::Dumper::Dumper($self->options);

    $self->replication;
}

sub debian_packages
{
    my $self = shift->new;

    my $v = $self->verbose;

    ## get current package information.
    my $packages;

    {
        my $prev_col = $ENV{COLUMNS};
        ## now set it to 200
        $ENV{COLUMNS} = 200;

        ## read dpkg -l
        open(my $dpkg_h, 'dpkg -l |')
            or die "could not exec dpkg -l: $!";

        $packages = parse_package_list($dpkg_h);

        close($dpkg_h);

        if ($prev_col) {
            $ENV{COLUMNS} = $prev_col;
        }
        else {
            delete $ENV{COLUMNS};
        }
    }

    my $total   = keys(%$packages);
    my $done    = 0;
    my $percent = 0;

    ## iterate through packages and get real version number.
    {
        open(my $fh, "</var/lib/dpkg/status")
            or $self->fault("Could not read /var/lib/dpkg/status: $!");

        my $package = '';
        while (<$fh>) {
            chomp;
            next unless length;

            ## current package block.
            if (my ($pkg) = $_ =~ /^Package:\s+(\S+)/) {
                $package = $pkg;
                next;
            }

            next unless exists $packages->{$package};

            if (my ($ver) = $_ =~ /^Version:\s+(\S+)/) {
                $packages->{$package}[2] = $ver;

                ## only display progress if verbose is on
                ## and we are on a tty.
                if ($v and tty) {
                    my $new = int( (++$done / $total) * 100);
                    if ($new > $percent) {
                        my $str = "$new%";
                        $self->display(1, "$str", "\b" x length($str));
                        $percent = $new;
                    }
                }
            }
        }

        close($fh);
    }

    if ($v and tty) {
        $self->display(1, ' ' x 5, "\b" x 5);
    }

    return $packages;
}

sub debian_snapshot ($)
{
    my $self = shift->new;

    ## open file to write snapshot to.
    my $fn = $self->option('debian-snapshot');

    open(my $fh, ">$fn") or $self->fault("can not write to $fn: $!");

    $self->display(1, "Creating snapshot of Debian package installation...");

    my $packages = $self->debian_packages;

    ## write package information to file.
    foreach my $pkg (sort keys %$packages) {
        my $p = $packages->{$pkg};
        print $fh $p->[0], ' ', $p->[1], ' ',
                  $p->[2], ' ', $p->[3], "\n";
    }

    close($fh);

    $self->display(1, "done.\n");

}

sub debian_package_diff ($$$;$)
{
    my $self  = shift->new;

    my $spkgs = shift;
    my $cpkgs = shift;
    my $ipkgs = shift;

    my @install = ();
    my @remove  = ();
    my @purge   = ();

    my $total   = keys(%$spkgs) + keys(%$cpkgs);
    my $done    = 0;
    my $percent = 0;

    ## compare server packages to the client and create
    ## list of apt-get commands to run on the client.
    foreach my $pkg (keys %$spkgs) {

        ## display progress update.
        if ($self->verbose and tty) {
            my $new = int( ($done++ / $total) * 100);
            if ($new > $percent) {
                my $str = "$new%";
                $self->display(1, "$str", "\b" x length($str));
                $percent = $new;
            }
        }

        ## check if we should ignore this package.
        my $ignore = 0;
        foreach my $regex (@$ipkgs) {
            if ($pkg =~ /$regex/) {
                $ignore = 1;
                last;
            }
        }
        next if $ignore;

        my $spkg = $spkgs->{$pkg};

        ## if installed, make sure client installs it.
        if ($spkg->[0] eq 'ii ') {
            ## client has package installed
            if (exists $cpkgs->{$pkg} and $cpkgs->{$pkg}[0] eq 'ii ') {
                ## check version.
                if ($spkg->[2] ne $cpkgs->{$pkg}[2]) {
                    my $ver = $spkg->[2];
                    push @install, "$pkg=$ver";
                }
            }
            ## client does not have package installed.
            else {
                my $ver = $spkg->[2];
                push @install, "$pkg=$ver";
            }
        }

        ## if removed, make sure client removes it.
        elsif ($spkg->[0] =~ /^[rp]c $/) {
            ## client has package installed
            if (exists $cpkgs->{$pkg} and $cpkgs->{$pkg}[0] !~ /^[rp]c $/) {
                push @remove, $pkg;
            }
        }

        ## unexpected state, possibly flagged for removal,
        ## but not yet uninstalled.
        else {
            $self->display(0, "!! server package $pkg $spkg->[2] ",
                              "has unexpected state: $spkg->[0].\n");
        }

        ## mark package as processed.
        $spkg->[5] = $cpkgs->{$pkg}[5] = 1;

    }

    ## now compare remaining client packages to the server.
    foreach my $pkg (keys %$cpkgs) {

        ## display progress update.
        if ($self->verbose and tty) {
            my $new = int( ($done++ / $total) * 100);
            if ($new > $percent) {
                my $str = "$new%";
                $self->display(1, "$str", "\b" x length($str));
                $percent = $new;
            }
        }

        ## skip if we already processed it.
        next if $cpkgs->{$pkg}[5];

        ## check if we should ignore this package.
        my $ignore = 0;
        foreach my $regex (@$ipkgs) {
            if ($pkg =~ /$regex/) {
                $ignore = 1;
                last;
            }
        }
        next if $ignore;

        push @purge, $pkg;
    }

    return \@install, \@remove, \@purge;
}


## apt_cache_versions($pkg) - get available versions of $pkg using apt-cache

sub apt_cache_versions ($)
{
    my $pkg = shift;

    my @apt = ( 'apt-cache', 'showpkg', $pkg );

    open(my $apt_h, '-|') || exec( @apt )
        or die "error '$!' trying to exec: @apt";

    my $versions = 0;

    my %vers = ();

    while (<$apt_h>) {
        chomp;
        ## iterate through output until we get to Versions: line.
        if ($_ =~ /^Versions:/) {
            $versions = 1;
            next;
        }
        next unless $versions;

        ## we are done soon as we hit blank line after Versions:
        last unless length;

        ## get version and where it's available.
        my ($ver, $list) = $_ =~ m!^([^()]+)\(([^()]+)\)!;

        $vers{$ver} = $list;
    }

    close($apt_h);

    return \%vers;
}

sub debian_closest_version
{
    my $self = shift->new;

    my $install = shift;

    my $total   = @$install;
    my $done    = 0;
    my $percent = 0;

    for (my $i = 0; $i < @$install; ++$i) {
        my $inst = $install->[$i];

        ## display progress update.
        if (tty and $self->verbose) {
            my $new = int( ($done++ / $total) * 100);
            if ($new > $percent) {
                my $str = "$new%";
                $self->display(1, "$str", "\b" x length($str));
                $percent = $new;
            }
        }

        my ($pkg, $ver) = split /=/, $inst, 2;

        ## get available versions for this package.
        my $vers = apt_cache_versions($pkg);

        ## use this version if it's available.
        next if $vers->{$ver};

        ## otherwise find the closest available package
        ## that is a newer version.
        my $last_ver = '';
        foreach my $aver (sort { dpkg_cmp_ver($a, $b) } keys %$vers) {
            $last_ver = $aver;
            last if dpkg_cmp_ver($ver, $aver) < 1;
        }

        ## set version if we found one.
        if ($last_ver) {
            $self->display(1, "* using $pkg $last_ver instead of $ver\n");
            $install->[$i] = "$pkg=$last_ver" if $last_ver;
        }
    }
}

sub debian_from_snapshot
{
    my $self = shift->new;

    ## open file to read snapshot from.
    my $fn = $self->option('debian-from-snapshot');
    open(my $fh, "<$fn") or $self->fault("can not read from $fn: $!");
    my $spkgs = parse_package_list($fh);
    close($fh);

    $self->display(1, "Determining current package list...");
    my $cpkgs = $self->debian_packages;

    $self->display(1, "done.\nCalculating differences...");

    my $ipkgs = [ map { split } @{ $self->option('debian-ignore') || [''] } ];

    my ($install, $remove, $purge) =
        $self->debian_package_diff($spkgs, $cpkgs, $ipkgs);

    unless (@$install || @$remove || @$purge) {
        $self->display(1, "No differences.\n");
        return;
    }

    $self->display(1, "out-of-sync.\n");

    ## if debian-closest-version was specified, check each install
    ## version and make sure it is a version that can be installed.
    if (@$install and $self->option('debian-closest-version')) {
        $self->debian_closest_version($install);
    }

    my $dryrun = $self->dryrun ? ' (dry-run) ' : '';

    $self->display(1, "Installing/removing packages with apt-get$dryrun...\n");

    my @apt = 'apt-get';
    push @apt, '--dry-run' if $dryrun;
    push @apt, 'install', @$install;
    push @apt, map { "$_-" } @$remove;
    push @apt, map { "$_-" } @$purge;

    $self->display(0, "dry-run: @apt\n") if $dryrun;

    $self->system_call(@apt)
        or $self->fault("something went wrong running apt-get install: code ", $? >> 8);

    if (@$purge) {
        $self->display(1, "Purging package configurations$dryrun...\n");

        my @dpkg = 'dpkg';
        push @dpkg, '--no-act' if $dryrun;
        push @dpkg, '--purge', @$purge;

        $self->display(0, "dry-run: @dpkg\n") if $dryrun;

        $self->system_call(@dpkg)
            or $self->fault("something went wrong running dpkg --purge: code ", $? >> 8);
    }

    $self->display(1, "Debian package installation syncronized.\n");
}

sub verbose
{
    my $self = shift->new;

    return $self->option('v') || 0;
}

sub timestamp
{
    my $self = shift->new;
    
    return $self->option('t') ? '[%Y-%m-%d %T]' : 0;
}

sub rsync_version {
    my $self  = shift->new;
    my $check = shift;

    unless ($self->{rsync_version}) {
        open(my $fh, "rsync --version|")
            or $self->fault("could not get rsync version: $! (code ", $? >> 8, ")");
        my $ver_line = <$fh>;
        close($fh);
        
        ## sample output.
        ## rsync  version 2.6.4  protocol version 29
        my ($version) = $ver_line =~ /rsync\s+version\s+(\d+\.\d+\.\d+)/;
        $version or $self->fault("could not determine rsync version: $ver_line");
        
        $self->display(3, "  # rsync version: $version\n");
        
        $self->{rsync_version} = $version;
    }
    
    if ($check) {
        no warnings 'uninitialized';
        my @ver = split /\./, $self->{rsync_version};
        my @des = split /\./, $check;
        
        for (my $i = 0; $i < @des; ++$i) {
            return 1 if $ver[$i] > $des[$i];
            return 0 if $ver[$i] < $des[$i];
        }
        return 1;
    }
    
    return wantarray ? (split /\./, $self->{rsync_version})
                     : $self->{rsync_version};
}

sub exit_code {
    my $self = shift->new;
    if (@_) {
        $self->{exit_code} ||= shift;
        return $self;
    }
    return $self->{exit_code} || 0;
}

1;
