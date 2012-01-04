package Warsync::Util;

##############################################################################
## Warsync::Util - Warsync Utilities Module                                 ##
##                                                                          ##
## Copyright (C) 2001-2004 Paul J. Baker                                    ##
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
## You may contact the author via electronic mail at pfars@paulbaker.net    ##
##############################################################################

## $Id: Util.pm 173 2006-09-06 22:58:57Z pbaker-guest $

=head1 NAME

Warsync - Wrapper Around Rsync

=head1 SYNOPSIS

  use Warsync::Util;

=head1 DESCRIPTION

Provides common utility functions for Warsync. Further documentation
to be written.

=head1 AUTHOR

Paul Baker, pbaker@where2getit.com

=head1 SEE ALSO

warsync(8).

=cut

#############################################################################

require 5.6.0;

use strict;
use warnings;

use Warsync;
use Carp;

require Exporter;

our @ISA = qw( Exporter );

our @EXPORT = qw( deep_copy dpkg_cmp_ver    fault is_ipaddr
                  is_fqdn   parse_package_list prompt
                  tty       version                         );
our @EXPORT_OK = @EXPORT;

our $NoPrompt = 0;

sub deep_copy ($);
sub deep_copy ($)
{
    ## http://www.stonehenge.com/merlyn/UnixReview/col30.html
    my $this = shift;
    if (not ref $this) {
        $this;
    }
    elsif (ref $this eq "ARRAY") {
        [map deep_copy($_), @$this];
    }
    elsif (ref $this eq "HASH") {
        +{map { $_ => deep_copy($this->{$_}) } keys %$this};
    }
    else {
        croak "what type is $_?"
    }
}

sub dpkg_cmp_ver ($$)
{

    my $a = shift;
    my $b = shift;

    ## note that system returns true when a command fails. and false
    ## when it was successful. that is why we are using unless
    ## here instead of if.

    ## $a less than $b
    unless (system('dpkg', '--compare-versions', $a, 'lt', $b)) {
        return -1;
    }
    ## $a equal $b
    unless (system('dpkg', '--compare-versions', $a, 'eq', $b)) {
        return 0;
    }
    ## if not less-than and not equal, it must be more.
    return 1;
}

sub fault (@)
{
    my $code  = 1;
    my $class = '';
    if (ref $_[0]) {
        $class = ' ' . ref($_[0]);
        $code = shift->exit_code;
    }
    print STDERR "Fatal$class Error: ", @_, "\n";
    exit($code || 1);
}

sub is_ipaddr ($) {
    my $in = shift;

    my ($ip) = $in =~ /(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/;

    return $ip;
}


{
    ## hostname/domain regex
    my $rfc1035_label_re  = qr/[0-9A-Za-z](?:[-0-9A-Za-z]*[0-9A-Za-z])?/;

    sub is_fqdn ($) {
        my $in = shift || '';

        my ($hostname) = $in =~ /($rfc1035_label_re(?:\.$rfc1035_label_re)*)/;

        return $hostname;
    }
}

sub parse_package_list ($)
{
    my $dpkg_h = shift;

    my %packages = ();

    while (my $line = <$dpkg_h>) {
        ## skip header lines.
        next if $line =~ /^[|+]|Desired/;
        chomp($line);

        if (my @pkg = $line =~
                /^
                (...) \s+              ## 0 - desired state
                ([-a-z0-9+.]+) \s+     ## 1 - package
                ([-.0-9+a-zA-Z:]+) \s+ ## 2 - version
                (\S.*)                 ## 3 - description
                /x)
        {
            $packages{$pkg[1]} = \@pkg;
        }
    }

    return \%packages;
}

sub prompt (@)
{
    my (%args) = @_;

    my $response = '';
    my @choices;

    if (exists $args{default}) {
        $response = $args{default};
    }

    if (exists $args{choices}) {
        $response ||= $args{choices}[0];
        @choices  = @{$args{choices}};
    }

    return $response if $NoPrompt;

    for (;;) {
        ## print the question.
        print $args{prompt};

        ## print list of choices.
        if (@choices) {
            print '(';
            print join( '/', map { (lc($response) eq lc($_))
                                   ? uc($_) : $_ } @choices );
            print ') ';
        }
        ## or print the default.
        elsif ($response) {
            print '[', $response, '] ';
        }
        ## mark end of prompt.
        print '>> ';
        ## get the response.
        chomp(my $in = <STDIN>);
        ## use if not blank.
        $response = $in if length($in);

        ## check that it matches a choice
        if (@choices) {
            foreach my $choice (@choices) {
                ## return if it matches.
                return lc($response) if lc($response) eq lc($choice);
            }
        }
        ## no specific choices, return any response.
        elsif ($response) {
            return $response;
        }
    }
}

sub tty (;$)
{
    if (@_) {
        $Warsync::FORCE_TTY = shift;
    }
    return $Warsync::FORCE_TTY || -t;
}

sub version ()
{
    my @version = ( "Warsync - Wrapper Around Rsync\n\n",
                    "warsync version $Warsync::VERSION ",
                    "protocol version $Warsync::PROTOCOL\n",
                    "Copyright (C) 2001-2004 by Paul Baker\n\n",
                    sprintf("perl version %vd\n", $^V ) );

    ## rsync version.
    open(my $rsync_h, '-|') || exec( 'rsync', '--version' );
    push @version, scalar(<$rsync_h>);
    close($rsync_h);
    ## ssh version.
    push @version, `ssh -V 2>&1`;

    return grep { defined } @version;
}

1;
