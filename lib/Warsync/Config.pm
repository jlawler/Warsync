package Warsync::Config;

##############################################################################
## Config.pm - Warsync Config Management Perl Module                        ##
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

## $Id: Config.pm 136 2004-05-06 06:37:36Z pbaker-guest $

=head1 NAME

Warsync::Config - Module for managing Warsync config files.

=head1 SYNOPSIS

  use Warsync::Config;

  my $config = Warsync::Config->new('/path/to/file');

=head1 DESCRIPTION

Warsync::Config reads in an ini style config file. It allows reading and
writing changes back to the config file.

Further documentation to be written.

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
use Fcntl ':flock';
use Warsync::Util 'deep_copy';

sub new
{
    my $self = shift;
    my $class = ref($self) || $self;
    return $self if ref $self;

    my $conffile = shift;

    my %section = (); ## sections of config.

    ## read in config file.
    open(my $fh, $conffile) or return undef;

    my $sec = ''; ## current section.
    while (<$fh>) {
        chomp;                  # no newline
        s/#.*//;                # no comments
        s/^\s+//;               # no leading white
        s/\s+$//;               # no trailing white
        next unless length;     # anything left?

        ## new section
        if (my ($new_sec) = $_ =~ /^\[(.+)\]$/) {
            $sec = $new_sec;
            next;
        }

        ## we must have a section before we can have
        ## any keys. so die if there is no section yet.
        croak "cannot have key=value line prior to [section] line"
            unless $sec;

        ## new key
        my ($var, $value) = split(/\s*=\s*/, $_, 2);
        $section{$sec}{ lc($var) } = $value;
    }

    close($fh);

    return bless { conffile => $conffile, section => \%section } => $class;
}

sub as_hashref
{
    my $self = shift->new;

    return deep_copy $self->{section};
}

sub list_sections
{
    my $self = shift->new;
    return keys %{ $self->{section} };
}

sub has_section
{
    my $self = shift->new;
    my $sec  = shift;

    return exists $self->{section}{$sec};
}

sub get_section
{
    my $self = shift->new;
    my $sec  = shift;

    $self->add_section($sec);

    return bless {
        config => $self,
        name   => $sec,
    } => 'Warsync::Config::Section';
}

sub add_section
{
    my $self = shift->new;

    my $sec = shift;

    ## check if section already exists.
    return $self if $self->has_section($sec);

    ## create section.
    push @{$self->{changes}}, [ 'add-section', $sec ];

    $self->{section}{$sec} = {};

    $self->write if $self->auto_write;

    return $self;
}

sub delete_section
{
    my $self = shift->new;

    my $sec = shift;

    return $self unless $self->has_section($sec);

    push @{$self->{changes}}, [ 'del-section', $sec ];
    delete $self->{section}{$sec};

    $self->write if $self->auto_write;

    return $self;
}

sub get_key
{
    my $self = shift->new;

    my $sec = shift;
    my $key = shift;

    $self->add_section($sec);

    return $self->{section}{$sec}{$key};
}

sub set_key
{
    my $self  = shift->new;

    my $sec   = shift;
    my $key   = shift;
    my $value = shift;

    $self->add_section($sec);

    ## check if key already exists.
    if (exists $self->{section}{$sec}{$key}) {
        ## key exists, only change if we have to.
        if ($self->{section}{$sec}{$key} eq $value) {
            return $self;
        }
    }

    push @{$self->{changes}}, [ 'set-key', $sec, $key, $value ];
    $self->{section}{$sec}{$key} = $value;

    $self->write if $self->auto_write;

    return $self;
}

sub delete_key
{
    my $self = shift->new;

    my $sec = shift;
    my $key     = shift;

    return $self unless exists $self->{section}{$sec};
    return $self unless exists $self->{section}{$sec}{$key};

    push @{$self->{changes}}, [ 'del-key', $sec, $key ];
    delete $self->{section}{$sec}{$key};

    $self->write if $self->auto_write;

    return $self;
}

sub auto_write
{
    my $self = shift->new;

    if (@_) {
        $self->{auto_write} = shift;
        return $self;
    }
    return $self->{auto_write};
}

sub write
{
    my $self = shift->new;

    ## open file for read/write.
    open(my $fh, "+< $self->{conffile}")
        or croak "could not write new config file: $!";
    ## lock the file.
    flock($fh, LOCK_EX);

    ## now read in the entire file to memory.
    my @lines = <$fh>;

    ## okay now iterate through list of changes, and make them.
    while (my $change = shift @{$self->{changes}}) {

        #print STDERR Data::Dumper::Dumper(\@lines, $change);

        if ($change->[0] eq 'set-key') {
            $self->_write_set_key(\@lines, $change);
        }
        elsif ($change->[0] eq 'add-section') {
            $self->_write_add_section(\@lines, $change);
        }
        elsif ($change->[0] eq 'del-section') {
            $self->_write_del_section(\@lines, $change);
        }
        elsif ($change->[0] eq 'del-key') {
            $self->_write_del_key(\@lines, $change);
        }
        else {
            die "bad change command: $change->[0]";
        }

    }

    #print STDERR Data::Dumper::Dumper(\@lines);

    ## okay let's write. seek to begin of file and then write new lines.
    seek($fh, 0, 0);
    print $fh @lines;

    ## all done.
    close($fh);

    return $self;

}

sub _write_set_key
{
    my $self   = shift->new;
    my $lines  = shift;      ## lines in file.
    my $change = shift;      ## change to be made.

    my $sec;         ## current section.
    my $last;        ## last non-blank line of section.
    my $changed = 0; ## true if change has been made.

    for (my $i = 0; $i < @$lines; ++$i) {
        my $line = $lines->[$i];
        chomp($line);                  # no newline
        $line =~ s/#.*//;                # no comments
        $line =~ s/^\s+//;               # no leading white
        $line =~ s/\s+$//;               # no trailing white
        next unless length $line;     # anything left?

        ## new section
        my $new_sec;
        if (($new_sec) = $line =~ /^\[(.+)\]$/) {
            $sec = $new_sec;
        }

        ## if we are in the right section, then track this line.
        if ($sec eq $change->[1]) {
            $last = $i;
        }
        ## wrong section, continue on...
        else {
            next;
        }

        ## skip if this is the first line of the section.
        next if $new_sec;

        ## get the key for this line.
        my ($var, $value) = split(/\s*=\s*/, $line, 2);

        ## is this the key we are looking for?
        if (lc($var) eq $change->[2]) {
            $lines->[$i] = "$change->[2] = $change->[3]\n";
            ++$changed;
        }
    }

    ## check if we made the change, if not add to last line of section.
    unless ($changed) {
        ## make sure we actually found the section.
        croak "could not find section $change->[1]" unless defined($last);
        splice(@$lines, $last + 1, 0, "$change->[2] = $change->[3]\n");
    }
}

sub _write_add_section
{
    my $self   = shift->new;
    my $lines  = shift;      ## lines in file.
    my $change = shift;      ## change to be made.

    push @$lines, "\n", "[$change->[1]]\n";
}

sub _write_del_section
{
    croak "delete section not yet implemented";
}

sub _write_del_key
{
    croak "delete key not yet implemented";
}

#######################################################################
#######################################################################

package Warsync::Config::Section;

sub get_key
{
    my $self = shift;
    my $key  = shift;

    my $sec  = $self->{name};

    return $self->{config}->get_key($sec, $key);
}

sub set_key
{
    my $self  = shift;
    my $key   = shift;
    my $value = shift;

    my $sec = $self->{name};

    $self->{config}->set_key($sec, $key, $value);

    return $self;
}
