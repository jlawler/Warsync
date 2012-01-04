package Warsync;

##############################################################################
## Warsync.pm - Warsync                                                     ##
##                                                                          ##
## Copyright (C) 2001-2005 Paul J. Baker                                    ##
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

## $Id: Warsync.pm 171 2006-04-24 21:15:48Z pbaker-guest $

=head1 NAME

Warsync - Wrapper Around Rsync

=head1 SYNOPSIS

  use Warsync;

=head1 DESCRIPTION

Warsync exports various functions used by B<Warsync> scripts.

=head1 AUTHOR

Paul Baker, pbaker@where2getit.com

=head1 SEE ALSO

warsync(8).

=cut

#############################################################################

require 5.6.0;

use strict;
use warnings;

require Exporter;

our @ISA = qw( Exporter );

=head1 VERSION

 Warsync Version 0.9.9
 Warsync Protocol 1.2

=cut

our $VERSION  = '0.9.9';
our $PROTOCOL = 1.2;

our $FORCE_TTY = 0;

1;
