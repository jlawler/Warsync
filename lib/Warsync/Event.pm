package Warsync::Event;

##############################################################################
## Event.pm - Warsync::Event                                                ##
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

use strict;
use warnings;

use Carp;
use IO::Handle;
use IO::File;
use Fcntl ':flock';
use Errno qw(ESRCH EPERM ENOENT);
use File::stat;

use Warsync::Util;

sub new {
    my $self  = shift;
    my $class = ref($self) || $self;
    return $self if ref $self;
    
    my $event = shift;
    my $sub   = shift || sub { 1 };
    
    $self = bless { name => $event, code => $sub }, $class;

    ## keep looping until it's our turn or we give up.
    for (;;) {
        ## try to create the event file.
        my $fh = IO::File->new('/var/run/warsync-events', O_RDWR|O_CREAT)
            or $self->fault("Could not open Warsync Events file: $!");

        ## get exclusive lock.
        flock $fh, LOCK_EX;
    
        ## make sure it's there is no funny business going on.
        if (stat($fh)->ino != stat('/var/run/warsync-events')->ino) {
            $self->fault("Warsync Events file inode has changed!!\nH4XX0R1NG DE73CTED!!!");
        }
        
        ## parse for current events.
        my $events = $self->parse_events($fh);
            
        ## check for queued event.
        if ($self->check_pid($events->{$event}[1])) {
            ## we are good to go for the queue.
            $events->{$event}[1] = $$;
            $self->{status} = 'queued';
            ## now check if we can run the event.
            if ($self->check_pid($events->{$event}[0])) {
                ## we are clear to run!
                $events->{$event}[1] = 0;
                $events->{$event}[0] = $$;
                $self->{status} = 'running';
                $self->write_events($fh, $events);
                $fh->close;
                return $self;
            }
            ## event is running in another process.
            else {
                ## write to event log that we are queued.
                ## and then check again later.
                $self->write_events($fh, $events);
                $fh->close;
                $self->runsub or return 0;
                sleep 10;
                next;
            }
        }
        ## event is already queued by another process
        ## so just skip this event.
        else {
            $self->{status} = 'skipped';
            $fh->close;
            return 0;
        }
    }
}

sub name {
    my $self = shift;
    return $self->{name};
}

sub status {
    my $self = shift;
    if (@_) {
        $self->{status} = shift;
        return $self;
    }
    return $self->{status};
}

sub runsub {
    my $self = shift;
    return $self->{code}->($self);
}

sub parse_events
{
    my $self = shift;
    my $fh   = shift;
    
    seek $fh, 0, 0;
    
    my %events = ();
    
    ## read in file contents.
    while (my $line = <$fh>) {
        $line =~ s/\s+$//;
        next unless $line;
        my ($name, $running, $queued) = split /:/, $line, 3;
        $events{$name} = [ $running, $queued ];
    }
    
    return \%events;
}

sub write_events
{
    my $self   = shift;
    my $fh     = shift;
    my $events = shift;
    
    seek $fh, 0, 0;
    truncate $fh, 0;
    
    foreach my $event (sort keys %$events) {
        next unless $events->{$event}[0] || $events->{$event}[1];
        print $fh "$event:$events->{$event}[0]:$events->{$event}[1]\n";
    }
    
    IO::Handle::flush($fh);
}

sub check_pid
{
    my $self = shift;
    my $upid  = shift or return 1;
    
    my ($spid) = $upid =~ /(\d+)/;
    
    return 1 unless $spid;
    return 1 if $spid == $$;    ## this process
    return 0 if kill 0, $spid;  ## signal confirms process is running.
    return 0 if $! == EPERM;   ## no permission to send signal.
    return 1 if $! == ESRCH;   ## process is dead.
}

sub DESTROY
{
    my $self  = shift;

    my $event = $self->name;
    
    my $fh = IO::File->new('/var/run/warsync-events', O_RDWR|O_CREAT)
        or $self->fault("Could not open Warsync Events file: $!");

    ## get exclusive lock.
    flock $fh, LOCK_EX;

    ## make sure it's there is no funny business going on.
    if (stat($fh)->ino != stat('/var/run/warsync-events')->ino) {
        $self->fault("Warsync Events file inode has changed!!\nH4XX0R1NG DE73CTED!!!");
    }
    
    ## parse for current events.
    my $events = $self->parse_events($fh);
     
    ## clear this process from the event.
    if ($self->{status} eq 'running') {
        $events->{$event}[0] = 0;
    }
    elsif ($self->{status} eq 'queued') {
        $events->{$event}[1] = 0;
    }
    
    if ($self->{status} ne 'skipped') {
        ## write event log.
        $self->write_events($fh, $events);
        $fh->close;
    }
}

1;
