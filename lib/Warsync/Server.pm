package Warsync::Server;

##############################################################################
## Server.pm - Warsync::Server                                              ##
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

use strict;
use warnings;

use Warsync::Host;
use Warsync::Util;
use Warsync::Config;
use Warsync::Event;

use IPC::Open2;
use IO::Handle;
use File::Spec;
use File::Temp;
use IO::File;
use DirHandle;
use Fcntl ':flock';


our @ISA = qw( Warsync::Host );

## here we have a regex that matches lines produced by rsync
## that don't signify that a change has been made, and at
## what verbosities to check for them.
## TODO: these regexes should be optimized by which statements
## occur most often.
my %rsync_regex = ();

## rsync -v
$rsync_regex{v} = qr/
    ^building \s+ file \s+ list
  | ^wrote \s+ \d+ \s+ bytes
  | ^sent \s+ \d+ \s+ bytes \s+ received \s+ \d+ \s+ bytes
  | ^total \s+ size \s+ is \s+ \d+
/x;

## rsync -vv
$rsync_regex{vv} = qr/
    ^(?:\[sender\]\s+|\[generator\]\s+)?including \s+ (?:file|directory)
  | \s+ is \s+ uptodate$
  | ^(?:\[sender\]\s+|\[generator\]\s+)?excluding \s+ (?:file|directory)
  | ^opening \s+ connection \s+ using
  | ^expand \s+ file_list \s+ to \s+ \d+ \s+ bytes
  | ^done$
  | ^deleting \s+ in \s+ \.
  | ^delta-transmission \s+ enabled
  | ^total: \s+ matches=\d+
  | $rsync_regex{v}
/x;

## rsync -vvv
$rsync_regex{vvv} = qr/
    ^add_exclude\(
  | ^(?:\[client\]|\[receiver\]) \s+ add_rule\(
  | ^\[client\] \s+ parse_filter_file
  | ^(?:\[sender\]\s+|\[generator\]\s+)?make_file\(
  | ^(?:recv|send)_file_list \s+ done$
  | ^send_files \s+ starting$
  | ^server_recv\(
  | ^recv_file_name\(
  | ^received \s+ \d+ \s+ names$
  | ^get_local_name \s+ count=\d+
  | ^generator \s+ starting \s+ pid=
  | ^delete_in_dir\(
  | ^recv_generator\(
  | ^send_files\(
  | ^recv_files\(
  | ^(?:send|recv|generate)_files \s+ phase=
  | ^send \s+ files \s+ finished$
  | ^(?:send|recv|generate)_files \s+ finished
  | ^_exit_cleanup\(
  | $rsync_regex{vv}
/x;

## rsync -vvvv
$rsync_regex{vvvv} = qr/
    ^cmd=ssh
  | ^\(Client\) \s+ Protocol \s+ versions:
  | ^\[(?:generator|receiver|sender|\d+)\] \s+ i=\d+
  | ^[ug]id \s+ \d+\(
  | ^file \s+ list \s+ sent$
  | ^server_recv\(
  | ^client_run \s+ waiting \s+ on \s+ \d+$
  | $rsync_regex{vvv}
/x;

sub new
{
    my $self = shift;
    my $class = ref($self) || $self;
    return $self if ref $self;

    unless (-r '/etc/warsync/server-key' and -w '/etc/warsync/server.conf') {
        return undef;
    }

    mkdir '/etc/warsync/filepacks.d' unless -d '/etc/warsync/filepacks.d';

    my $config = Warsync::Config->new('/etc/warsync/server.conf')
        or $self->fault("could not read server config: $!");

    return bless { config => $config, _display_queue => [] } => $class;
}

sub apt_conf
{
    my $self = shift->new;

    return '' unless -e '/etc/apt/apt.conf';

    open(my $fh, '/etc/apt/apt.conf')
        or $self->fault("could not read /etc/apt/apt.conf");
    my $slurp;
    { local $/ = undef; $slurp = <$fh> }
    close($fh);

    return $slurp;
}

sub apt_preferences
{
    my $self = shift->new;

    return '' unless -e '/etc/apt/preferences';

    open(my $fh, '/etc/apt/preferences')
        or $self->fault("could not read /etc/apt/preferences");
    my $slurp;
    { local $/ = undef; $slurp = <$fh> }
    close($fh);

    return $slurp;
}

sub apt_sources_list
{
    my $self = shift->new;

    open(my $fh, '/etc/apt/sources.list')
        or $self->fault("could not read /etc/apt/sources.list");
    my $slurp;
    { local $/ = undef; $slurp = <$fh> }
    close($fh);

    return $slurp;
}

sub client
{
    my $self   = shift->new;
    my $client = shift;

    return undef unless $self->config->has_section("client-$client");

    return Warsync::Server::Client->new($self, $client);
}

sub config
{
    my $self = shift->new;

    return $self->{config};
}

sub display
{
    my $self = shift->new;

    ## if we are on a tty, something has changed, or want verbosity,
    ## display the output as appropriate.
    if (tty or $self->modified or $self->verbose > 1) {
        $self->SUPER::display(@_);
    }
    ## otherwise queue the message for a later time.
    else {
        push @{ $self->{'_display_queue'} }, [ \time, @_ ];
    }
}

sub display_clear
{
    my $self = shift->new;

    $self->{'_display_queue'} = [];
    $self->{'_display_queue_block'} = undef;
}

sub filepacks
{
    my $self = shift->new;

    my $d = DirHandle->new('/etc/warsync/filepacks.d')
        or return undef;

    my (@packs) = sort grep { /^[^.]/ && -f "/etc/warsync/filepacks.d/$_" } $d->read;
    $d->close;

    return @packs;
}

sub list_clients
{
    my $self = shift->new;

    return map  { $_ =~ s/^client-//;  $self->client($_) }
           sort { my (@a) = $a =~ /([a-zA-Z]+|[0-9]+|[^a-zA-Z0-9])/g;
                  my (@b) = $b =~ /([a-zA-Z]+|[0-9]+|[^a-zA-Z0-9])/g;
                  no warnings;
                  for (my $i = 0; $i < @a; ++$i) {
                      if ($a[$i] =~ /\d/ and my $rv = $a[$i] <=> $b[$i]) {
                          return $rv;
                      }
                      if (my $rv = $a[$i] cmp $b[$i]) {
                          return $rv;
                      }
                  }
                  return -1;
                } 
           grep { $_ =~ m/^client-/  } $self->{config}->list_sections;
}

sub modified
{
    my $self = shift->new;
    if (@_) {
        ## set modified bit.
        $self->{_modified} = shift;
        ## if modified is true, dump currently queued output.
        my $dq = $self->{'_display_queue'};
        if ($self->{_modified} and @$dq) {
            $self->{'_display_queue_block'} = undef;
            while (my $out = shift @$dq) {
                $self->display(@$out);
            }
        }
        ## if modified now false, clear old queue if something there.
        else {
            if (defined $self->{'_display_queue_block'}) {
                splice @$dq, $self->{'_display_queue_block'};
            }
            else {
                $self->{'_display_queue_block'} = @$dq;
            }
        }

        return $self;
    }
    return $self->{_modified} || 0;
}

sub replication
{
    my $self = shift->new;

    ## this will store the list of clients to replicate.
    my @clients = ();

    ## get clients from command line.
    if (my $clients = $self->option('client')) {
        foreach (@$clients) {
            foreach my $name (split /[\s,]+/, $_) {
                my $client = $self->client($name);
                unless ($client) {
                    $self->display(0, "!! $name is not a valid client\n");
                    next;
                }
                push @clients, $client;
            }
        }
    }
    ## if no clients specified, default to all.
    else {
        ## except for those configured as explict only.
        @clients = grep { not $_->skip } $self->list_clients;
    }

    my @filepacks = @ARGV;

    if ($self->option('a')) {
        @filepacks = $self->filepacks;
    }

    ## they need to specify something to do.
    unless (@filepacks or $self->option('debian')
            or $self->option('check-clients'))
    {
        Pod::Usage::pod2usage(1);
    }

    ## iterate through each client and replicate.
    my $i = 0;
    my $num_of_clients = @clients;
    my $mod = 1;
    my $off = 0;
    if (my $offset = $self->option('client-offset')) {
        ($off, $mod) = $offset =~ /(\d+):(\d+)/;
        $mod ||= 1;
        $off ||= 0;
    }
    foreach my $client (@clients) {

        ## check offset and increment.
        next unless ((++$i % $mod) == $off);

        my $name = $client->name;

        my $dryrun = $self->option('n') ? ' (dry-run) ' : '';

        $self->display_clear;
        
        $self->display(1,
            "######################################################\n");

        $self->display(1, "Replicating $name$dryrun... ($i of $num_of_clients)\n");

        ## check protocol of client to make sure it matches.
        if (my $prot_match = $client->protocol_match) {
            if ($client->dot_warsyncignore and $client->protocol < 1.1) {
                $self->display(0, "~ Use of .warsyncignore files enabled for $name client,\n");
                $self->display(0, "~ but client does not support this feature. Please\n");
                $self->display(0, "~ upgrade client if you would like to use this feature.\n");
            }
            if ($self->option('check-clients')) {
                $client->integrity_check;
            }
            ## replicate filepacks.
            foreach my $filepack (@filepacks) {
                $client->replicate_filepack($filepack);
            }
            ## debian syncronization.
            if ($self->option('debian')) {
                $client->sync_debian_packages;
            }
        }
        elsif (defined $prot_match) {
            $self->display(0, "!! Client $name communication protocol differs.\n");
            $self->display(0, "!! Please manually upgrade client to version $Warsync::VERSION.\n");
            $self->exit_code(11);
        }
        else {
            $self->display(0, "!! Could not connect to client $name.\n");
            $self->exit_code(12);
        }
    }
}

## warsync-echo-name command. this simply echos the server's hostname.
sub rpc_echo_name
{
    my $self = shift->new;
    print 'name: ', $self->name, "\n";
}

sub system_call
{
    my $self = shift->new;

    $self->display(3, "s-syscall: @_\n");

    return system(@_) ? 0 : 1;
}

#############################################################################
#############################################################################

package Warsync::Server::Client;

use Warsync::Util grep(!/fault/, @Warsync::Util::EXPORT);

## Warsync::Server::Client->new($server, $client_name);
sub new
{
    my $self = shift;
    my $class = ref($self) || $self;
    return $self if ref $self;

    my $server = shift;
    my $name   = shift;

    $self = bless {
        server => $server,
        name   => $name,
        config => $server->config->get_section("client-$name")
    } => $class;

    return $self;
}

sub exit_code {
    my $server = shift->server;
    $server->exit_code(@_);
}

sub fault {
    my $server = shift->server;
    $server->Warsync::Util::fault(@_);
}

sub base_command
{
    my $self = shift->new;
    my $cmd  = shift;

    my $v = $self->server->option('v');

    my $rcmd = 'warsync-';
    $rcmd   .= 'compress-'  if $self->compress;
    $rcmd   .= 'tty-'       if tty;
    $rcmd   .= 'dry-run-'   if $self->server->option('n');
    $rcmd   .= 'quiet-'     if $self->server->option('q');
    $rcmd   .= "verbose$v-" if $v;

    $rcmd   .= $cmd;

    return $rcmd;
}

sub system_call
{
    my $self = shift->new;

    $self->server->display(4, "s-syscall: @_\n");

    return system(@_) ? 0 : 1;
}

sub command
{
    my $self   = shift->new;
    my $cmd    = $self->base_command(shift);
    my @args   = @_;

    my @scmd = ( 'ssh', '-p', $self->port,
                        ($self->compress ? '-C' : () ),
                        ($self->server->option('accept-unknown-hosts')
                            ? ('-o', 'StrictHostKeyChecking=no') : () ),
                        '-o', 'NumberOfPasswordPrompts=0',
                        '-i', '/etc/warsync/server-key',
                        '-T', $self->fqdn, $cmd, @args );

    open(my $ssh_h, '-|') || exec( @scmd )
        or die "error '$!' trying to exec: @scmd";

    return $ssh_h;
}

sub gui_command
{
    my $self   = shift->new;

    ## run as non-gui command if this is not a tty.
    unless (-t) {
        my $server = $self->server;
        my $comh = $self->command(@_);
        while (<$comh>) {
            $server->modified(1);
            $server->display(1, $_);
        }
        return close($comh);
    }

    my $cmd    = $self->base_command(shift);
    my @args   = @_;

    my @scmd = ( 'ssh', '-p', $self->port,
                        ($self->compress ? '-C' : () ),
                        ($self->server->option('accept-unknown-hosts')
                            ? ('-o', 'StrictHostKeyChecking=no') : () ),
                        '-o', 'NumberOfPasswordPrompts=0',
                        '-i', '/etc/warsync/server-key',
                        '-qt', $self->fqdn, $cmd, @args );

    return $self->system_call(@scmd)
}

sub rw_command
{
    my $self   = shift->new;
    my $cmd    = $self->base_command(shift);
    my @args   = @_;

    my @scmd = ( 'ssh', '-p', $self->port,
                        ($self->compress ? '-C' : () ),
                        ($self->server->option('accept-unknown-hosts')
                            ? ('-o', 'StrictHostKeyChecking=no') : () ),
                        '-o', 'NumberOfPasswordPrompts=0',
                        '-i', '/etc/warsync/server-key',
                        '-T', $self->fqdn, $cmd, @args );

    my ($rh, $wh);
    my $pid = IPC::Open2::open2($rh, $wh, @scmd);

    return $pid, $rh, $wh;
}

sub compress
{
    my $self = shift->new;

    if (@_) {
        $self->set_key('compress', shift);
        return $self;
    }
    ## the next two lines are needed to get work
    ## with perl 5.8's more restrictive taintmode.
    my $unsafe = $self->get_key('compress') || 0;
    my ($compress) = $unsafe =~ /(.)/;

    return $compress;
}

sub config
{
    my $self = shift->new;

    return $self->{config};
}

sub skip
{
    my $self = shift->new;

    if (@_) {
        $self->set_key('skip', shift);
        return $self;
    }

    return $self->get_key('skip');
}

sub fqdn
{
    my $self = shift->new;

    if (@_) {
        $self->set_key('fqdn', is_fqdn(shift));
        return $self;
    }

    return is_fqdn($self->get_key('fqdn'));
}

sub get_key
{
    my $self = shift->new;

    return $self->config->get_key(shift);
}

sub ignore_debs
{
    my $self   = shift->new;

    my @debs = split ' ', $self->get_key('ignore_debs') || '';
    return \@debs;
}

sub dot_warsyncignore
{
    my $self = shift->new;

    if (@_) {
        $self->set_key('dot_warsyncignore', shift);
        return $self;
    }

    return $self->get_key('dot_warsyncignore');
}


sub set_key
{
    my $self = shift->new;

    return $self->config->set_key(shift, shift);
}

sub name
{
    my $self = shift->new;

    return $self->{name};
}

sub rsync_opt
{
    my $self = shift->new;

    if (@_) {
        $self->set_key('rsync_opt', join(' ', @_));
        return $self;
    }

    my $t = $self->get_key('rsync_opt') || '';
    my ($ut) = $t =~ /(.*)/;

    return split ' ', $ut;
}

sub packages
{
    my $self = shift->new;

    my $cmd_h = $self->command('echo-package-list');
    my $cpkgs = parse_package_list($cmd_h);
    close($cmd_h);

    return $cpkgs;
}

sub port
{
    my $self = shift->new;

    if (@_) {
        my ($port) = shift =~ /(\d+)/;
        $self->set_key('port', $port);
        return $self;
    }

    my $unsafe = $self->get_key('port') || 22;
    my ($port) = $unsafe =~ /(\d+)/;

    return $port || 22;
}
    
sub protocol
{
    my $self = shift->new;
    
    return $self->{protocol} if $self->{protocol};

    my $com_h = $self->command('echo-protocol-version');

    chomp(my $ver = <$com_h> || 0);

    close($com_h)
        or return undef;
    
    return $self->{protocol} = $ver;
}

sub protocol_match
{
    my $self = shift->new;

    my $ver = $self->protocol;
    no warnings;

    return 1 if int($ver) == int($Warsync::PROTOCOL);
    return 0;
}

sub process_warsyncignores
{
    my $self  = shift->new;
    my $paths = shift;
    
    ## only supported on protocol 1.1 or greater clients.
    return () if $self->protocol < 1.1;

    my $server = $self->server;
    
    ## this will hold list of .warsyncignore files to fetch.
    my %files = ();
    ## paths to search for .warsyncignore files.
    my @search_paths = ();
    
    ## go through paths get list of .warsyncignore files.
    foreach my $path (keys %$paths) {
        if ($paths->{$path} eq 'R') {
            push @search_paths, $path;
        }
        else {
            $files{"$path.warsyncignore"} = 1;
        }
    }
    
    if (@search_paths) {
        my $com_h = $self->command('find-warsyncignore-files', @search_paths);
        while (my $file = <$com_h>) {
            $file =~ s/\s+$//;
            $files{$file} = 1;
        }
        close($com_h);
    }
    
    ## now with the list of files we need, start fetching them from client.
    my @excludes = ();
    foreach my $file (keys %files) {
        my ($path) = $file =~ m!^(/.*/?)\.warsyncignore$!;
        next unless $path;
        my $parsing = 0;
        my $fh = $self->command('echo-file', $file);
        while(my $unsafe_excl = <$fh>) {

            ## skip comment lines
            next if $unsafe_excl =~ /^\s*#/;
            
            ## grab only that which we want. but first get rid of 
            ## leading and trailing spaces.
            $unsafe_excl =~ s/^\s+|\s+$//g;
            
            next unless length($unsafe_excl);

            ## capture the file we are ignoring
            if ( my ($excl) =
                     $unsafe_excl
                  =~ m{^ ( ## capture something that is either
            
                           [.] ## a single dot and nothing else
                           
                         |     ## or a string that...  
                           
                           [.]?    ## the first character may be a .
                           [^/.]+  ## followed by anything that
                                   ## is not a / or another .
                           
                           (?:     ## the remaining characters may be either
                               [^.]    ## something that is not a dot
                             | [.][^.] ## or a dot followed by not a dot.
                           )*      ## and this can repeat indefinitely...
                           
                         ) $   ## until the end of the string
                         
                       }x )
            {
                ## display message if we are ignoring something.
                $server->display(1, "    ^ processing ignores from $file\n") unless $parsing++;
                
                if ($excl eq '.') {
                    push @excludes, $path;
                }
                else {
                    push @excludes, "$path$excl";
                }
            }
        }
        close($fh);
    }
    return @excludes;
}

## you never want to replicate /dev or /proc. EVAR!
my @default_include = ( '- /dev', '- /proc', '- lost+found' );
my %default_include = map { $_ => 1 } @default_include;


sub run_filepack_command {
    my $self = shift->new;
    my $pack = shift;
    my $cmd  = shift;
    my $server = $self->server;
    
    $self->fault("Client does not support remote commands")
        if $self->protocol < 1.2;
    
    my %subs = (
        's' => $server->name,
        'c' => $self->name,
        'f' => $pack,
        '%' => '%',
    );
    
    ## perform substitutions on command.
    $server->display(2, "    # $cmd\n");
    $cmd =~ s/%([scf%])/$subs{$1}/g;
    $server->display(1, "    ^ $cmd\n");
    
    my @run = ( 'remote-command', $cmd );
    
    $self->gui_command(@run)
        or $self->fault("Client remote command returned status: ", $? >> 8);
}

sub replicate_filepack
{
    my $self = shift->new;
    my $unsafe_pack = shift;

    my $server = $self->server;

    $server->modified(0);

    my ($pack) = $unsafe_pack =~ /([-\w.]+)/;

    my $v = $server->option('v') || 0;
    
    ## scan filepack to see if we are skipping it before getting even lock.
    open(my $fh, "/etc/warsync/filepacks.d/$pack")
        or $self->fault("Filepack $pack not found: $!");
    while (<$fh>) {
        chomp;              # no newline
        s/#.*//;            # no comments
        s/^\s+//;           # no leading white
        s/\s+$//;           # no trailing white
        next unless length; # anything left?

        ## read include line.
        my $inc = $_;

        ## check for config setting.
        if ($inc =~ /=/) {
            my ($var, $value) = $inc =~ /([^=]+?)\s*=\s*(.*)/;
            $var = uc($var);
            ## skip when doing all-filepacks
            if ($var eq 'SKIP') {
                if ($value and $server->option('a')
                        and not $server->option('include-skipped-filepacks')) {
                    $server->display(1, "  ~ skipping $pack by default.\n");
                    return;
                }
            }
        }
    }
    ## seek back to the beginning.
    seek $fh, 0, 0;

    ## start warsync event or skip if already running.
    my $event = Warsync::Event->new(
        $self->name . '-filepack-' . $pack,
        sub { $server->display(1, "* filepack $pack syncronization queued...\n"); 1 }
    );
    
    unless ($event) {
        $server->display(1, "~ skipping filepack $pack syncronization - already running\n");
        return;
    }
    
    $server->display(1, "* filepack $pack syncronization...\n");

    my @pre_cmd   = ();  # pre commands are run before replicatoin
    my @mod_cmd   = ();  # mod commands are run if files modified
    my @post_cmd  = ();  # post commands are run after replication
    my $version_message = 0; # true if notified that client needs upgrade.

    my @rsync_opt = $self->rsync_opt;  # extra options to pass to rsync.

    my $base_dir  = '/'; # base directory to replicate
    
    ## if rsync version 2.6.4 or greater use --delete-during.    
    my $delete = $server->rsync_version('2.6.4')
               ? '--delete-during'
               : '--delete';

    my @include   = @default_include;  # contains include order
    my %include   = %default_include;  # used for include dup check

    ## check if client is configured for (and supports) .warsyncignore files.
    my $dot_ignore  = $self->dot_warsyncignore
                            && $self->protocol >= 1.1;
    ## paths to check for .warsyncignore
    my %ignore_path = ( '/' => 1 );

    my $line = 0;
    while (<$fh>) {
        ++$line;            # line number
        chomp;              # no newline
        s/#.*//;            # no comments
        s/^\s+//;           # no leading white
        s/\s+$//;           # no trailing white
        next unless length; # anything left?

        ## read include line.
        my $inc = $_;

        ## check for config setting.
        if ($inc =~ /=/) {
            my ($var, $value) = $inc =~ /([^=]+?)\s*=\s*(.*)/;
            $var = uc($var);
            if ($var eq 'PRE_CMD') {
                push @pre_cmd, $value;
            }
            elsif ($var eq 'POST_CMD') {
                push @post_cmd, $value;
            }
            elsif ($var eq 'MOD_CMD') {
                push @mod_cmd, $value;
            }
            #elsif ($var eq 'BASEDIR') {
            #    fault "BASEDIR specified on line $line of $pack ",
            #          "must be absolute" unless $value =~ m!^/!;
            #
            #    $value .= '/' unless $value =~ m/\/$/;
            #    $base_dir = $value;
            #}
            elsif ($var eq 'RSYNC_OPT') {
                ## check for quotes.
                if (my ($quote, $val) = $value =~ /^(["'])(.+)\1$/) {
                    $server->display(2, "  ^ rsync option: $val\n");
                    push @rsync_opt, $val;
                }
                else {
                    foreach my $val (split / /, $value) {
                        $server->display(2, "  ^ rsync option: $val\n");
                        push @rsync_opt, $value;
                    }
                }
            }
            elsif ($var eq 'DELETE') {
                unless ($value) {
                    $delete = '';
                    $server->display(2, "  ^ file deletion: disabled\n");
                }
            }
            #elsif ($var eq 'INCLUDE') {
            #    my $inc_fp = $self->read_filepack($value);
            #    
            #    push @pre_cmd,   @{ $inc_fp{pre_cmd} };
            #    push @post_cmd,  @{ $inc_fp{post_cmd} };
            #    push @mod_cmd,   @{ $inc_fp{mod_cmd} };
            #    push @rsync_opt, @{ $inc_fp{rsync_opt} };
            #    
            #}
            ## skip when doing all-filepacks
            elsif ($var eq 'SKIP') {
                if ($value and $server->option('a')
		    and not $server->option('include-skipped-filepacks')) {
                    $server->display(1, "  ~ skipping $pack by default.\n");
                    return;
                }
            }
            else {
                $self->fault("Unknown $var variable at line $line ",
                      "of filepack $pack.");
            }
            next;
        }

        ## check for list reset.
        if ($inc eq '!') {
            @include = @default_include;
            %include = %default_include;
            $server->display(2, "  - found '!' resetting filepack includes\n");
            next;
        }

        ## exclude if starts with -
        if ($inc =~ /^- /) {
            unless ($include{$inc}++) {
                push @include, $inc;
                (my $ex = $inc) =~ s/^- //;
                $server->display(2, "  - excluding $ex\n");
            }
            next;
        }

        ## remove forced include if it exists.
        $inc =~ s!^\+ !!;

        ## check if path is absolute
        my $abs = $inc =~ s!^/!!;

        ## split path into separate components.
        my @path = split('/', $inc);
        ## find the first without a wildcard.
        my @pre_path = ();
        while (my $c = shift @path) {
            ## path does not contain a wildcard,
            ## pre-include it.
            if ($c !~ m![\[\?\*\]]!) {
                push @pre_path, $c;
                my $path = join('/', @pre_path);
                ## make absolute if it should be.
                $path = "/$path" if $abs;
                ## add directory slash if not last component.
                $path .= '/' if @path;
                ## check for .warsyncignore if absolute directory path
                if ($abs and @path) {
                    $ignore_path{$path} ||= 1;
                }
                ## add to includes if not already there.
                unless ($include{$path}++) {
                    push @include, $path;
                    $server->display(@path ? 3 : 2, "  - including $path\n");
                }
            }
            ## other wise if it contained a wildcard, put it back
            ## and dump out.
            else {
                unshift @path, $c;
                ## if absolute, set previous directory to
                ## look for .warsyncignore files recursively.
                if ($abs) {
                    my $path = join('/', @pre_path);
                    $ignore_path{"/$path/"} = 'R' if $path;
                }
                last;
            }
        }

        ## build final include
        my $path = join('/', @pre_path, @path);
        ## make absolute if necessary.
        $path = "/$path" if $abs;

        ## and finally do it.
        unless ($include{$path}++) {
            push @include, $path;
            $server->display(2, "  - including $path\n");
        }
    }

    close($fh);

    ## nothing to do if include is empty.
    unless (@include) {
        $server->display(1, "  - nothing included by filepack\n");
        return;
    }

    if (@pre_cmd) {
        if ($server->option('skip-pre-cmds')) {
            $server->display(1, "  ~ skipping pre-sync commands.\n");
        }
        else {
            $server->display(1, "  - running pre-sync commands on client...\n");
            if ($self->protocol < 1.2) {
                $server->display(1, "    ^ client does not support remote commands\n");
                $server->display(1, "    ^ please upgrade client to warsync 0.9.9 or later\n")
                    unless $version_message++;
            }
            else {
                foreach my $cmd (@pre_cmd) {
                    $self->run_filepack_command($pack, $cmd);
                }
                $server->display(1, "  - pre-sync commands complete.\n");
            }
        }
        $server->modified(0);
    }

    ## now fetch .warsyncignore information
    if ($dot_ignore) {
        $server->display(1, "  - processing .warsyncignore files on client...\n");
        unshift @include, '- .warsyncignore';
        foreach my $path ($self->process_warsyncignores(\%ignore_path)) {
            unless ($include{"- $path"}++) {
                unshift @include, "- $path";
                $server->display(2, "    - ignoring $path\n");
            }
        }
    }

    ## exclude everything not explicitly included.
    push @include, '- *' unless $include{'- *'}++;

    ## open tempfile to write include list.
    my ($tmpfh, $unsafe_tmpfn) = File::Temp::tempfile(
        'warsyncXXXXXXXX', DIR => File::Spec->tmpdir, UNLINK => 1
    );

    my ($tmpfn) = $unsafe_tmpfn =~ /([\/0-9A-Za-z]*warsync[0-9A-Za-z]{8})/;

    die "strange tmpfn $unsafe_tmpfn != $tmpfn"
        unless $tmpfn eq $unsafe_tmpfn;

    foreach my $inc (@include) {
        print $tmpfh $inc, "\n";
    }

    ## flush include list just to be safe.
    IO::Handle::flush($tmpfh);

    ## build ssh command to use for rsync communication.
    my @ssh   = ( 'ssh', '-p', $self->port,
                        ($self->server->option('accept-unknown-hosts')
                            ? ('-o', 'StrictHostKeyChecking=no') : () ),
                         '-o', 'NumberOfPasswordPrompts=0',
                         '-i', '/etc/warsync/server-key', );

    ## rsync needs to be a little verbose so we can get the output.
    ## but increase it's verbosity if warsync was called with -vv or more.
    my $vv = $v > 2 ? 'v' x ($v - 1) : 'v';
    ## but no need to have more than four.
    $vv = 'vvvv' if length($vv) > 4;

    my $dryrun = $server->option('n') ? ' (dry-run) ' : '';
    ($dryrun) = $dryrun =~ /(.*)/; ## untaint for perl 5.8.

    my $exclude_tmp = '--exclude=' . $tmpfn;
    my $include_tmp = '--include-from=' . $tmpfn;

    ## build rsync command.
    my @rsync = ( 'rsync', '--rsh='.join(' ', @ssh),
                           $delete         ? $delete  : (), 
                           $self->compress ? '-z'     : (),
                           $dryrun         ? "-an$vv" : "-a$vv",
                           @rsync_opt,
                           $exclude_tmp,
                           $include_tmp,
                           $base_dir, $self->fqdn.":$base_dir"
                );

    $server->display(1, "  - file replication$dryrun...\n");
    for (;;) {
        $server->display(3, "    # rsync: @rsync\n");
        open(my $rsync_h, '-|') || exec( @rsync )
            or $self->fault("error '$!' trying to exec: @rsync");
    
        while (<$rsync_h>) {
            chomp;
            s/^\s+//;           # no leading white
            s/\s+$//;           # no trailing white
            next unless length; # anything left?
    
            ## grep for lines that do not signify a change.
            if ($_ =~ /$rsync_regex{$vv}/) {
                $server->display(2, "    # ", $_, "\n");
            }
            ## change was made
            else {
                $server->modified(1);
                $server->display(1, "    + ", $_, "\n");
            }
        }
    
        unless (close $rsync_h) {
            ## something failed on the rsync.
            my $code = ($? + 0) / 256;
            ## check for missing source files "error".
            if ($code == 24) {
                $server->display(1, "  - files removed during rsync. restarting rsync$dryrun...\n");
                next;
            }
            $self->fault("Something went wrong during $pack filepack replication: code $code");
        }
        last;
    }

    ## close temporary include file.
    close($tmpfh);

    if ($server->modified) {
        $server->display(1, "  - file replication completed.\n");
        if (@mod_cmd) {
            if ($server->option('skip-mod-cmds')) {
                $server->display(1, "  ~ skipping modification commands.\n");
            }
            else {
                $server->display(1, "  - running modification commands on client..\n");
                if ($self->protocol < 1.2) {
                    $server->display(1, "    ^ client does not support remote commands\n");
                    $server->display(1, "    ^ please upgrade client to warsync 0.9.9 or later\n")
                        unless $version_message++;
                }
                else {
                    foreach my $cmd (@mod_cmd) {
                        $self->run_filepack_command($pack, $cmd);
                    }
                    $server->display(1, "  - modification commands complete.\n");
                }
            }
        }
    }
    else {
        $server->display(1, "  - files already syncronized.\n");
    }

    $server->modified(0);

    if (@post_cmd) {
        if ($server->option('skip-post-cmds')) {
            $server->display(1, "  ~ skipping post-sync commands.\n");
        }
        else {
            $server->display(1, "  - running post-sync commands on client...\n");
            if ($self->protocol < 1.2) {
                $server->display(1, "    ^ client does not support remote commands\n");
                $server->display(1, "    ^ please upgrade client to warsync 0.9.9 or later\n")
                    unless $version_message++;
            }
            else {
                foreach my $cmd (@post_cmd) {
                    $self->run_filepack_command($pack, $cmd);
                }
                $server->display(1, "  - post-sync commands complete.\n");
            }
        }
        $server->modified(0);
    }
}

sub sync_apt_conf
{
    my $self   = shift->new;

    my $server = $self->server;

    return 1 if $self->get_key('no_apt_conf_sync');

    

    my ($pid, $rh, $wh) = $self->rw_command('sync-apt-conf');

    print $wh $server->apt_conf;
    close($wh);

    my $res = <$rh>;

    my $worked = 1 if (close($rh) and $res =~ /success/);

    waitpid $pid, 0;

    return $worked;
}

sub sync_apt_preferences
{
    my $self   = shift->new;

    my $server = $self->server;

    return 1 if $self->get_key('no_apt_preferences_sync');

    my ($pid, $rh, $wh) = $self->rw_command('sync-apt-preferences');

    print $wh $server->apt_preferences;
    close($wh);

    my $res = <$rh>;

    my $worked = 1 if (close($rh) and $res =~ /success/);

    waitpid $pid, 0;

    return $worked;
}

sub sync_apt_sources
{
    my $self   = shift->new;

    my $server = $self->server;

    return 1 if $self->get_key('no_apt_sources_sync');

    my ($pid, $rh, $wh) = $self->rw_command('sync-apt-sources');

    print $wh $server->apt_sources_list;
    close($wh);

    my $res = <$rh>;

    my $worked = 1 if (close($rh) and $res =~ /success/);

    waitpid $pid, 0;

    return $worked;
}

sub apt_get_update
{
    my $self = shift->new;
    return $self->gui_command('apt-get-update');
}

sub sync_debian_packages
{
    my $self   = shift->new;

    my $server = $self->server;

    $server->modified(0);
    
    ## start warsync event or skip if already running.
    my $event = Warsync::Event->new(
        $self->name . '-debian',
        sub { $server->display(1, "* debian package syncronization queued...\n"); 1 }
    );
    
    unless ($event) {
        $server->display(1, "* skipping debian package syncronization - already running\n");
        return;
    }

    my $v = $server->option('v');
    $server->display(1, "* debian package syncronization\n");

    $server->display(1, "  - determining server packages...");
    my $spkgs = $server->packages($v);
    $server->display(1, "done.\n");

    $server->display(1, "  - determining client packages...");
    my $cpkgs = $self->packages($v);
    $server->display(1, "done.\n");

    my $ipkgs = $self->ignore_debs;

    $server->display(1, "  - calculating differences...");
    my ($install, $remove, $purge) =
        $self->server->debian_package_diff($spkgs, $cpkgs, $ipkgs, $v);

    ## return if there is nothing to do.
    unless (@$install || @$remove || @$purge) {
        $server->display(1, "client up-to-date.\n");
        return;
    }

    $server->modified(1);

    ## have client update package sources.
    $server->display(1, "client out-of-sync.\n");
    $server->display(1, "  - syncronizing package sources...\n");

    $self->sync_apt_conf
        or $self->fault("client sync-apt-conf returned status: ", $? >> 8);

    $self->sync_apt_preferences
        or $self->fault("client sync-apt-preferences returned status: ", $? >> 8);

    $self->sync_apt_sources
        or $self->fault("client sync-apt-sources returned status: ", $? >> 8);

    $self->apt_get_update
        or $self->fault("client apt-get-update returned status: ", $? >> 8);

    $server->display(1, "  - installing/removing packages on client with apt-get...\n");

    my @apt = "package-install";
    push @apt, @$install; ## packages to install
    push @apt, map { "$_-" } @$remove;              ## packages to remove
    push @apt, map { "$_-" } @$purge;               ## packages to purge

    $self->gui_command(@apt)
            or $self->fault("client apt-get-install returned status: ", $? >> 8);

    if (@$purge) {
        $server->display(1, "  - purging packages on client with apt-get...\n");
        my @apt = ("package-purge", @$purge);
        $self->gui_command(@apt)
                or $self->fault("client apt-get-purge returned status: ", $? >> 8);
    }

    $server->display(1, "  - package syncronization completed!\n");
    ## reset known client protocol version because it could have changed
    ## if warsync was upgraded during package syncronization.
    $self->{protocol} = 0;
}

sub server
{
    my $self = shift->new;

    return $self->{server};
}

sub fetch_public_key {
    my $self = shift;

    my $ssh_h = $self->command('echo-client-public-key');

    my @scom = <$ssh_h>;

    close($ssh_h);

    if ($scom[0]) {
        return undef unless $scom[0] =~ /^ssh-dss/;
        return $scom[0];
    }

    return undef;
}

sub loop_back_connection
{
    my $self = shift;
    my $server = $self->server;

    my $ssh_h = $self->command('echo-server-name');

    my @scom = <$ssh_h>;

    close($ssh_h);

    ## if the server name matches, return true.
    if ($scom[0]) {
        my ($sn) = $scom[0] =~ /^server-name: (\S+)/;
        return 1 if $sn and $sn eq $server->name;
    }

    ## if we connected, but name didn't match return 0.
    ## otherwise return undef.
    return $scom[0] ? 0 : undef;
}

sub integrity_check {
    my $self = shift->new;
    
    my $server = $self->server;
    my $client_name = $self->name;

    $server->display(1,
        "Testing server to $client_name round-trip communications...");

    ## $client is reachable at $fqdn. fetch it's authentication public key.
    my $client_public_key = $self->fetch_public_key
        or $self->fault("could not fetch client public key: $!");

    ## now add server key to authorized_keys.
    {
        my @ak_lines = ();
        ## first read in the existing authorized_keys.
        if (-e "/root/.ssh/authorized_keys") {
            open(my $ak, '/root/.ssh/authorized_keys')
                or $self->fault("could not read /root/.ssh/authorized_keys: $!");
            @ak_lines = <$ak>;
            close($ak);
    
            ## remove existing warsync-server commands for this client.
            @ak_lines = grep(!/warsync-server \Q$client_name\E/, @ak_lines);
    
            ## remove lines matching client key.
            @ak_lines = grep(!/\Q$client_public_key\E/, @ak_lines);
        }
    
        open(my $ak, '> /root/.ssh/authorized_keys')
            or $self->fault("could not write /root/.ssh/authorized_keys: $!");
    
        print $ak @ak_lines;
    
        print $ak <<EOF;
# warsync-server $client_name command is used for Warsync client connections
command="/usr/sbin/warsync-server $client_name",no-port-forwarding,no-X11-forwarding,no-agent-forwarding $client_public_key
EOF
    
        close($ak);
    }

    ## test loop-back connection
    $self->loop_back_connection or
        $self->fault("could not make successful loop-back connection: $!");
    
    $server->display(1, "success.\n");
}

1;
