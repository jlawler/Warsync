package Warsync::Client;

use warnings;
use strict;

use Warsync::Host;
use Warsync::Util;

our @ISA = qw( Warsync::Host );

sub new
{
    my $self = shift;
    my $class = ref($self) || $self;
    return $self if ref $self;

    ## only return object if this is a client system.
    ## check that we are a client by looking for the client
    ## authentication key and config file.
    unless (-r '/etc/warsync/client-key' and -r '/etc/warsync/client.conf') {
        return undef;
    }

    $self = bless {} => $class;

    ## okay now parse the client config file.
    open(my $conf_fh, '/etc/warsync/client.conf')
        or $self->fault("could not open client config file: $!");


    while (<$conf_fh>) {
        chomp;                  # no newline
        s/#.*//;                # no comments
        s/^\s+//;               # no leading white
        s/\s+$//;               # no trailing white
        next unless length;     # anything left?
        my ($var, $value) = split(/\s*=\s*/, $_, 2);
        $self->{ lc($var) } = $value;
    }

    close($conf_fh);

    ## must have a server.
    $self->fault("no server configured.") unless $self->server;

    return $self;
}

sub compress
{
    my $self = shift->new;

    if (@_) {
        $self->{compress} = shift;
        return $self;
    }

    return $self->{compress};
}

sub package_install
{
    my $self     = shift->new;
    my @packages = @_;

    my $v = $self->verbose;

    my @apt = 'apt-get';
    push @apt, '--dry-run' if $self->dryrun;

    print STDERR "dry-run: apt-get install @packages\n" if $self->dryrun;

    my ($vv, $qq, $y);

    $vv = '-u'  if $v > 1;
    $qq = '-q'  unless tty;
    $qq = '-qq' unless $v;
    $y  = '-y'  unless tty;

    push @apt, $qq, $vv, $y, 'install', @packages;

    @apt = map { /(.+)/ } grep { defined } @apt;

    $self->system_call(@apt)
        or $self->fault("something went wrong running apt-get install: code ", $? >> 8);
}

sub package_purge
{
    my $self     = shift->new;
    my @packages = @_;

    my @dpkg = 'dpkg';
    push @dpkg, '--no-act' if $self->dryrun;

    print STDERR "dry-run: purge @packages\n" if $self->dryrun;

    push @dpkg, '--purge', @packages;

    @dpkg = map { /(.+)/ } @dpkg;

    $self->system_call(@dpkg)
        or $self->fault("something went wrong running dpkg --purge: code ", $? >> 8);
}


sub package_list
{
    my $self = shift->new;
    my $v    = shift;

    my $packages = $self->packages($v);

    my $list = '';

    foreach my $pkg (sort keys %$packages) {
        my $p = $packages->{$pkg};
        $list .= $p->[0] . ' ' . $p->[1] . ' '
               . $p->[2] . ' ' . $p->[3] . "\n";
    }

    return $list;
}

sub public_key
{
    my $self = shift->new;

    return `cat /etc/warsync/client-key.pub`;
}

sub server
{
    my $self = shift->new;

    return is_fqdn $self->{server};
}

sub server_port
{
    my $self = shift->new;

    my $unsafe = $self->{port} || 22;

    my ($port) = $unsafe =~ /(\d+)/;

    return $port || 22;
}

sub server_command
{
    my $self = shift->new;
    my @cmd  = @_;

    my @scmd = ( 'ssh', '-p', $self->server_port,
                        '-o', 'NumberOfPasswordPrompts=0',
                        '-i', '/etc/warsync/client-key'    );

    push @scmd, '-C' if $self->compress;

    push @scmd, $self->server, @cmd;

    open(my $ssh_h, '-|') || exec( @scmd )
        or $self->fault("error '$!' trying to exec: @scmd");

    return $ssh_h;
}

sub server_name
{
    my $self = shift->new;

    my $server_h = $self->server_command('warsync-echo-name');

    my @scom = <$server_h>;

    close($server_h);

    if ($scom[0]) {
        my ($name) = $scom[0] =~ /^name: (\S+)/;
        return $name if $name;
    }

    $self->fault("could not retrieve server name.");
}

sub slurp_file
{
    my $self = shift->new;
    my $file = shift;
    
    $self->display(3, "c-echofile: $file\n");

    my $output = '';

    open(my $fh, "< $file")
        or return $output;
    { local $/ = undef; $output = <$fh> }
    close($fh);
    return $output;
}

sub system_call
{
    my $self = shift->new;

    $self->display(3, "c-syscall: @_\n");

    return system(@_) ? 0 : 1;
}

## echo-file command. this simple returns the contents of a file on
## the client filesystem.
sub rpc_echo_file
{
    my $self = shift->new;
    print $self->slurp_file(@_);
}

## echo-name command. this simply echo's the client's hostname.
sub rpc_echo_name
{
    my $self = shift->new;
    print 'name: ', $self->name, "\n";
}

## echo-client-public-key command. this sends the contents
## of /etc/warsync/client-key.pub.
sub rpc_echo_client_public_key
{
    my $self = shift->new;
    print $self->public_key;
}

## warsync-echo-server-name command. this causes the client to try
## and connect to the server to ask the server it's hostname.
## it then echos back what the server told it.
sub rpc_echo_server_name
{
    my $self = shift->new;
    print 'server-name: ', $self->server_name, "\n";
}

## warsync-echo-package-list command. this causes the client
## to send list of currently installed debian packages.
sub rpc_echo_package_list
{
    my $self = shift->new;
    print $self->package_list;
}

## report current protocol version.
sub rpc_echo_protocol_version
{
    my $self = shift->new;
    print "$Warsync::PROTOCOL\n";
}

## warsync-sync-apt-conf command. this writes received
## data to /etc/apt/apt.conf file.
sub rpc_sync_apt_conf
{
    my $self = shift->new;

    my @data = <STDIN>;

    if ($self->dryrun) {
        print STDERR "dry-run: syncronize /etc/apt/apt.conf\n";
    }
    else {
        open(my $fh, '> /etc/apt/apt.conf')
            or $self->fault("could not write apt.conf: $!");
        print $fh @data;
        close($fh);
    }

    print "success\n";
}

## warsync-sync-apt-preferences command. this writes received
## data to /etc/apt/preferences file.
sub rpc_sync_apt_preferences
{
    my $self = shift->new;

    my @data = <STDIN>;

    if ($self->dryrun) {
        print STDERR "dry-run: syncronize /etc/apt/preferences\n";
    }
    else {
        open(my $fh, '> /etc/apt/preferences')
            or $self->fault("could not write preferences: $!");
        print $fh @data;
        close($fh);
    }

    print "success\n";
}

## warsync-sync-apt-sources command. this writes received
## data to /etc/apt/sources.list file.
sub rpc_sync_apt_sources
{
    my $self = shift->new;

    my @data = <STDIN>;

    if ($self->dryrun) {
        print STDERR "dry-run: syncronize /etc/apt/sources.list\n";
    }
    else {
        open(my $fh, '> /etc/apt/sources.list')
            or $self->fault("could not write to sources.list: $!");
        print $fh @data;
        close($fh);
    }

    print "success\n";
}

## warsync-apt-get-update command. this causes the client
## to run apt-get update.
sub rpc_apt_get_update
{
    my $self = shift->new;

    if ($self->dryrun) {
        print STDERR "dry-run: execute apt-get update\n";
        return;
    }

    my @apt = 'apt-get';

    my $qq;

    $qq = '-q'  unless tty;
    $qq = '-qq' unless $self->verbose;

    push @apt, $qq if defined($qq);

    push @apt, 'update';

    $self->system_call(@apt)
        or $self->fault("something went wrong running apt-get update: $?");
}

## warsync-apt-get-install command. this causes the client
## to install or remove debian packages.
sub rpc_package_install
{
    my $self = shift->new;
    $self->package_install(@_);
}

## warsync-apt-get-purge command. this causes the client
## to purge debian packages.
sub rpc_package_purge
{
    my $self = shift->new;
    $self->package_purge(@_);
}

## handles rsync command sent by server.
sub rpc_rsync
{
    my $self = shift->new;
    my @cmd = map { /(.+)/ } @_;
    exec('rsync', @cmd)
        or $self->fault("rsync failed: $!");
}

## searches for .warsyncignore files in given paths.
sub rpc_find_warsyncignore_files
{
    my $self = shift->new;

    ## untaint and verify directories.
    my @paths = map { my ($dir) = $_ =~ /^(\/[-_.\/0-9A-Za-z]+)/;
                      -d $dir ? $dir : () } @_;
    return unless @paths;
    
    $self->display(3, "c-findignore: @paths\n");
    
    exec('find', @paths, '-name', '.warsyncignore')
        or $self->fault("find .warsyncignore files failed: $!");
}    

sub rpc_remote_command {
    my $self = shift->new;
    my $cmd = "@_";

    my $v = $self->verbose;
    
    if ($self->dryrun) {
        print STDERR "dry-run: $cmd\n";
    }
    else {
        my ($blindly_run_cmd) = $cmd =~ /(.+)/;
        unless ($self->system_call($blindly_run_cmd)) {
            $self->exit_code($?);
            $self->fault("something went wrong running $blindly_run_cmd: code ", $? >> 8);
        }
    }
}

1;
