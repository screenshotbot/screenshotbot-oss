#!/usr/bin/perl -w

use IO::Socket;
use Net::hostent;              # for OO version of gethostbyaddr

$PORT = 2931;                  # pick something not in use

$server = IO::Socket::INET->new( Proto     => 'tcp',
				 LocalPort => $PORT,
				 Listen    => SOMAXCONN,
				 Reuse     => 1);

die "can't setup server" unless $server;
print "[Server $0 accepting clients]\n";

while ($client = $server->accept()) {
    $client->autoflush(1);
    $hostinfo = gethostbyaddr($client->peeraddr);
    printf "[Connect from %s]\n", $hostinfo->name || $client->peerhost;
    while ( <$client>) {
	chomp;
	print "command: [$_]\n";
	next unless /\S/;       # blank line
	if (/translate (.*)/i) {
	    syswrite($client, "$1\n");
	} else {
	    print "unknown command ignored: $_\n";
	}
    }
    close $client;
}
