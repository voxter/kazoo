#!/usr/bin/perl

use DBI;

my %config;
my $dbh;

$config{'dbhost'} = 'localhost';
$config{'dbname'} = 'qstats';
$config{'dbuser'} = 'qstats';
$config{'dbpass'} = 'some_password';

$recording_directory="/var/www/html/recordings";

sub connect_db() {
    my $return = 0;
    my %attr = (
        PrintError => 0,
        RaiseError => 0,
    );
    my $dsn = "DBI:mysql:database=$config{'dbname'};host=$config{'dbhost'}";
    $dbh->disconnect if $dbh;
    $dbh = DBI->connect( $dsn, $config{'dbuser'}, $config{'dbpass'}, \%attr ) or $return = 1;
    return $return;
}

&connect_db();

my $in       = $ARGV[0];
my $out      = $ARGV[1];
my $archivo  = $ARGV[2];
my $uniqueid = $ARGV[3];

my $archimp3 = $archivo;
$archimp3 =~ s/\.wav/\.mp3/g;
$archimp3 =~ s/\.gsm/\.mp3/g;

my $intmp = $in;
$intmp =~ s/\.wav//g;
$intmp =~ s/\.gsm//g;

my $outtmp = $out;
$outtmp =~ s/\.wav//g;
$outtmp =~ s/\.gsm//g;

my $archifin = $archimp3;
$archifin =~ s/(.*)\/(.*)/$2/g;

my $query = "INSERT INTO recordings VALUES('$uniqueid','$archifin')";
$dbh->do($query);
$dbh->disconnect if $dbh;

system("/usr/bin/sox -c 1 $in $intmp-tmp.wav pan -1");
system("/usr/bin/sox -c 1 $out $outtmp-tmp.wav pan 1");
system("/usr/bin/soxmix -v 0.5 $intmp-tmp.wav $outtmp-tmp.wav $archivo");
system("/usr/bin/lame --silent --resample 44.1 --tt $archivo --add-id3v2 $archivo $archimp3");
system("cp $archimp3 $recording_directory");
system("rm -rf $intmp-tmp.wav");
system("rm -rf $outtmp-tmp.wav");
system("rm -rf $in");
system("rm -rf $out");
system("rm -rf $archivo");
system("rm -rf $archimp3");

