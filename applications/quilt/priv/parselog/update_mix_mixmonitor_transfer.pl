#!/usr/bin/perl
use DBI;
use File::Path qw(mkpath);
use File::Basename;
use constant false => 0;
use constant true  => 1;
my %config;
my $dbh;

# CONFIGURATION
# You have to set the proper database credentials
$config{'dbhost'} = 'localhost';
$config{'dbname'} = 'qstats';
$config{'dbuser'} = 'root';
$config{'dbpass'} = '';

# Destination directory for recordings
$config{'asterisk_spool'}  = "/var/spool/asterisk/monitor";
$config{'destination_dir'} = "/var/spool/asterisk/asternic";

# If you want "wav" recordings to be converted to .mp3 
# It requires the lame tool to be installed.
$config{'convertmp3'} = false;

# Do not modify bellow this line
my $LAME = `which lame`;
chomp($LAME);

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

my $uniqueid             = $ARGV[0];
my $original_sound_file  = $ARGV[1];

# We add the spool path to the original sound file
$original_sound_file     =~ s/$config{'asterisk_spool'}//g;
$original_sound_file     =~ s/^\///g;
$original_sound_file     = $config{'asterisk_spool'}."/".$original_sound_file;

# Extract filename and suffix for later processing
my($filename, $directories, $suffix) = fileparse($original_sound_file, "\.[^.]*");
$filename_nosuffix = $filename;
$filename = $filename.$suffix;

# Set subdate destination directory
$time = localtime(time);
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$datesubdir = sprintf ("%4d-%02d-%02d",$year+1900,$mon+1,$mday);
my $dest_directory     = $config{'destination_dir'}."/".$datesubdir;

# Create destination directory
mkpath("$dest_directory");

# Set sql field
my $dest_sql           = $datesubdir."/".$filename;

if($suffix eq ".wav" && $config{'convertmp3'} == true && $LAME ne "" && -f $LAME) {
    # mp3 convertion if all conditions are met (config, lame installed, .wav file)
    my $file_mp3           = $filename_nosuffix.".mp3";
    my $dest_file_mp3      = $config{'destination_dir'}."/".$datesubdir."/".$file_mp3;
    my $temp_file_mp3      = $config{'asterisk_spool'}."/".$file_mp3;
    $dest_sql              = $datesubdir."/".$file_mp3;
    if ( -f $LAME ) {
        mkpath("$dest_directory");
        system("$LAME --silent -m m -b 8 --tt $original_sound_file --add-id3v2 $original_sound_file $temp_file_mp3");
        my $result = system("cp $temp_file_mp3 $dest_file_mp3");
        if($result==0) {
#            system("rm -f $original_sound_file");
        }
#        system("rm -f $temp_file_mp3");
    }
} else {
    # No convertion, just copy the file to destination directory
    my $dest_file_wav  = $config{'destination_dir'}."/".$datesubdir."/".$filename;
    my $result = system("cp $original_sound_file $dest_file_wav");
    if($result==0) {
#        system("rm -f $original_sound_file");
    }
}

# Update the DB

# First we check if we did not have one record already
my $query = "SELECT filename FROM recordings WHERE uniqueid='$uniqueid'";
$sth = $dbh->prepare($query);
$sth->execute;
my @result = $sth->fetchrow_array;
my $existe = @result;
$sth->finish;

if ($existe) {
    # We already have one uniqueid, look in disk for older file and update filename
    my $stored_file = $result[0];
    my($cerofile, undef, undef) = fileparse($filename, "\.[^.]*");
    my($unofile, undef, undef) = fileparse($stored_file, "\.[^.]*");
    my @partes = split(/-/,$cerofile);
    my $cerofechaguardada = $partes[1].$partes[2];
    @partes = split(/-/,$unofile);
    my $unofechaguardada = $partes[1].$partes[2];
    if($cerofechaguardada < $unofechaguardada) {
        # La fecha de este archivo es menor, actualizo la base
        $query = "UPDATE recordings SET filename='$dest_sql' WHERE uniqueid='$uniqueid'";
        $dbh->do($query);
    } else {
        # Si la fecha es mayor, lo ignoro
    }
}
else {
    # We do not have the uniqueid, insert new record
    $sth->finish;
    $query = "INSERT INTO recordings VALUES('$uniqueid','$dest_sql')";
    $dbh->do($query);
}


$dbh->disconnect if $dbh;


