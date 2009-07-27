use strict;
use encoding 'utf8';
use FindBin '$Bin';

my $students = << '.';

students =
.
open STUDENTS, "$Bin/data/students.txt";
while (<STUDENTS>) {
    chomp;
    my (undef, $name, $styles, $power, $topics, $paralyzed, $flavor) = split(/\s*\|\s*/, $_);
    $styles = join ',', sort split(//, $styles);

    my ($p, $t) = split(/\//, $power);
    $t ||= $p;
    $p = 0 if $p eq 'x';
    $t = 0 if $t eq 'x';
    $topics = '' if $topics eq '*';

    my @flavor = split(/(?<=\W)/, $flavor);
    my $len = 0;
    $flavor = '';
    while (my $chunk = shift @flavor) {
        $len += length $chunk;
        if ($len > 18) {
            $len = length $chunk;
            $flavor .= '\\n';
        }
        $flavor .= $chunk;
    }

    $topics = join ',', sort split(//, lc $topics);
    $paralyzed = join ',', sort split(//, lc $paralyzed);
    
    $students .= << ".";
    , Student "$name" [$styles] $p $t [$topics] [$paralyzed]
        "$flavor"
.

}

$students =~ s/,/[/ or $students .= "   [\n";
$students .= "   ]\n";

my $lessons = << '.';

lessons =
.
open LESSONS, "$Bin/data/lessons.txt";
while (<LESSONS>) {
    chomp;
    my (undef, $name, $styles, $power, $topics, $ability, $flavor) = split(/\s*\|\s*/, $_);
    $styles = join ',', sort split(//, $styles);

    my ($p, $t) = split(/\//, $power);
    $t ||= $p;
    $p = 0 if $p eq 'x';
    $t = 0 if $t eq 'x';
    $topics = '' if $topics eq '*';

    my @flavor = split(/(?<=\W)/, $flavor);
    my $len = 0;
    $flavor = '';
    while (my $chunk = shift @flavor) {
        $len += length $chunk;
        if ($len > 18) {
            $len = length $chunk;
            $flavor .= '\\n';
        }
        $flavor .= $chunk;
    }

    $topics = join ',', sort split(//, lc $topics);
    $ability = join ',', sort split(//, lc $ability);
    
    $lessons .= << ".";
    , Lesson "$name" [$styles] $p $t [$topics] [$ability]
        "$flavor"
.

}

$lessons =~ s/,/[/ or $lessons .= "   [\n";
$lessons .= "   ]\n";

open FH, '>:utf8', 'data.hs';
print FH $students, $lessons;
