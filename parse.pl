use strict;
use encoding 'utf8';
use FindBin '$Bin';
sub parse (&$);

my $actions = parse {
    my ($serial, $name, $turns, $effect, $flavor) = @_;

    $flavor = splitWords($flavor);
    $effect = splitWords($effect);

    return << ".";
    , Action $serial "$name" $turns "$effect"
        "$flavor"
.

} "actions";

my $skills = parse {
    my ($serial, $name, $effect, $flavor) = @_;

    $flavor = splitWords($flavor);
    $effect = splitWords($effect);

    return << ".";
    , Skill $serial "$name" "$effect"
        "$flavor"
.

} "skills";

my $environments = parse {
    my ($serial, $name, $effect, $flavor) = @_;

    $flavor = splitWords($flavor);
    $effect = splitWords($effect);

    return << ".";
    , Environment $serial "$name" "$effect"
        "$flavor"
.

} "environments";

my $students = parse {
    my ($serial, $name, $styles, $power, $topics, $paralyzed, $flavor) = @_;
    $styles = join ',', map {/[a-z]/ ? "Anti ".uc($_) : $_ } sort split(//, $styles);

    my ($p, $t) = split(/\//, $power);
    $t ||= $p;
    $p = 100 if $p eq 'x';
    $t = 100 if $t eq 'x';
    $topics = '' if $topics eq '*';

    $flavor = splitWords($flavor);
    $topics = join ',', sort split(//, lc $topics);
    $paralyzed = join ',', sort split(//, lc $paralyzed);
    
    return << ".";
    , Student $serial "$name" [$styles] $p $t [$topics] [$paralyzed]
        "$flavor"
.
} "students";

my $lessons = parse {
    my ($serial, $name, $styles, $power, $topics, $abilities, $flavor) = @_;
    $styles = join ',', map {/[a-z]/ ? "Anti ".uc($_) : $_ } sort split(//, $styles);

    my ($p, $t) = split(/\//, $power);
    $t ||= $p if $t ne '0';
    $p = 100 if $p eq 'x';
    $t = 100 if $t eq 'x';
    $topics = '' if $topics eq '*';

    $flavor = splitWords($flavor);
    $topics = join ',', sort split(//, lc $topics);
    $abilities = join ',', sort split(//, lc $abilities);
    
    return << ".";
    , Lesson $serial "$name" [$styles] $p $t [$topics] [$abilities]
        "$flavor"
.
} "lessons";

my $assistants = parse {
    my ($serial, $name, $styles, $topics, $abilities, $cost, $flavor) = @_;
    $styles = join ',', map {/[a-z]/ ? "Anti ".uc($_) : $_ } sort split(//, $styles);

    $flavor = splitWords($flavor);
    $topics = join ',', sort split(//, lc $topics);
    $abilities = join ',', sort split(//, lc $abilities);
    
    return << ".";
    , Assistant $serial "$name" [$styles] [$topics] [$abilities] $cost
        "$flavor"
.
} "assistants";

#######

open FH, '>:utf8', 'data.hs';
print FH $actions, $students, $lessons, $skills, $environments, $assistants;
close FH;

#######

sub splitWords {
    my $words = shift;
    my @chunks = split(/(?<=\W)/, $words);
    my $len = 0;
    my $out = '';
    while (my $chunk = shift @chunks) {
        $len += length $chunk;
        if ($len > 18) {
            $len = length $chunk;
            $out .= '\\n';
        }
        $out .= $chunk;
    }
    return $out;
}

sub parse (&$) {
    my ($code, $type) = @_;
    open my $fh, '<:utf8', "$Bin/data/$type.txt" or die $!;

    my $out = << ".";

$type =
.

    my $serial = 0;
    while (<$fh>) {
        chomp;
        $serial++;
        my @fields = split(/\s*\|\s*/, $_) ;
        shift @fields;
        $out .= $code->($serial, @fields);
    }

    $out =~ s/,/[/ or $out .= "   [\n";
    $out .= "   ]\n";

    return $out;
}

