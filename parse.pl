use strict;
use encoding 'utf8';
use FindBin '$Bin';
sub parse (&$);

my $actions = parse {
    my ($name, $turns, $effect, $flavor) = @_;

    $flavor = splitWords($flavor);
    $effect = splitWords($effect);

    return << ".";
    , Action "$name" $turns "$effect"
        "$flavor"
.

} "actions";

my $skills = parse {
    my ($name, $effect, $flavor) = @_;

    $flavor = splitWords($flavor);
    $effect = splitWords($effect);

    return << ".";
    , Skill "$name" "$effect"
        "$flavor"
.

} "skills";

my $environments = parse {
    my ($name, $effect, $flavor) = @_;

    $flavor = splitWords($flavor);
    $effect = splitWords($effect);

    return << ".";
    , Environment "$name" "$effect"
        "$flavor"
.

} "environments";

my $students = parse {
    my ($name, $styles, $power, $topics, $paralyzed, $flavor) = @_;
    $styles = join ',', sort split(//, $styles);

    my ($p, $t) = split(/\//, $power);
    $t ||= $p;
    $p = 0 if $p eq 'x';
    $t = 0 if $t eq 'x';
    $topics = '' if $topics eq '*';

    $flavor = splitWords($flavor);
    $topics = join ',', sort split(//, lc $topics);
    $paralyzed = join ',', sort split(//, lc $paralyzed);
    
    return << ".";
    , Student "$name" [$styles] $p $t [$topics] [$paralyzed]
        "$flavor"
.
} "students";

my $lessons = parse {
    my ($name, $styles, $power, $topics, $abilities, $flavor) = @_;
    $styles = join ',', sort split(//, $styles);

    my ($p, $t) = split(/\//, $power);
    $t ||= $p;
    $p = 0 if $p eq 'x';
    $t = 0 if $t eq 'x';
    $topics = '' if $topics eq '*';

    $flavor = splitWords($flavor);
    $topics = join ',', sort split(//, lc $topics);
    $abilities = join ',', sort split(//, lc $abilities);
    
    return << ".";
    , Lesson "$name" [$styles] $p $t [$topics] [$abilities]
        "$flavor"
.
} "lessons";

my $assistants = parse {
    my ($name, $styles, $topics, $abilities, $cost, $flavor) = @_;
    $styles = join ',', sort split(//, $styles);

    $flavor = splitWords($flavor);
    $topics = join ',', sort split(//, lc $topics);
    $abilities = join ',', sort split(//, lc $abilities);
    
    return << ".";
    , Assistant "$name" [$styles] [$topics] [$abilities] $cost
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

    while (<$fh>) {
        chomp;
        my @fields = split(/\s*\|\s*/, $_) ;
        shift @fields;
        $out .= $code->(@fields);
    }

    $out =~ s/,/[/ or $out .= "   [\n";
    $out .= "   ]\n";

    return $out;
}

