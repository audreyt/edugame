use strict;
use encoding 'utf8';
use FindBin '$Bin';

$_ = `curl -s http://www.socialtext.net/data/workspaces/audreyt/pages/edugame?accept=text/x.socialtext-wiki`;

my %map = qw(
    students 學生
    lessons 課程
    actions 行動
    skills 技藝
    environments 環境
    assistants 助教
);

utf8::upgrade($_);

while (my ($type, $word) = each %map) {
    /^\^+\s+$word\s+(?:\|\|.*\n)(?:\|.*\n)((?:\|.*\n)*)/m or warn "Cannot find $word";
    open FH, '>:utf8', "data/$type.txt" or die $!;
    print FH $1;
    close FH;
}
