#!/usr/bin/perl

use Test;

%tests = ("1" => "1\n",
          "1+1" => "2\n");

plan tests => (scalar (keys %tests));

$ENV{REQUEST_METHOD} = 'GET';

for $expr (keys %tests) {
    open OUTPUT, "./links -s '$expr'|";
    $expr_str = <OUTPUT>;
    chomp($expr_str);
    $env_str = <OUTPUT>;
    chomp($env_str);
    close OUTPUT;
    $ENV{QUERY_STRING} = "?expression%=$expr_str&environment%=$env_str";
    open OUTPUT, "./links examples/null.links|";
    while (<OUTPUT> =~ /./) { }
    $orig_sep = $/;
    $/ = undef;
    $result = <OUTPUT>;
    $/ = $orig_sep;
    ok($result, $tests{$expr});
    close OUTPUT;
}
