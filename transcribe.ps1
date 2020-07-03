

$files = ls tests/*.tests

$nl = "
"

function New-List() {
    New-Object Collections.Generic.List[Object]
}

function Format-Args($argstr) {
    if ($argstr.Length -gt 0) {
        $largs = "[";
        foreach ($larg in $argstr.Split(' ')) {
            if ($largs.Length -gt 1) {
                $largs += "; ";
            }
            $larg = $larg.Replace("`"", "\`"");
            $largs += "`"$larg`"";
        }
        $largs += "]";
        $largs
    } else {
        ""
    }
}

function Fix-File($file) {
    $outfile = Split-Path -LeafBase $file
    $outfile = $outfile.Replace("-", "_")
    $outfile = Join-Path "expect" "$outfile.ml"


    $globalconf
    $tests = New-List
    $current = New-List
    foreach ($line in (Get-Content $file).Split($nl)) {
        if ($line -eq "") {
            if ($current[0].StartsWith("---")) {
                $globalconf = $current[1..($current.Count-2)]
            } else {
                $tests.Add($current)
            }
            $current = New-List
        } else {
            $current.Add($line)
        }
    }

    $gargs = @{}
    foreach ($arg in $globalconf) {
        $parts = $arg.Split(':')
        $gargs.Add($parts[0].Trim(), $parts[1].Trim())
    }


    New-Item $outfile -Force
    Set-Content $outfile "open Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location

"

    if ($gargs.config -ne $null) {
        $conf = $gargs.config
        Add-Content $outfile "let config = Some (`"$conf`")
"
        $gconfig = " ~config"
    } else {
        $gconfig = ""
    }

    foreach ($test in $tests) {
        $name = $test[0].Replace("`"", "\`"")
        $code = $test[1]
        $argsraw = $test[2..$test.Count]
        $args = @{}
        foreach ($arg in $argsraw) {
            $parts = $arg.Split(':')
            $args.Add($parts[0].Trim(), $parts[1].Trim())
        }

        if ($args.args -ne $null) {
            $argparts = $args.args.Split("-- ")
            if ($argparts.Count -gt 1) {
                $largs = Format-Args ($argparts[0].Trim())
                $pargs = Format-Args ($argparts[1].Trim())
            } else {
                $largs = Format-Args ($argparts[0].Trim())
                $pargs = ""
            }

            if ($largs.Length -gt 0) {
                $largs = " ~args:" + $largs
            }
            if ($pargs.Length -gt 0) {
                $pargs = " ~pargs:" + $pargs
            }
        } else {
            $largs = ""
        }

        if ($args.filemode -eq "true") {
            Add-Content $outfile "let%expect_test ""$name"" =
  run_file$gconfig$largs {|$code|}$pargs
"
        } else {
            Add-Content $outfile "let%expect_test ""$name"" =
  run_expr$gconfig$largs {|$code|}$pargs
"
        }
    }
}

function Fix-Files($file) {
    $files = ls tests/*.tests
    foreach ($file in $files) {
        Fix-File $file
    }
}

function Make-Fast() {
    $files = ls tests/*.tests

    foreach($test in $files) {
        $base = Split-Path -LeafBase $test
        $base = $base.Replace("-", "_")

        mkdir -p "expect/$base"
        mv "expect/$base.ml" "expect/$base/test.ml"

        $target = "expect/$base/test.ml"
        $content = Get-Content $target
        Set-Content $target (
            $content.Replace("open Test_common", "open Links_expect.Test_common"))

        Set-Content "expect/$base/dune" "(library
 (name links_expect_$base)
 (libraries base core core_kernel ppx_expect.payload
   links_expect
   ppx_expect.config ppx_expect.config_types
   ppx_expect.common ppx_expect.evaluator
   ppx_expect.matcher ppx_inline_test
   ppx_inline_test.config)
 (inline_tests)
 (preprocess (pps ppx_expect)))"
    }
    mkdir expect
}
