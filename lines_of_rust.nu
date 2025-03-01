#!/usr/bin/env nu

def test_for_exe [exe: string]: nothing -> bool {
  (which $exe | length) > 0
}
export def lines_of_rust [
  --no-sloc(-l) # Force LOC instead of SLOC
]: nothing -> table {
  let has_tokei = (test_for_exe tokei) and (not $no_sloc)
  if not $has_tokei {
    let rust_source = ls **/src/**/*.rs
    $rust_source | insert lines {|| open $in.name | lines | length} | sort-by -r lines | select name lines size
  } else {
    let tokei_out = (^tokei ...(glob **/src/**/*.rs) --output json | from json)
    $tokei_out |
      get Rust.reports |
      each {||
        $in | flatten -a | get 0 | rename -c {code: lines}
      } |
      update name {|| $in | path relative-to (pwd) } |
      select name lines comments blanks |
      sort-by -r lines
  }
}

def main [
  --no-sloc(-l) # Force LOC calculation instead of SLOC
  --full(-f) # Merge SLOC and LOC outputs
  --nuon(-d) # Output as nuon
] {
  # Get number of lines of code
  # Including comments and everything
  let has_tokei = (test_for_exe tokei) and (not $no_sloc);
  if $full and $has_tokei {
    let data = (lines_of_rust |
      rename -c {lines: sloc} |
      join (lines_of_rust -l |
      rename -c {lines: loc}) name |
      sort-by -r sloc |
      # Specify column ordering
      select name sloc loc comments blanks size)
    if $nuon {
      $data | to nuon
    } else {
      $data
    }
  } else {
    let data = (lines_of_rust --no-sloc=$no_sloc)
    if $nuon {
      $data | to nuon
    } else {
      if $has_tokei {
        print "Found tokei! Calculating SLOC instead"
      }
      print $'Lines of Rust: ($data | get lines | math sum)'
      $data
    }
  }
}
