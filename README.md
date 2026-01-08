<!--; export this_file=$(readlink -f $0); export script_lines=60; sed -n 2,${script_lines}p $this_file | sh; exit
this_dir=$(dirname $this_file)
tmp_file=$(mktemp $this_file.XXXXX)
head -n $((script_lines + 1)) $this_file > $tmp_file
printf "As of \`$(date --rfc-3339=seconds --utc)\` the following puzzles are solved:\n\`\`\`\n" >> $tmp_file
find $this_dir -type f -name puzzle.out | gawk '
BEGIN {
  FS = "/"
  PROCINFO["sorted_in"] = "@ind_str_desc"
  solved_char = "▓"
  unsolved_char = "░"
  num_days[2025] = 12
}
{
  year = int($(NF - 3))
  if (match($(NF - 1), "([0-2][0-9]).+([12]$)|(25|12)", m) != 0) {
    if (m[3] != "") {
      day = int(m[3])
      part = 1
    } else {
      day = int(m[1])
      part = int(m[2])
    }
    part_solved[year, day, part] = 1
    year_solved[year] += 1
  } else {
    print "Unexpected filename " $0 > "/dev/stderr"
  }
}
END {
  printf("%6s", "")
  for (i = 1; i <= 25; ++i) printf(" %02d", i)
  printf("\n")
  for (year in year_solved) {
    printf("[%04d] ", year)
    if (num_days[year] == "") num_days[year] = 25
    for (day = 1; day <= 25; ++day) {
      if (day > num_days[year]) {
        printf("-- ")
        continue
      }
      for (part = 1; part <= 2; ++part) {
        if (day == num_days[year] && part == 2 && year_solved[year] == (2 * num_days[year] - 1)) {
          part_solved[year, day, 2] = 1
          year_solved[year] += 1
        }
        if (part_solved[year, day, part]) {
          printf(solved_char)
        } else {
          printf(unsolved_char)
        }
      }
      printf(" ")
    }
    printf("%02d\n", year_solved[year])
  }
}
' >> $tmp_file
tail -n 1 $this_file >> $tmp_file
mv $tmp_file $this_file
-->
As of `2026-01-08 00:59:57+00:00` the following puzzles are solved:
```
       01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
[2025] ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ -- -- -- -- -- -- -- -- -- -- -- -- -- 24
[2024] ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ 50
[2023] ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓░ ▓▓ ▓▓ ▓▓ ▓░ 48
[2022] ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ 50
[2015] ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ ▓▓ 50
```
