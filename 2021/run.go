package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path"
	"runtime/pprof"

	"a12n/advent_of_code/2021/day01"
	"a12n/advent_of_code/2021/day02"
	"a12n/advent_of_code/2021/day03"
	"a12n/advent_of_code/2021/day04"
	"a12n/advent_of_code/2021/day05"
	"a12n/advent_of_code/2021/day06"
	"a12n/advent_of_code/2021/day07"
	"a12n/advent_of_code/2021/day08"
	"a12n/advent_of_code/2021/day09"
	"a12n/advent_of_code/2021/day10"
	"a12n/advent_of_code/2021/day11"
	"a12n/advent_of_code/2021/day12"
	"a12n/advent_of_code/2021/day13"
	"a12n/advent_of_code/2021/day14"
	"a12n/advent_of_code/2021/day15"
	"a12n/advent_of_code/2021/day16"
	"a12n/advent_of_code/2021/day17"
	"a12n/advent_of_code/2021/day18"
	"a12n/advent_of_code/2021/day19"
	"a12n/advent_of_code/2021/day20"
	"a12n/advent_of_code/2021/day21"
	"a12n/advent_of_code/2021/day22"
	"a12n/advent_of_code/2021/day23"
	"a12n/advent_of_code/2021/day25"
)

func run() error {
	var debug bool
	var err error
	var prof string

	flag.BoolVar(&debug, "debug", debug, "enable debug output")
	flag.StringVar(&prof, "prof", prof, "write CPU profile to file")
	flag.Parse()

	if !debug {
		log.SetOutput(io.Discard)
	}

	if prof != "" {
		var f *os.File

		if f, err = os.Create(prof); err != nil {
			return err
		}
		defer f.Close()

		if err = pprof.StartCPUProfile(f); err != nil {
			return err
		}
		defer pprof.StopCPUProfile()
	}

	var day, part int

	if _, err = fmt.Sscanf(path.Base(os.Args[0]), "%02d-%1d", &day, &part); err != nil {
		return err
	}

	var dispatch = [25][2](func(*bufio.Reader, io.Writer) error){
		{day01.Part1, day01.Part2},
		{day02.Part1, day02.Part2},
		{day03.Part1, day03.Part2},
		{day04.Part1, day04.Part2},
		{day05.Part1, day05.Part2},
		{day06.Part1, day06.Part2},
		{day07.Part1, day07.Part2},
		{day08.Part1, day08.Part2},
		{day09.Part1, day09.Part2},
		{day10.Part1, day10.Part2},
		{day11.Part1, day11.Part2},
		{day12.Part1, day12.Part2},
		{day13.Part1, day13.Part2},
		{day14.Part1, day14.Part2},
		{day15.Part1, day15.Part2},
		{day16.Part1, day16.Part2},
		{day17.Part1, day17.Part2},
		{day18.Part1, day18.Part2},
		{day19.Part1, day19.Part2},
		{day20.Part1, day20.Part2},
		{day21.Part1, day21.Part2},
		{day22.Part1, day22.Part2},
		{day23.Part1, day23.Part2},
		{nil, nil},
		{day25.Part1},
	}

	return dispatch[day-1][part-1](bufio.NewReader(os.Stdin), os.Stdout)
}

func main() {
	var err error
	if err = run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
