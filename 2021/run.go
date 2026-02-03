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
