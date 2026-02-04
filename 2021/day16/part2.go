package day16

import (
	"bufio"
	"fmt"
	"io"
)

func Part2(r *bufio.Reader, w io.Writer) error {
	var err error
	var packets []Packet

	if packets, err = ReadPackets(r); err != nil {
		return err
	}

	fmt.Fprintln(w, packets[0].Value())

	return nil

}
