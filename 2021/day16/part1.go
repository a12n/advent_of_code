package day16

import (
	"bufio"
	"bytes"
	"encoding/hex"
	"fmt"
	"io"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var packets []Packet

	if packets, err = ReadPackets(r); err != nil {
		return err
	}

	fmt.Fprintln(w, VersionSum(packets))

	return nil
}

func ReadPackets(r *bufio.Reader) ([]Packet, error) {
	var encoded, decoded []byte
	var err error
	var packets []Packet

	if encoded, err = r.ReadBytes('\n'); err != nil {
		return nil, err
	}

	encoded = bytes.TrimSpace(encoded)
	decoded = make([]byte, hex.DecodedLen(len(encoded)))
	if _, err = hex.Decode(decoded, encoded); err != nil {
		return nil, err
	}

	if packets, err = DecodePackets(NewBitStream(decoded)); err != nil {
		return nil, err
	}

	return packets, err
}

func VersionSum(packets []Packet) int {
	var packet Packet
	var sum int

	for _, packet = range packets {
		var ok bool
		var operator Operator

		sum += packet.Version()

		if operator, ok = packet.(Operator); ok {
			sum += VersionSum(operator.packets)
		}
	}

	return sum
}
