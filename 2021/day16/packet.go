package day16

import (
	"fmt"
	"log"
	"math"
)

type Packet interface {
	Version() int
	TypeID() int
	Value() uint64
}

const (
	TypeLiteral     = 4
	TypeSum         = 0
	TypeProduct     = 1
	TypeMinimum     = 2
	TypeMaximum     = 3
	TypeGreaterThan = 5
	TypeLessThan    = 6
	TypeEqualTo     = 7
)

type AnyPacket struct {
	version, typeID uint8
}

func (p AnyPacket) Version() int {
	return int(p.version)
}

func (p AnyPacket) TypeID() int {
	return int(p.typeID)
}

type LiteralValue struct {
	AnyPacket
	value uint64
}

// Decode past header.
func (p *LiteralValue) Decode(bits *BitStream) error {
	log.Printf("LiteralValue.Decode: bits %v %d", bits, bits.Size())

	var nBits = 64

	for ; nBits > 0; nBits -= 5 {
		if bits.Size() < 5 {
			return fmt.Errorf("only %d bits left", bits.Size())
		}

		var isLastGroup = (bits.Get(1) == 0)

		p.value = (p.value << 4) | bits.Get(4)

		log.Printf("LiteralValue.Decode: isLastGroup %v, value %04b", isLastGroup, p.value&0b1111)

		if isLastGroup {
			log.Printf("LiteralValue.Decode: value %d", p.value)
			return nil
		}
	}

	log.Printf("LiteralValue.Decode: done, bits %v %d", bits, bits.Size())

	return fmt.Errorf("too long literal value")
}

func (p LiteralValue) Value() uint64 {
	return p.value
}

type Operator struct {
	AnyPacket
	packets []Packet
}

// Decode past header.
func (p *Operator) Decode(bits *BitStream) error {
	log.Printf("Operator.Decode: bits %v %d", bits, bits.Size())

	var err error
	var packet Packet

	if bits.Get(1) == 1 {
		var n = int(bits.Get(11))

		log.Printf("Operator.Decode: n %d", n)

		for ; n > 0; n-- {
			if packet, err = DecodePacket(bits); err != nil {
				return err
			}
			p.packets = append(p.packets, packet)
		}
	} else {
		var nBits = int(bits.Get(15))

		log.Printf("Operator.Decode: nBits %d", nBits)

		if bits.Size() < nBits {
			return fmt.Errorf("need %d bits, but only %d in the stream", nBits, bits.Size())
		}

		for nBits = bits.Size() - nBits; bits.Size() > nBits; {
			if packet, err = DecodePacket(bits); err != nil {
				return err
			}
			p.packets = append(p.packets, packet)
		}

		log.Printf("Operator.Decode: bits.Size %d, nBits %d", bits.Size(), nBits)
	}

	log.Printf("Operator.Decode: done, bits %v %d", bits, bits.Size())

	return nil
}

func (p Operator) Value() uint64 {
	var packet Packet
	var value uint64

	switch p.typeID {
	case TypeSum:
		for _, packet = range p.packets {
			value += packet.Value()
		}
	case TypeProduct:
		value = 1
		for _, packet = range p.packets {
			value *= packet.Value()
		}
	case TypeMinimum:
		value = math.MaxUint64
		for _, packet = range p.packets {
			value = min(value, packet.Value())
		}
	case TypeMaximum:
		for _, packet = range p.packets {
			value = max(value, packet.Value())
		}
	case TypeGreaterThan:
		if p.packets[0].Value() > p.packets[1].Value() {
			value = 1
		}
	case TypeLessThan:
		if p.packets[0].Value() < p.packets[1].Value() {
			value = 1
		}
	case TypeEqualTo:
		if p.packets[0].Value() == p.packets[1].Value() {
			value = 1
		}
	}

	return value
}

func DecodePacket(bits *BitStream) (Packet, error) {
	log.Printf("DecodePacket: bits %v %d", bits, bits.Size())

	if bits.Size() < 6 {
		return nil, fmt.Errorf("only %d bits left", bits.Size())
	}

	var err error
	var header AnyPacket

	header.version = uint8(bits.Get(3))
	header.typeID = uint8(bits.Get(3))

	log.Printf("DecodePacket: header %#v", header)

	switch int(header.typeID) {
	case TypeLiteral:
		var packet = LiteralValue{AnyPacket: header}
		if err = packet.Decode(bits); err != nil {
			return nil, err
		}
		return packet, nil
	case TypeSum, TypeProduct, TypeMinimum, TypeMaximum, TypeGreaterThan, TypeLessThan, TypeEqualTo:
		var packet = Operator{AnyPacket: header}
		if err = packet.Decode(bits); err != nil {
			return nil, err
		}
		return packet, nil
	default:
		return nil, fmt.Errorf("unknown type ID %d", header.typeID)
	}
}

func DecodePackets(bits *BitStream) ([]Packet, error) {
	var err error
	var packet Packet
	var packets []Packet

	// Size of a literal value packet with one group.
	const MinSize = 11

	for bits.Size() >= MinSize {
		if packet, err = DecodePacket(bits); err != nil {
			return nil, err
		}
		packets = append(packets, packet)
		log.Printf("DecodePackets: %d decoded", len(packets))
	}

	return packets, nil
}
