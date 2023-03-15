package util

import (
	"errors"
	"fmt"
	"math"
)

type PrintList struct {
	count int
	index int
	print []string
	value []int
}

func (pl *PrintList) Init(pt []string) {
	// if pl.count != 0 {
	// 	return
	// }
	pl.count = len(pt)
	pl.print = pt

	pl.index++

	for i := 0; i < pl.count; i++ {
		fmt.Printf("%-30s\t%-3v\n", pl.print[i], 0)
		pl.index++
		pl.value = append(pl.value, 0)
	}
}

func (pl *PrintList) Print(idx int, vl int) error {
	// \033[nA 上移动n行 B 下移
	if idx >= pl.count {
		return errors.New("索引超长")
	}
	newIndex := pl.index - idx

	pl.value[idx-1] += vl

	if newIndex > 0 {
		fmt.Printf("\r\033[%vA%-30s\t%-3v", math.Abs(float64(newIndex)), pl.print[idx-1], pl.value[idx-1])
	} else if newIndex < 0 {
		fmt.Printf("\r\033[%vB%-30s\t%-3v", math.Abs(float64(newIndex)), pl.print[idx-1], pl.value[idx-1])
	} else {
		fmt.Printf("\r%-30s\t%-3v", pl.print[idx-1], pl.value[idx-1])
	}

	pl.index = idx

	return nil
}

func (pl *PrintList) End() {
	if pl.index < pl.count {
		fmt.Printf("\r\033[%vB", (pl.count - pl.index + 1))
	}
	pl.index = 0
	pl.value = nil
	pl.count = 0
	pl.print = nil
}
