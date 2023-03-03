package util

import (
	"fmt"
	"strings"
)

type Bar struct {
	percent int64 // 百分比
	curr    int64
	total   int64
	grap    string
	show    string
	IsInit  bool
}

func (bar *Bar) Init(total int64, grap string) {
	bar.IsInit = true
	bar.total = total
	if grap == "" {
		bar.grap = "#"
	}
	bar.show = strings.Repeat(bar.grap, 100)
}

func (bar *Bar) percents() {
	bar.percent = int64(float32(bar.curr) / float32(bar.total) * 100)
}

func (bar *Bar) Add(desc string) {
	if !bar.IsInit {
		return
	}

	bar.curr++

	bar.percents()

	fmt.Printf("\r[%-50s]%3d%%  %8d/%d %-10s", bar.show[:bar.percent/2], bar.percent, bar.curr, bar.total, desc)
}

func (bar *Bar) End() {
	if !bar.IsInit {
		return
	}

	fmt.Println()
}
