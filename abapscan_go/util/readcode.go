package util

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

type Relation struct {
	Table       []string `json:"table,omitempty"`
	TableUsed   []string `json:"tableUsed,omitempty"`
	Cds         []string `json:"cds,omitempty"`
	ProxyClass  []string `json:"proxyClas,omitempty"`
	Function    []string `json:"function,omitempty"`
	Struct      []string `json:"struct,omitempty"`
	Lock        []string `json:"lock,omitempty"`
	AuthCheck   []string `json:"authCheck,omitempty"`
	Transaction []string `json:"transaction,omitempty"`
}

type KeyValue struct {
	path      string
	Main      string
	index     int
	Relations Relation `json:"relations,omitempty"`
}

var Plt PrintList

func ReadFile(pwd string) []KeyValue {

	_, err := os.Stat(pwd)
	if err != nil {
		fmt.Println(pwd, " no exists")
		return nil
	}

	abapTables := []KeyValue{}

	Plt.Init([]string{
		"table",
		"tableUsed",
		"cds",
		"proxyClas",
		"function",
		"struct",
		"lock",
		"authCheck",
		"transaction",
	})

	// fmt.Println("\n处理文件: ")
	filepath.Walk(pwd, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}

		if !info.IsDir() {
			// fmt.Println(path) //打印path信息
			// fmt.Println(info.Name()) //打印文件

			if info.Name() == ResultName {
				return nil
			} else {
				// fmt.Printf("\n%s:\n", info.Name())
			}

			abapTable := KeyValue{Main: info.Name(), path: path}

			err := read(&abapTable)
			if err != nil {
				fmt.Println("Read:", err)
				return nil
			}

			if abapTable.index > 0 {
				abapTables = append(abapTables, abapTable)
			}
		}
		return nil
	})

	Plt.End()

	return abapTables
}

func read(kv *KeyValue) error {

	file, err := os.Open(kv.path)
	if err != nil {
		return err
	}

	defer file.Close()

	reader := bufio.NewReader(file)

	for {
		line, _, err := reader.ReadLine()
		if err == io.EOF {
			break
		}

		str := string(line)

		table := Relation{}

		regex := `(?i)` +
			`(?: +(ZT(?:[PM]{2}|SD|FI|API|JOB)\d{3}\w*))|` +
			// `(?: +(?:(?:MODIFY)|(?:INSERT)|(?:UPDATE))(?: +((?:ZT[PM]{2}|ZTSD|ZTFI)\d{3}\w*)))|` +
			`(?: +(ZV.*?_?(?:(?:DDL)|(?:VIEW))))|` +
			`(ZCO_SI_IF\d{3}\w+)|` +
			`(?:CALL +FUNCTION +'(Z\w*)')|` +
			`(?: +(Z(?:S|TT)(?:[PM]{2}|SD|FI|XX)\d{3}\w*))|` +
			`(?: +AUTHORITY-CHECK +OBJECT +'(.*?)')|` +
			`(?:CALL +TRANSACTION[ \n]+'(.*?)')`

		reg := regexp.MustCompile(regex)

		sub := reg.FindAllStringSubmatch(str, -1)

		if len(sub) > 0 {
			for rp := range sub {
				for s := range sub[rp] {
					if s > 0 && len(sub[rp][s]) > 0 {
						kv.index++

						// 显示
						scanPrint(s, sub[rp][s])

						value := strings.ToUpper(sub[rp][s])

						switch s {
						case 1:
							table.Table = append(table.Table, value)
							Plt.Print(1, 1)
						case 2:
							table.Cds = append(table.Cds, value)
							Plt.Print(3, 1)
						case 3:
							table.ProxyClass = append(table.ProxyClass, value)
							Plt.Print(4, 1)
						case 4:
							table.Function = append(table.Function, value)
							Plt.Print(5, 1)
						case 5:
							table.Struct = append(table.Struct, value)
							Plt.Print(6, 1)
						case 6:
							table.AuthCheck = append(table.AuthCheck, value)
							Plt.Print(8, 1)
						case 7:
							table.Transaction = append(table.Transaction, value)
							Plt.Print(9, 1)
						}

					}
				}
			}
		}

		// 使用的表
		if len(table.Table) != 0 {
			regex = `(?i)` +
				`(?: +(?:(?:MODIFY)|(?:INSERT)|(?:UPDATE))(?: +((?:ZT[PM]{2}|ZTSD|ZTFI)\d{3}\w*)))`
			reg = regexp.MustCompile(regex)
			sub = reg.FindAllStringSubmatch(str, -1)
			if len(sub) > 0 {
				for rp := range sub {
					for s := range sub[rp] {
						if len(sub[rp][s]) > 0 {
							kv.index++

							// 显示
							scanPrint(s, sub[rp][1])

							Plt.Print(2, 1)

							table.TableUsed = append(table.TableUsed, strings.ToUpper(sub[rp][1]))
						}
					}
				}
			}
		}

		// 锁情况
		regex = `(?:'(?:ENQUEUE_|DEQUEUE_)(\w+)')` // 不忽略大小写
		reg = regexp.MustCompile(regex)
		sub = reg.FindAllStringSubmatch(str, -1)
		if len(sub) > 0 {
			for rp := range sub {
				for s := range sub[rp] {
					if s > 0 && len(sub[rp][s]) > 0 {
						kv.index++

						// 显示
						scanPrint(s, sub[rp][1])
						Plt.Print(7, 1)

						table.Lock = append(table.Lock, strings.ToUpper(sub[rp][1]))
					}
				}
			}
		}

		kv.Relations.Table = append(kv.Relations.Table, table.Table...)
		kv.Relations.Cds = append(kv.Relations.Cds, table.Cds...)
		kv.Relations.ProxyClass = append(kv.Relations.ProxyClass, table.ProxyClass...)
		kv.Relations.Function = append(kv.Relations.Function, table.Function...)
		kv.Relations.Struct = append(kv.Relations.Struct, table.Struct...)
		kv.Relations.TableUsed = append(kv.Relations.TableUsed, table.TableUsed...)
		kv.Relations.Lock = append(kv.Relations.Lock, table.Lock...)
		kv.Relations.AuthCheck = append(kv.Relations.AuthCheck, table.AuthCheck...)
		kv.Relations.Transaction = append(kv.Relations.Transaction, table.Transaction...)
	}

	// fmt.Println(kv.index)

	if kv.index > 0 {
		kv.Relations.Table = removeDuplication_map(kv.Relations.Table)
		kv.Relations.Cds = removeDuplication_map(kv.Relations.Cds)
		kv.Relations.ProxyClass = removeDuplication_map(kv.Relations.ProxyClass)
		kv.Relations.Function = removeDuplication_map(kv.Relations.Function)
		kv.Relations.Struct = removeDuplication_map(kv.Relations.Struct)
		kv.Relations.TableUsed = removeDuplication_map(kv.Relations.TableUsed)
		kv.Relations.Lock = removeDuplication_map(kv.Relations.Lock)
		kv.Relations.AuthCheck = removeDuplication_map(kv.Relations.AuthCheck)
		kv.Relations.Transaction = removeDuplication_map(kv.Relations.Transaction)
	}

	return nil
}

func removeDuplication_map(arr []string) []string {
	set := make(map[string]struct{}, len(arr))
	j := 0
	for _, v := range arr {
		_, ok := set[v]
		if ok {
			continue
		}
		set[v] = struct{}{}
		arr[j] = v
		j++
	}

	return arr[:j]
}

func scanPrint(index int, str string) {
	// fmt.Printf("%v\t", s)
	// fmt.Println(sub[1])
	// fmt.Printf("\r%-10v%-30s", index, str)
}
