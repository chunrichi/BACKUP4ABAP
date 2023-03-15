package main

import (
	"abapscan/util"
	"fmt"
	"os"
)

func main() {
	print("\tABAP Scan\n")

	// 暂不从外部读取 改写死
	// rl := util.ReadList{}
	// err := rl.Load()
	// if err != nil {
	// 	fmt.Println(err)
	// }
	rl := util.ReadList{
		Pwd: []string{"SE38", "SE24", "SE37", "ench"},
	}

	abapTables := []util.KeyValue{}

	if len(rl.Pwd) > 0 {
		pwd, _ := os.Getwd()
		for pw := range rl.Pwd {
			fmt.Printf("\n处理路径:")
			fmt.Printf("\t%v\t%s\n", pw, rl.Pwd[pw])
			abapTables = append(abapTables, util.ReadFile(pwd+"/"+rl.Pwd[pw])...)
		}
	} else {
		pwd, _ := os.Getwd()
		fmt.Printf("处理路径: %s\n", pwd)
		abapTables = util.ReadFile(pwd)
	}

	// fmt.Println("\n匹配结果")
	// fmt.Println(abapTables)

	fmt.Println("\n保存自建表")
	err := util.Save(abapTables)
	if err != nil {
		fmt.Println(err)
	}
}
