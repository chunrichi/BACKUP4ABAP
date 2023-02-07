package util

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"reflect"
	"regexp"
	"strings"
)

const ConfigFileName string = "config.json"

type Set struct {
	SapSever  string
	Username  string
	Password  string
	AutoUnZip bool
}

func (set Set) IsEmpty() bool {
	return reflect.DeepEqual(set, Set{})
}

func (set *Set) Save() error {

	setFile, err := os.Create(ConfigFileName)
	if err != nil {
		return err
	}

	defer setFile.Close()

	// 编码写入配置文件;
	cfgEncoder := json.NewEncoder(setFile)
	cfgEncoder.SetIndent("", "\t")
	if err := cfgEncoder.Encode(set); err != nil {
		return err
	}

	return nil
}

func (set *Set) Load() error {
	setFile, err := os.Open(ConfigFileName)
	if err != nil {
		return err
	}

	defer setFile.Close()

	if err := json.NewDecoder(setFile).Decode(set); err != nil {
		return err
	}

	return nil
}

func Setting() Set {
	// 设置
	set := Set{}

	_, ok := IsExists(ConfigFileName)
	if !ok {
		// 新增配置
		err := settingProcess(&set)
		if err != nil {
			fmt.Println(err)
			return Set{}
		}

		err = set.Save()
		if err != nil {
			fmt.Println(err)
			return Set{}
		}
	} else {
		// 检查设置是否齐全
		set.Load()

		err := checkHttpUrl(set.SapSever)
		if err != nil {
			fmt.Println(err)
			return Set{}
		}
	}

	// 检查是否已设置
	return set
}

func settingProcess(set *Set) error {
	fmt.Printf("\t初始化设置\n")

	// Sap 服务地址
	fmt.Print("\nSap 服务地址:")
	fmt.Scanln(&set.SapSever)

	// 检查 url
	if err := checkHttpUrl(set.SapSever); err != nil {
		return err
	}

	// 账号密码
	fmt.Print("\n登录账号:")
	fmt.Scanln(&set.Username)
	fmt.Print("\n登录密码:")
	fmt.Scanln(&set.Password)

	// 是否自动解压
	var autoUnZip string

	autoUnZip = "y"

	fmt.Print("\n是否自动解压到当前文件夹(y/n):")
	fmt.Scanln(&autoUnZip)

	autoUnZip = strings.ToLower(autoUnZip)
	autoUnZip = strings.TrimSpace(autoUnZip)

	if autoUnZip[:1] == "y" {
		set.AutoUnZip = true
	} else {
		set.AutoUnZip = false
	}

	return nil
}

func checkHttpUrl(url string) error {
	reg := regexp.MustCompile(`^(?:(http|https):\/\/)?((?:[\w-]+\.)+[a-z0-9]+)((?:\/[^/?#]*)+)?(\?[^#]+)?(#.+)?`)

	if len(reg.FindStringSubmatch(url)) <= 0 {
		return errors.New("服务地址错误！")
	}

	return nil
}
