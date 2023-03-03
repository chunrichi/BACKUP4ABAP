package main

import (
	"backup4abap/util"
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"regexp"
	"strings"
	"time"
)

type ReqJson struct {
	CheckFlag string `json:"CHECKFLAG"`
	DeltaFlag string `json:"DELTAFLAG"`
}

type ResJson struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

func main() {
	var delta string

	set := util.Setting()

	if set.IsEmpty() {
		fmt.Print("\n配置文件 config.json 内容错误, 请按提示修改\n")
		return
	}

	delta = "YES"

	if set.AutoUnZip == false {
		fmt.Print("\t程序下载\n是否增量?(Yes/No) 默认 Yes:")
		fmt.Scanln(&delta)
	}

	delta = strings.ToLower(delta)
	delta = strings.TrimSpace(delta)

	reqJson := ReqJson{
		CheckFlag: "BACKUP4ABAP",
	}

	if delta == "yes" {
		reqJson.DeltaFlag = "X"
	}

	// 生成json报文
	jsonString := new(bytes.Buffer)
	json.NewEncoder(jsonString).Encode(reqJson)

	var client = &http.Client{
		Timeout: time.Second * 5,
	}

	reqCoustTime := time.Now()

	reqt, err := http.NewRequest("POST", set.SapSever, jsonString)
	if err != nil {
		fmt.Printf("\nNew request failed:%s,\n", err)
		return
	}

	reqt.SetBasicAuth(set.Username, set.Password)
	reqt.Header.Add("content-type", "application/json")

	res, err := client.Do(reqt)
	if err != nil {
		fmt.Printf("\nrequest failed:%s\n", err)
		return
	}

	useTime := time.Since(reqCoustTime)
	fmt.Printf("\n\nRequest Cost Time: %s\n", useTime)

	defer res.Body.Close()

	if res.StatusCode != 200 {
		fmt.Println(res.Status)
		return
	}

	// 是否为错误
	if strings.Contains(res.Header.Get("content-type"), "application/json") {
		body, err := ioutil.ReadAll(res.Body)
		if err != nil {
			fmt.Println(err)
			return
		}
		res := ResJson{}
		err = json.Unmarshal(body, &res)
		if err != nil {
			fmt.Println(err)
			return
		}
		fmt.Println("")
		fmt.Println("Code:", res.Code)
		fmt.Println("Message:", res.Message)
		return
	}

	// 文件名
	dispostion := res.Header.Get("content-disposition")
	reg := regexp.MustCompile(`filename=(.*)$`)
	matchFiles := reg.FindStringSubmatch(dispostion)
	filename := matchFiles[len(matchFiles)-1]

	if filename == "" {
		filename = "backup.zip"
	}

	fmt.Println("")
	fmt.Println("content-disposition:", dispostion)
	fmt.Println("filename:", filename)
	fmt.Println("")

	// 保存文件到本地

	// 创建文件
	err = util.SaveHttpZip(filename, res.Body)
	if err != nil {
		fmt.Println(err)
		return
	}

	UzipTime := time.Now()

	// 解压
	err = util.DeCompressed(filename, set.ProcessBar)
	if err != nil {
		fmt.Println(err)
		return
	}

	useTime = time.Since(UzipTime)
	fmt.Printf("\nUzip Cost Time: %s\n", useTime)

	// 删除文件
	err = os.Remove(filename)
	if err != nil {
		fmt.Println(err)
		return
	}
}
