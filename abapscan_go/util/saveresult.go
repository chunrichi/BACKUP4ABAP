package util

import (
	"encoding/json"
	"os"
)

var ResultName string = "relations.json"

func Save(kv []KeyValue) error {

	setFile, err := os.Create(ResultName)
	if err != nil {
		return err
	}

	defer setFile.Close()

	// 编码写入配置文件;
	cfgEncoder := json.NewEncoder(setFile)
	cfgEncoder.SetIndent("", "    ")
	if err := cfgEncoder.Encode(kv); err != nil {
		return err
	}

	return nil
}
