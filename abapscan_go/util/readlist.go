package util

import (
	"encoding/json"
	"os"
)

type ReadList struct {
	Pwd []string `json:"pwd"`
}

func (rl *ReadList) Load() error {
	setFile, err := os.Open("readlist.json")
	if err != nil {
		return err
	}

	defer setFile.Close()

	if err := json.NewDecoder(setFile).Decode(rl); err != nil {
		return err
	}

	return nil
}
