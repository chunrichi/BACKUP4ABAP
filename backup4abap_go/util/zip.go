package util

import (
	"archive/zip"
	"fmt"
	"io"
	"os"
	"strings"
)

func DeCompressed(src string) error {
	ZipReader, err := zip.OpenReader(src)
	if err != nil {
		return err
	}

	defer ZipReader.Close()

	for _, f := range ZipReader.File {
		if err := deCompressed(f); err != nil {
			return err
		}
	}
	return nil
}

func deCompressed(f *zip.File) error {
	fmt.Println(f.Name)

	if f.FileInfo().IsDir() {
		err := os.MkdirAll(f.Name, 0755)
		if err != nil {
			return err
		}
		return nil
	} else {
		// 由于 sap zip 无添加目录逻辑，此处分割目录创建
		dirName := f.Name[:strings.LastIndex(f.Name, "/")]

		err := os.MkdirAll(dirName, 0755)
		if err != nil {
			return err
		}
	}

	d, err := os.Create(f.Name)
	if err != nil {
		return err
	}

	unzipFile, err := f.Open()
	if err != nil {
		return err
	}
	if _, err := io.Copy(d, unzipFile); err != nil {
		return err
	}
	if err := unzipFile.Close(); err != nil {
		return err
	}
	return d.Close()
}
