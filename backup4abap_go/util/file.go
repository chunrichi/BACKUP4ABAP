package util

import (
	"errors"
	"io"
	"os"
)

func SaveHttpZip(filename string, body io.ReadCloser) error {
	// 保存 HTTP 返回的 body 为 zip 文件

	fileFlags := os.O_CREATE | os.O_WRONLY
	zipFile, err := os.OpenFile(filename, fileFlags, 0666)
	if err != nil {
		return errors.New("创建 zip 文件失败")
	}

	defer zipFile.Close()

	reader := make([]byte, 16*1024)

	// 内容拷贝
	_, err = io.CopyBuffer(zipFile, body, reader)
	if err != nil {
		if err == io.EOF {
			return errors.New("io.EOF")
		}
		return err
	}

	return nil
}

func IsExists(path string) (os.FileInfo, bool) {
	f, err := os.Stat(path)
	return f, err == nil || os.IsExist(err)
}
