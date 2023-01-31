
> 本文档为 backup4abap 程序的使用说明

## 导入

从 github 中直接复制 `zbackup4abap.abap` 的源码

![](../src/GihubCopyCode%2020230131.png)

然后将代码导入到系统中，建议命名为 `zbackup4abap` 后续如有其他功能不需要更改代码

## 执行

将代码拷贝进系统后执行可看到如下界面

![](../src/SelectScreen%2020230131.png)

按需勾选需要下载的内容。各个 `block` 功能如下

- [x] 代码下载
	- [x] 报表程序代码 
	- [x] 函数程序代码
	- [x] 类程序代码
- [x] DDIC 表相关内容下载
	- [x] 表结构下载
		- [ ] `XML` 类型结构 (功能有缺陷，按钮已灰)
		- [x] `DDL` 类型表结构 (下载无效考虑版本)
	- [x] CDS 视图
	- [x] 数据域
	- [x] 数据元素
- [x] SMW0 附件
- [x] 开发类(包) 限制
- [x] 增量获取

注意: **下载仅包含 `Z` 开头的程序**

选中需要的内容后，点击执行按钮，确认保存路径

![](../src/RunReport%2020230131.png)

右键压缩文件，解压到当前文件夹

![](../src/UnZip%2020230131.png)

即可拿到本次备份的内容

![](../src/ShowDir%2020230131.png)

### 使用 git 进行备份程序

新建文件 `.gitignore` 设置忽略文件内容，以下为常设置的内容(注意无后缀名)

![](../src/Gitignore%2020230131.png)

```txt
*.txt
*.xml
*.exe
*.zip
```

在当前路径右键打开命令行工具

![](../src/OpenPowershell%2020230131.png)

初始化 git 仓库

```powershell
git init
```

提交

```powershell
git commit -m '提交描述'
```

## 多次备份

后续再有处理时，按上序方法，再次执行，当有覆盖提示时，**选择覆盖** ，然后通过 `git` 再次提交

![](../src/UnZipAgain%2020230131.png)

然后可通过类似 `github desktop` 等程序看到每次提交的内容

![](../src/GithubDesktop%2020230131.png)