
> 本文档为 backup4abap 接口相关的说明

## 程序下载

进入标签页

![](./src/GithubTag%2020230207.png)

进入 `Release` 下载 exe 文件（仅适用 x64 版本）

![](./src/GithubRelease%2020230207.png)

## SICF 配置

将程序 `zcl_backup4abap_interface.abap` 拷贝进系统，然后 `SICF` 配置接口

事务码 `SICF` 创建服务

![](./src/Sicf%2020230207.png)

填写服务元素名称

![](./src/SicfName%2020230207.png)

设置服务内容

![](./src/SicfSetting01%2020230207.png)

![](./src/SicfSetting02%2020230207.png)

![](./src/SicfSetting03%2020230207.png)

其他内容选填

## 运行程序

将 `backup.exe` 拷入需要存储代码的目录（每个项目单独一个）

![](./src/CopyBackupExe%2020230207.png)

在 shell 中执行或双击执行

![](./src/BackupExeRun%2020230207.png)

第一次执行会有设置选项，需要设置服务地址，登录系统账号，登录系统密码，并会在目录结构中创建 `config.json` 文件，如再续调整可直接调整 JSON

![](./src/FileTree%2020230207.png)