# BACKUP4ABAP

abap code backup

## 简介

在项目管理过程中，我们更多的关注代码的情况，而不在乎能否快捷的拷贝程序

和 abapgit 比较，本程序导出的内容略显简略，但有一些我认为较为不错的优点

- 更为直观的 目录结构
- 基于 TABLE CDS 的更为直观的 表内容

## 使用

> 基础功能的使用

将程序 `zbackup4abap` 直接拷贝到 SAP 系统中，运行即可 [详情说明](./docs/backup4abap.md)

> 接口功能的使用 (扩展)

1. 将类 `zcl_backup4abap_interface` 拷贝到系统中
2. 事务码 `SICF` 新增接口，将 `zcl_backup4abap_interface` 配置为一个接口
3. 拿到接口 `url`
4. 在本项目 `Releases` 中下载最新的 `backup.exe` 文件到存储代码的目录
5. 执行程序 `backup.exe` 即可

[详情说明](./docs/backup4abap_interface.md)

## 功能

- [ ] 导出功能
  - [X] 程序代码导出
  - [X] 函数代码导出
  - [X] 类代码导出
  - [X] 表 `ddl` 导出
  - [X] 结构 `ddl` 导出
  - [X] `cds` 视图导出
  - [X] 数据元素导出
  - [X] 数据域导出
  - [X] SMW0 信息导出
- [ ] 程序逻辑
  - [X] 增量导出
- [ ] 接口功能
  - [ ] 通过接口获取 zip 文件
  - [ ] 通过exe，快速执行下载并解压到指定文件夹

## 版本

- v1.00 初始 可用版本
- v1.01 新增 增量下载，避免无意义的耗时
- v1.03 修改 调整程序结构和变量名
- v1.04 修改 调整修改信息 `JSON` 文件结构，新增类型信息类型
- v1.05 新增 新增 SMW0 文件的下载
- v1.06 新增 新增下载时排除当前下载程序
- v1.07 新增 新增下载数据元素、数据域导出功能
- v2.00 新增 新增接口和搭配使用的golang脚本

## 协议

[MIT](https://choosealicense.com/licenses/mit/)
