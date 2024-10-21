# BACKUP4ABAP

abap code backup

## 简介

在项目管理过程中，我们更多的关注代码的情况，而不在乎能否快捷的拷贝程序，并且想要有一个更清晰的列表用于查看程序

和 abapgit 比较，本程序导出的内容略显简略，但有一些我认为较为不错的优点

- 更为直观的 目录结构
- 基于 TABLE CDS 的更为直观的 表内容
- 基于 JSON 的数据展示，更为直观
- 基于 txt 列表的文本信息，更为直观

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
  - [X] 代码导出
    - [X] 程序代码导出
    - [X] 函数代码导出
    - [X] 类代码导出
    - [X] 隐式增强导出
    - [X] 代码插入导出
    - [X] 程序文本导出
    - [X] 消息类导出
  - [X] 结构导出
    - [X] 表 `ddl` 导出
    - [X] 结构 `ddl` 导出
    - [X] `cds` 视图导出
    - [X] 数据元素导出
    - [X] 数据域导出
    - [X] 表类型导出
  - [X] 打印导出
    - [X] smartforms 表单
    - [X] smartforms 样式
  - [X] 其他导出
    - [X] SMW0 信息导出
    - [X] 事务码导出
    - [X] STRANS 转换导出
- [ ] 程序逻辑
  - [X] 增量导出
- [ ] 接口功能
  - [X] 通过接口获取 zip 文件
  - [X] 通过exe，快速执行下载并解压到指定文件夹

## 协议

[MIT](https://choosealicense.com/licenses/mit/)
