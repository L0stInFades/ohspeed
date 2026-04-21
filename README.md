# ohspeed

`ohspeed` 是一个偏专业取向的 OCaml CLI 网络测速工具，目标是做成终端里的 `speedtest`：高性能、可视化、可持续记录。

- 空闲延迟与抖动
- 并发下载 / 上传
- 分级放大请求尺寸，直到链路被充分压满
- 使用延迟分位数和带宽高分位数归约结果
- 基于 `OCaml 5 + Eio + cohttp-eio` 的 direct-style effects 并发测量引擎
- 支持文本输出和 JSON 输出
- 支持高性能终端实时可视化 dashboard
- 默认启动交互式 TUI 首页
- 重构版全屏 TUI：launch pad、live deck、result deck、history lounge
- 彩色状态条、速率热区、迷你 sparkline 曲线
- 终端尺寸变化后的实时重排，窄终端和宽终端布局不同
- 轻量过场动画、心跳式状态刷新、screen transition
- 历史页支持 `24h / 7d / 30d / all` 多时间尺度切换
- 持久化历史记录与历史对比图

默认端点使用 Cloudflare：

- `https://speed.cloudflare.com/__down`
- `https://speed.cloudflare.com/__up`
- `https://speed.cloudflare.com/meta`

## 环境

本项目面向 `OCaml 5.4.1`，使用 `opam` 与 `dune`。

仓库地址：

```bash
https://github.com/L0stInFades/ohspeed
```

## 安装

二进制安装：

```bash
curl -fsSL https://raw.githubusercontent.com/L0stInFades/ohspeed/main/scripts/install.sh | bash
```

指定版本：

```bash
curl -fsSL https://raw.githubusercontent.com/L0stInFades/ohspeed/main/scripts/install.sh | OHSPEED_VERSION=v0.5.2 bash
```

通过 `opam` 直接从 GitHub 安装：

```bash
opam pin add ohspeed https://github.com/L0stInFades/ohspeed.git
```

## 构建

```bash
eval $(opam env --switch=5.4.1 --set-switch)
cd /Users/Apple/ohspeed
opam install . --deps-only --with-test
dune build
```

## 运行

直接启动交互式 TUI：

```bash
ohspeed
```

也可以显式指定：

```bash
ohspeed --tui
```

首页里可以直接选：

- `Quick Burst`
- `Balanced Sweep`
- `Full Saturation`
- `History Lounge`
- `Control Deck`

TUI 快捷键：

- `↑/↓` 或 `j/k` 移动
- `Enter` 进入或执行
- `Esc` / `b` 返回
- `q` 退出

历史页额外快捷键：

- `←/→` 或 `[` / `]` 切换时间尺度
- `1 / 2 / 3 / 4` 直接切到 `24h / 7d / 30d / all`

保留原来的直接命令模式：

```bash
eval $(opam env --switch=5.4.1 --set-switch)
cd /Users/Apple/ohspeed
dune exec ohspeed -- --preset balanced
```

JSON 输出：

```bash
dune exec ohspeed -- --output json
```

实时可视化：

```bash
ohspeed --live --preset quick
```

历史仪表盘：

```bash
ohspeed --show-history --history-limit 12
```

跳过上传：

```bash
dune exec ohspeed -- --skip-upload
```

自定义端点：

```bash
dune exec ohspeed -- \
  --download-url https://example.com/down \
  --upload-url https://example.com/up \
  --meta-url https://example.com/meta
```

## 关键参数

- `--preset quick|balanced|full`
- `--output text|json`
- `--latency-count N`
- `--download-parallel N`
- `--upload-parallel N`
- `--download-sizes CSV`
- `--upload-sizes CSV`
- `--timeout SECONDS`
- `--skip-download`
- `--skip-upload`
- `--no-meta`
- `--live`
- `--show-history`
- `--history-limit N`
- `--no-save-history`

## 历史记录

测速完成后，默认会把摘要写入：

```bash
$XDG_STATE_HOME/ohspeed/history.jsonl
```

如果 `XDG_STATE_HOME` 未设置，则回落到：

```bash
~/.local/state/ohspeed/history.jsonl
```

如果你之前已经用过 `netprobe`，`ohspeed` 会自动回读旧的 `netprobe` 历史文件；新结果会写入新的 `ohspeed` 路径。

## 性能说明

这版终端可视化没有强依赖一个重量级 TUI 框架，而是采用：

- 纯 OCaml 的测量与渲染主逻辑
- `Eio + cohttp-eio` 的 effect handlers / direct-style 并发 I/O
- ANSI + alt-screen 的轻量全屏刷新
- 缓存前一帧，避免重复输出相同画面
- `terminal_size` 只负责读取终端尺寸

## 测试

```bash
eval $(opam env --switch=5.4.1 --set-switch)
cd /Users/Apple/ohspeed
dune runtest
```
