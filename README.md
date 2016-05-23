# go-impl.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[impl](https://github.com/josharian/impl) for Emacs

## ScreenCast

![go-impl.el](image/go-impl.gif)


## Requirements

- [impl](https://github.com/josharian/impl)
- [godoc](https://godoc.org/golang.org/x/tools/cmd/godoc)

```
% go get github.com/josharian/impl
% go get -u golang.org/x/tools/cmd/godoc
```

## Command

#### `M-x go-impl` `(go-impl receiver interface)`

Insert snippet by `impl receiver interface`.

[travis-badge]: https://travis-ci.org/syohex/emacs-go-impl.svg
[travis-link]: https://travis-ci.org/syohex/emacs-go-impl
[melpa-link]: https://melpa.org/#/go-impl
[melpa-stable-link]: https://stable.melpa.org/#/go-impl
[melpa-badge]: https://melpa.org/packages/go-impl-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/go-impl-badge.svg
