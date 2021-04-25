
# Restructuring Websites

## Aim

Remove as much programming code as possible

## Methods by which to achieve

- Convert templates and helpers to Markdown
- Have very little in the way of config files
- Therefore there should be little need to use such code.

E.g.:
```
$ echo -e "---\nid: AGPL-3.0\nname: AGPL-3.0\n---\n\nHi! How are you?" | pandoc -f markdown --template sites/jolharg/templates/spdx.md -t html

[AGPL-3.0](https://spdx.org/licenses/AGPL-3.0.html)
```

Now can files and templates include others?

## Structure

```
All websites template (doctype, html, head, header)
    Head
        Title
        Keywords
        Extra Head content?
    Header
        Individual site can define/include?
        e.g. dandart includes social template
```