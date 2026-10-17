d = data.frame(x = 1:3, y = c("b", "a", "c"))

# Marker occurring exactly once in each asset, so it can be counted as well
# as looked for. Bare class or file name is not enough: CSS class names
# appear in extension's script too, and `lt-interactive` appears in a
# comment inside core lt.js.
ext_css = '[.]lti-sort \\{'; ext_js = 'LT[.]plugins[.]interactive = \\{'
core_css = '[.]lt-wrap \\{'; core_js = 'root[.]LT = \\{'

assert("the interactive extension is included only for a table that opts in", {
  html = format(lt(d) |> lt_interactive())
  (grepl(ext_css, html) %==% TRUE)
  (grepl(ext_js, html) %==% TRUE)
  html = format(lt(d))
  (grepl(ext_css, html) %==% FALSE)
  (grepl(ext_js, html) %==% FALSE)
})

assert("the linked form loads the extension after the core runtime", {
  html = format(lt(d) |> lt_interactive(), inline_assets = FALSE)
  (grepl('<link rel="stylesheet" href="[^"]*lt-interactive[^"]*">', html) %==% TRUE)
  # plugin registers on LT global, so core must execute first
  src = regmatches(html, gregexpr('<script src="[^"]+"', html))[[1]]
  (length(src) %==% 2L)
  (grepl('lt-interactive', src) %==% c(FALSE, TRUE))
})

assert("the extension follows the asset kind it belongs to", {
  x = lt(d) |> lt_interactive()
  # stylesheet only
  html = format(x, assets = 'css')
  (grepl(core_css, html) %==% TRUE)
  (grepl(ext_css, html) %==% TRUE)
  (grepl(ext_js, html) %==% FALSE)
  # script only
  html = format(x, assets = 'js')
  (grepl(core_js, html) %==% TRUE)
  (grepl(ext_js, html) %==% TRUE)
  (grepl(ext_css, html) %==% FALSE)
  # neither: the spec block alone
  html = format(x, assets = FALSE)
  (grepl(ext_css, html) %==% FALSE)
  (grepl(ext_js, html) %==% FALSE)
  (grepl('LT[.]q', html) %==% TRUE)
})

assert("the filter and pagination options reach the client spec", {
  # assets aside, format() emits spec runtime reads
  json = format(lt(d) |> lt_interactive(filter = 'y', pager = c(20, 5)), assets = FALSE)
  # column vector boxes just those columns: each maps to `true` under `cols`
  (grepl('"filter": \\{\\s*"cols": \\{\\s*"y": true', json) %==% TRUE)
  # page sizes keep given order: first is initial size
  (grepl('"pager": \\[20, 5\\]', json) %==% TRUE)
  # single size still serializes as array
  (grepl('"pager": \\[5\\]', format(lt(d) |> lt_interactive(pager = 5),
                                      assets = FALSE)) %==% TRUE)
  # Inf (every row on one page) travels as 0, which runtime reads as such
  (grepl('"pager": \\[5, 0\\]', format(lt(d) |> lt_interactive(pager = c(5, Inf)),
                                         assets = FALSE)) %==% TRUE)
  # filtering every column needs no column list: just default box
  (grepl('"filter": \\{\\s*"default": true\\s*\\}',
         format(lt(d) |> lt_interactive(filter = TRUE), assets = FALSE)) %==% TRUE)
  # paging is on by default; filtering is not
  json = format(lt(d) |> lt_interactive(), assets = FALSE)
  (grepl('"pager": \\[10, 25, 50, 100\\]', json) %==% TRUE)
  (grepl('filter', json) %==% FALSE)
  # both can be turned off
  json = format(lt(d) |> lt_interactive(pager = FALSE), assets = FALSE)
  (grepl('filter|pager', json) %==% FALSE)
})

assert("a typed filter spec reaches the client with its options", {
  # checklist spec keeps type and label, reshapes `choices` to {value,label}
  # objects, keeps scalar `selected` as JSON array (so client seeds it)
  json = format(lt(d) |> lt_interactive(filter = list(
    y = list(type = "checklist", label = "Y", choices = c("a", "b"), selected = "a")
  )), assets = FALSE)
  (grepl('"type": "checklist"', json) %==% TRUE)
  (grepl('"label": "Y"', json) %==% TRUE)
  (grepl('"choices": \\[\\s*\\{\\s*"value": "a",\\s*"label": "a"', json) %==% TRUE)
  (grepl('"selected": \\["a"\\]', json) %==% TRUE)
})

assert("a default sort reaches the client spec as ordered name keys", {
  # names travel as-is, keeping '-' prefix; client parses it
  json = format(lt(d) |> lt_interactive(sort = c('x', '-y')), assets = FALSE)
  (grepl('"sort": \\["x", "-y"\\]', json) %==% TRUE)
  # one-sided formula is equivalent way to spell same keys
  (grepl('"sort": \\["x", "-y"\\]',
         format(lt(d) |> lt_interactive(sort = ~ x + -y), assets = FALSE)) %==% TRUE)
  (grepl('"sort": \\["x", "-y"\\]',
         format(lt(d) |> lt_interactive(sort = ~ x - y), assets = FALSE)) %==% TRUE)
  # logical passes through unchanged (enable/disable click-to-sort)
  (grepl('"sort": true', format(lt(d) |> lt_interactive(), assets = FALSE)) %==% TRUE)
  (grepl('"sort": false',
         format(lt(d) |> lt_interactive(sort = FALSE), assets = FALSE)) %==% TRUE)
})

assert("download reaches the client spec as true or a file name, else absent", {
  # TRUE enables button with no file name (client defaults it)
  (grepl('"download": true',
         format(lt(d) |> lt_interactive(download = TRUE), assets = FALSE)) %==% TRUE)
  # string sets file name, kept verbatim
  (grepl('"download": "cars.csv"',
         format(lt(d) |> lt_interactive(download = 'cars.csv'), assets = FALSE)) %==% TRUE)
  # off by default, and FALSE/NULL emit nothing
  (grepl('download', format(lt(d) |> lt_interactive(), assets = FALSE)) %==% FALSE)
  (grepl('download',
         format(lt(d) |> lt_interactive(download = FALSE), assets = FALSE)) %==% FALSE)
  # shaper drops disabled download so no key serialized
  (download_opt(NULL) %==% NULL)
  (download_opt(FALSE) %==% NULL)
  # TRUE and bare file name pass through unchanged
  (download_opt(TRUE) %==% TRUE)
  (download_opt('cars.csv') %==% 'cars.csv')
  # raw mode serializes object carrying mode (and file, if given)
  (download_opt(list(file = 'cars.csv', mode = 'raw')) %==%
     list(file = 'cars.csv', mode = 'raw'))
  # raw mode with default file name omits file slot
  (download_opt(list(mode = 'raw')) %==% list(mode = 'raw'))
  # display mode stays implicit: bare string (or TRUE), never a mode key
  (download_opt(list(file = 'cars.csv', mode = 'display')) %==% 'cars.csv')
  (download_opt(list(mode = 'display')) %==% TRUE)
})

assert("detail = column names resolves to a name array", {
  # formula / character vector of columns becomes plain name array for
  # runtime to build one-row detail table from
  json = format(lt(d) |> lt_interactive(detail = ~ x + y), assets = FALSE)
  (grepl('"detail": ["x", "y"]', json, fixed = TRUE) %==% TRUE)
  json = format(lt(d) |> lt_interactive(detail = c('x', 'y')), assets = FALSE)
  (grepl('"detail": ["x", "y"]', json, fixed = TRUE) %==% TRUE)
})

count = function(p, x) sum(gregexpr(p, x)[[1]] > 0)

# Emulate a document: knit tables in order, after clearing flags
# knit_print() uses to dedup assets (knitr resets them between real knits).
knit_doc = function(...) {
  for (f in c(.knit_flag, .int_flag))
    knitr::opts_knit$set(set_names(list(NULL), f))
  paste(unlist(lapply(list(...), knit_print.lt_tbl)), collapse = '\n')
}

if (xfun::loadable('knitr'))
  assert("knit_print emits the extension once, for the first interactive table", {
    int = lt(d) |> lt_interactive(); static = lt(d)

    html = knit_doc(int, int)
    (count(ext_css, html) %==% 1L)
    (count(ext_js, html) %==% 1L)
    (count(core_js, html) %==% 1L)

    # static table first: core runtime already out by time extension is
    # needed; extension must still load after it
    html = knit_doc(static, int)
    (count(ext_css, html) %==% 1L)
    (count(ext_js, html) %==% 1L)
    (count(core_js, html) %==% 1L)
    (regexpr(ext_js, html) > regexpr(core_js, html))

    # document with no interactive table never loads extension
    (count(ext_js, knit_doc(static, static)) %==% 0L)
  })

assert("record_print emits the linked extension for an interactive table", {
  html = paste(unlist(record_print.lt_tbl(lt(d) |> lt_interactive())), collapse = '\n')
  (grepl('lt-interactive[^"]*[.]css', html) %==% TRUE)
  (grepl('lt-interactive[^"]*[.]js', html) %==% TRUE)
  html = paste(unlist(record_print.lt_tbl(lt(d))), collapse = '\n')
  (grepl('lt-interactive', html) %==% FALSE)
})
