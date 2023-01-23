## Linters

Emmy ships with a configuration that allows
[clj-kondo](https://github.com/clj-kondo/clj-kondo) to lint the library's
macros. This page contains installation instructions, as well as a directory of
all customizable linter warnings.

For all available linters offered by
[clj-kondo](https://github.com/clj-kondo/clj-kondo), see their
[linters.md](https://github.com/clj-kondo/clj-kondo/blob/master/doc/linters.md).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Linters](#linters)
    - [Installation](#installation)
- [Linter Directory](#linter-directory)
    - [Invalid Pattern Binding Symbol](#invalid-pattern-binding-symbol)
    - [Ignored Segment Restriction](#ignored-segment-restriction)
    - [Invalid Restriction in Consequence](#invalid-restriction-in-consequence)
    - [Ruleset Argument Count](#ruleset-argument-count)
    - [Invalid `with-literal-functions` Binding](#invalid-with-literal-functions-binding)
    - [Invalid Coordinate System Bindings](#invalid-coordinate-system-bindings)

<!-- markdown-toc end -->

### Installation

To use the configuration exported by Emmy, you'll need to install it into
your project.

The steps listed here mirror the [instructions in the clj-kondo
repo](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#importing).

To install the exported linter configuration:

1. Install the Emmy dependency into your project using the appropriate
   entry listed at [Clojars](https://clojars.org/emmy). For example, If you
   are using `deps.edn`, add the project to the map under `:deps`:

```clj
;; See https://clojars.org/emmy to pick an explicit version!
{:deps
 {emmy/emmy {:mvn/version "RELEASE"}}
```

2. Install clj-kondo using [these
   instructions](https://github.com/clj-kondo/clj-kondo/blob/master/doc/install.md).
   I highly recommend configuring [editor
   integration](https://github.com/clj-kondo/clj-kondo/blob/master/doc/editor-integration.md)
   for your text editor.

3.. If it doesn't exist yet, create a `.clj-kondo` folder in your project:

```sh
mkdir .clj-kondo
```

3. Run `clj-kondo` using the following command. This will import the `emmy`
   config and populate clj-kondo's cache with linting information about all of
   your dependencies:

```shellsession
# If you're using Leiningen:
$ clj-kondo --copy-configs --dependencies --lint "$(lein classpath)"
Imported config to .clj-kondo/emmy/emmy. To activate, add "emmy/emmy" to :config-paths in .clj-kondo/config.edn.

# If you're using deps.edn:
$ clj-kondo --copy-configs --dependencies --lint "$(clojure -Spath)"
Imported config to .clj-kondo/emmy/emmy. To activate, add "emmy/emmy" to :config-paths in .clj-kondo/config.edn.
```

5. As instructed, either create or edit `.clj-kondo/config.edn` so that it contains a `:config-paths` entry with `"emmy/emmy"`:

```clj
{:config-paths ["emmy/emmy"]}
```

6. Check the imported files into source control in your project.

### Disabling Linters

To disable or modify the warning level of any of the linters listed below, you
have a few options. This section will highlight the two most common overrides.

(For the full list, see the [config section of the clj-kondo
docs](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#options).)

You can place a specific override directly on a particular form:

```clj
;; Ignore any :emmy.pattern/ruleset-args warning emitted in the `(ruleset ...)` form:

#_{:clj-kondo/ignore [:emmy.pattern/ruleset-args]}
(ruleset
 (+ (? x) (? y)) (fn [m] (- ('?x m) ('?y m))))
```

Or you can add an entry for any of the linters to a map under `:linters` keyword
in your project's `.clj-kondo/config.edn`. For example, this config will disable
the `:emmy.pattern/ruleset-args` error:

```clj
{:linters
 {:emmy.pattern/ruleset-args {:level :off}}}
```

Other valid levels are `:warning` and `:error`.

See the [config section of the clj-kondo
docs](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#options)
for more information.

## Linter Directory

This section describes all of the custom warnings emitted by the Emmy clj-kondo config.

### Invalid Pattern Binding Symbol

*Keyword:* `:emmy.pattern/binding-sym`

*Description:* warn when a binding form like `(? x)` in the pattern argument to
a `pattern.rule` macro contains anything other than a simple, non-qualified
symbol.

*Default level:* `:error`

*Example trigger:*

``` clojure
(require '[emmy.rule :as r])

(r/rule (+ (? "x") ?y) => "match!")
```

*Example message:*: `Binding variable "x" must be a non-namespaced symbol.`

### Ignored Segment Restriction

*Keyword:* `:emmy.pattern/ignored-restriction`

*Description:* warn when a segment binding form like `(?? x)` or `($$ x)`
contain restrictions like `(?? x all-odd?)`. These don't error but aren't
currently used.

*Default level:* `:warning`

*Example trigger:*

`.clj-kondo/config.edn`:

``` clojure
(require '[emmy.rule :as r])

(r/rule (+ (?? x odd?) ?y) => "match!")
```

*Example message*: `Restrictions are (currently) ignored on ?? binding forms: odd?`

### Invalid Restriction in Consequence

*Keyword:* `:emmy.pattern/consequence-restriction`

*Description:* warn when a binding form like `(? x)`, `(?? x)` or `($$ x)` in a
consequence contains restrictions like `(? x odd?)`. These are meaningless in
consequences, and may error in the future.

*Default level:* `:error`

*Example trigger:*

``` clojure
(require '[emmy.rule :as r])

(r/rule (+ (? x) (? y)) => (+ (? y odd?) (? x)))
```

*Example message*: `Restrictions are not allowed in consequence bindings: odd?`.

### Ruleset Argument Count

*Keyword:* `:emmy.pattern/ruleset-args`

*Description:* warn when the `emmy.pattern/ruleset` receives arguments that
aren't grouped into three. Each triplet should match the arguments you would
supply to the 3-arity of `emmy.pattern/rule`.

*Default level:* `:error`

*Example trigger:*

``` clojure
(require '[emmy.rule :as r])

(r/ruleset (+ (? x) (? y))
           (fn [m] (- ('?x m) ('?y m))))
```

*Example message*: `ruleset requires bindings in groups of 3. Received 2 bindings.`

### Invalid `with-literal-functions` Binding

*Keyword:* `:emmy.abstract.function/invalid-binding`

*Description:* warn when an binding entry passed to the first argument of
`with-literal-functions` (in either `emmy.abstract.function` or
`emmy.env`) is anything other than an unqualified symbol or a 3-vector
containing a symbol, a domain and a range.

*Default level:* `:error`

*Example trigger:*

``` clojure
(require '[emmy.env :as e])

(e/with-literal-functions [x 10 y]
  [x y])
```

*Example message*: `Bindings must be either bare symbols or 3-vectors of the form [sym domain range]. Received: 10`.

### Invalid Coordinate System Bindings

*Keyword:* `:emmy.calculus.coordinate/invalid-binding`

*Description:* warn when the left side of a binding pair passed to
`let-coordinates` or `using-coordinates` (in either
`emmy.calculus.coordinate` or `emmy.env`) is anything other than a
potentially-nested structure of vectors or lists beginning with `up` or `down`
with unqualified symbols at the leaves.

*Default level:* `:error`

*Example trigger:*

```clj
(require '[emmy.env :as e])

(e/let-coordinates [[x (up {:key "val"})] R3-rect]
  ,,,)
```

*Example message*: `Bindings must be either a vector or list (optionally beginning with `up` or `down`) or a bare symbol. Received: {:key "val"}`.
