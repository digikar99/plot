<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/plot">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Plot</h3>

  <p align="center">
    A library for plotting with Common Lisp
    <br />
    <a href="https://lisp-stat.dev/docs/tutorials/plotting/"><strong>Explore the tutorial »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/plot/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/plot/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/plot/">Reference Manual</a>
  </p>
</p>

<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li>
      <a href="#usage">Usage</a>
      <ul>
        <li><a href="#high-level-authoring">High-Level Authoring</a></li>
        <li><a href="#advanced-explicit-construction">Advanced Explicit Construction</a></li>
        <li><a href="#output-paths">Output Paths</a></li>
      </ul>
    </li>
    <li><a href="#resources">Resources</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## About the Project

The Plot system provides a way to visualize data in Common Lisp. It includes
text-based plotting that works in the REPL and JavaScript visualizations rendered
through Vega and Vega-Lite. For Vega work, `make-plot` is now the constructor
center, while the public `geom` and `gg` packages provide the high-level
plot-owned authoring fragments that are merged into a plot specification.

Plot integrates with
[data-frame](https://github.com/Lisp-Stat/data-frame), and can also be used
independently.

### Built With

* [cl-who](https://github.com/edicl/cl-who)
* [cl-spark](https://github.com/tkych/cl-spark)
* [yason](https://phmarek.github.io/yason/)
* [LASS](https://github.com/Shinmera/LASS)

<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running, follow these steps.

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](https://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

### Installation

To make the system accessible to [ASDF](https://common-lisp.net/project/asdf/),
clone the repository in a directory ASDF already knows about. By default,
`~/common-lisp/` is commonly used:

```sh
cd ~/common-lisp
git clone https://github.com/Lisp-Stat/plot
```

If needed, reset the ASDF source registry from the REPL:

```lisp
(asdf:clear-source-registry)
```

Load the Vega backend with Quicklisp:

```lisp
(ql:quickload :plot/vega)
```

After dependencies are available, loading through ASDF also works:

```lisp
(asdf:load-system :plot/vega)
```

The examples below assume:

```lisp
(use-package '#:vega)
```

<!-- USAGE EXAMPLES -->
## Usage

### High-Level Authoring

The recommended path is:

```lisp
(make-plot data fragment &rest fragments)
```

`geom` and `gg` return plot-owned fragments. `make-plot` merges them into a
Vega-Lite spec and returns a plot object. Construction does not display a plot
implicitly.

#### A Small Point Plot

```lisp
(defparameter *points*
  #((:x 1 :y 2 :group "A")
    (:x 2 :y 3 :group "B")
    (:x 3 :y 5 :group "A")))

(defparameter *point-plot*
  (make-plot *points*
             '(:title "Simple Point Plot")
             (geom:point :x :y :color :group :filled t)
             (gg:label :x "X" :y "Y")
             (gg:theme :width 360 :height 220)))
```

Display is explicit:

```lisp
(plot:plot *point-plot*)
```

#### Layered Fragments

```lisp
(defparameter *layered-plot*
  (make-plot *points*
             '(:title "Points and Trend")
             (gg:layer
              '(:mark (:type :point :filled t)
                :encoding (:x (:field :x :type :quantitative)
                           :y (:field :y :type :quantitative)))
              '(:mark :line
                :encoding (:x (:field :x :type :quantitative)
                           :y (:field :y :type :quantitative))))
             (gg:label :x "X" :y "Y")
             (gg:theme :width 360 :height 220)))
```

`geom` helpers also compose directly with `gg` fragments:

```lisp
(defparameter *line-plot*
  (make-plot *points*
             '(:title "Geom + GG")
             (geom:line :x :y :color "steelblue" :point t)
             (gg:label :x "X" :y "Y")
             (gg:tooltip :x '(:y :quantitative))))
```

### Advanced Explicit Construction

Use the explicit `:base` path when you already have an authored lower-level
Vega-Lite plist and want `make-plot` to remain the constructor center.

```lisp
(defparameter *base*
  '(:title "Base Plot"
    :mark :line
    :data (:values #((:x 1 :y 2)
                     (:x 2 :y 4)))
    :encoding (:x (:field :x :type :quantitative)
               :y (:field :y :type :quantitative))))

(defparameter *overlay*
  '((:title "Base Plot With Overlay")
    (:encoding (:tooltip (:field :y :type :quantitative)))))
```

Small explicit-construction forms:

```lisp
(make-plot :base *base*)

(make-plot :base *base* :name "trend")

(make-plot :base *base* :overlay *overlay*)

(make-plot :base *base* :name "trend" :overlay *overlay*)
```

`overlay` is an explicit list of fragments that is merged on top of `base`.

### Output Paths

`make-plot` constructs a plot object only. Common explicit next steps are:

- `(plot:plot plot)` to render through the browser-oriented Vega path
- `(vega:write-html plot)` to write an HTML wrapper
- `(vega:write-spec plot)` to emit Vega-Lite JSON
- notebook consumers via the optional `plot/vega/jupyter` adapter

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/) project; that
should be your first stop for information. Also see:

- Tutorial: <https://lisp-stat.dev/docs/tutorials/plotting/>
- Cookbook: <https://lisp-stat.dev/docs/cookbooks/plotting/>
- Project page: <https://lisp-stat.dev/docs/tasks/plotting/>
- Resources: <https://lisp-stat.dev/resources>
- Community: <https://lisp-stat.dev/community>

## Roadmap

See the [open issues](https://github.com/lisp-stat/plot/issues) for a list of
proposed features and known issues.

## Contributing

Contributions are welcome. See [CONTRIBUTING](CONTRIBUTING.md) for details on
the code of conduct and the pull request process.

## License

Distributed under the MS-PL License. See [LICENSE](LICENSE) for more
information.

## Contact

Project Link: [https://github.com/lisp-stat/plot](https://github.com/lisp-stat/plot)

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/plot.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/plot/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/plot.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/plot/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/plot.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/plot/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/plot.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/plot/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/plot.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/plot/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
