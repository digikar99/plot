;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VEGA-TESTS -*-
;;; Copyright (c) 2022, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package #:vega-tests)

;;; Test runner
(defun run-tests (&optional (report-progress t))
  "Run all vega test suites. Returns the clunit-report object.

Binds *test-output-stream* to the current *standard-output* because
clunit2 initialises *test-output-stream* at load time; if the system
was loaded with (ql:quickload :silent t) that stream is a null broadcast
stream that discards all output.  Also binds *print-pretty* so the
report formats with line-breaks instead of printing on a single line."
  (let ((*print-pretty* t)
        (clunit:*test-output-stream* *standard-output*))
    (run-suite 'vega :report-progress report-progress)))

;;; Root suite
(defsuite vega ())

;;; Child suites
(defsuite write-spec-suite (vega))
(defsuite merge-plists-suite (vega))
(defsuite data-conversion-suite (vega))
(defsuite encoding-suite (vega))
(defsuite commands-suite (vega))
(defsuite representation-suite (vega))
(defsuite authoring-suite (vega))
(defsuite registry-suite (vega))

;;; Utility: parse JSON string to hash-table for order-independent comparison
(defun parse-json (json-string)
  "Parse a JSON string into a nested hash-table structure using Yason."
  (yason:parse json-string))

;;; Utility: load a fixture file and return its contents as a string
(defun load-fixture (fixture-name)
  "Load a JSON fixture file from tests/fixtures/ and return as a string."
  (let ((path (asdf:system-relative-pathname "plot/vega/tests"
                                             (format nil "tests/fixtures/~A" fixture-name))))
    (uiop:read-file-string path)))

;;; Utility: string-key plist accessor (getf uses EQ, fails for strings)
(defun getf-string (plist key)
  "Like GETF but uses STRING= for key comparison."
  (loop for (k v) on plist by #'cddr
        when (and (stringp k) (string= k key))
          return v))

(defun package-external-symbol-names (package)
  "Return the sorted list of external symbol names exported by PACKAGE."
  (sort (loop for symbol being the external-symbols of package
              collect (symbol-name symbol))
        #'string<))

(defun clear-plot-if-present (name)
  "Remove NAME from the registry if present."
  (unregister-plot name)
  nil)

;;; Utility: encode an object via the public Vega JSON helper
(defun encode-object-via-vega-helper (object)
  "Encode OBJECT through the public Vega JSON helper and return a JSON string."
  (with-output-to-string (s)
    (vega:encode-object-for-vega object s)))

;;; Utility: construct a mock gist for editor-url tests.
;;; NOTE: Uses internal constructors cl-gists.gist::%make-gist and
;;; cl-gists.history::%make-history.  If cl-gists renames or removes
;;; these, this helper is the single place to update.
(defun make-test-gist (&key id filename (version nil version-supplied-p))
  "Construct a mock cl-gists gist struct for testing.
When VERSION is supplied the gist includes a history entry."
  (let ((file (cl-gists:make-file :name filename)))
    (if version-supplied-p
        (cl-gists.gist::%make-gist :id id
				   :files (list file)
				   :history (list (cl-gists.history::%make-history :version version)))
        (cl-gists.gist::%make-gist :id id
				   :files (list file)))))

;;; Seed test — validates the harness works
(deftest harness-sanity (vega)
  "Validates that the test harness loads and runs."
  (assert-true t))


;;;
;;; merge-plists tests
;;;

(deftest merge-plists-simple (merge-plists-suite)
  "Simple merge: later values override earlier ones."
  (let ((result (vega::merge-plists '(:a 1 :b 2) '(:b 3 :c 4))))
    (assert-eql 1 (getf result :a))
    (assert-eql 3 (getf result :b))
    (assert-eql 4 (getf result :c))))

(deftest merge-plists-recursive (merge-plists-suite)
  "Recursive merge: nested plists are merged recursively."
  (let* ((result (vega::merge-plists '(:a (:x 1 :y 2))
                                     '(:a (:y 3 :z 4))))
         (nested (getf result :a)))
    (assert-eql 1 (getf nested :x))
    (assert-eql 3 (getf nested :y))
    (assert-eql 4 (getf nested :z))))

(deftest merge-plists-empty-override (merge-plists-suite)
  "Empty override: base is returned unchanged."
  (let ((result (vega::merge-plists '(:a 1 :b 2))))
    (assert-eql 1 (getf result :a))
    (assert-eql 2 (getf result :b))))

(deftest merge-plists-multiple-overrides (merge-plists-suite)
  "Multiple overrides: each successive override takes precedence."
  (let ((result (vega::merge-plists '(:a 1 :b 2)
                                    '(:b 10)
                                    '(:b 20 :c 30))))
    (assert-eql 1  (getf result :a))
    (assert-eql 20 (getf result :b))
    (assert-eql 30 (getf result :c))))

(deftest merge-plists-non-plist-override (merge-plists-suite)
  "Non-plist value replaces plist value entirely."
  (let ((result (vega::merge-plists '(:a (:x 1)) '(:a "string"))))
    (assert-equal "string" (getf result :a))))


;;;
;;; write-spec tests
;;;

(deftest write-spec-simple-bar-chart (write-spec-suite)
  "write-spec produces correct JSON for a simple bar chart."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 28)
                                  (:a "B" :b 55)
                                  (:a "C" :b 43)
                                  (:a "D" :b 91)
                                  (:a "E" :b 81)
                                  (:a "F" :b 53)
                                  (:a "G" :b 19)
                                  (:a "H" :b 87)
                                  (:a "I" :b 52)))
                 :encoding (:x (:field :a :type :nominal
                                :axis (:label-angle 0))
                            :y (:field :b :type :quantitative))))
         (plot (make-plot :base spec :name 'test-bar))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "simple-bar-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-grouped-bar-chart (write-spec-suite)
  "write-spec produces correct JSON for a grouped bar chart."
  (let* ((spec '(:mark :bar
                 :data (:values #((:category "A" :group "x" :value 0.1d0)
                                  (:category "A" :group "y" :value 0.6d0)
                                  (:category "A" :group "z" :value 0.9d0)
                                  (:category "B" :group "x" :value 0.7d0)
                                  (:category "B" :group "y" :value 0.2d0)
                                  (:category "B" :group "z" :value 1.1d0)
                                  (:category "C" :group "x" :value 0.6d0)
                                  (:category "C" :group "y" :value 0.1d0)
                                  (:category "C" :group "z" :value 0.2d0)))
                 :encoding (:x (:field :category)
                            :y (:field :value :type :quantitative)
                            :x-offset (:field :group)
                            :color (:field :group))))
         (plot (make-plot :base spec :name 'test-grouped))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "grouped-bar-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-pie-chart (write-spec-suite)
  "write-spec produces correct JSON for a pie chart."
  (let* ((spec '(:data (:values #((:category 1.0d0 :value 4)
                                  (:category 2.0d0 :value 6)
                                  (:category 3.0d0 :value 10)
                                  (:category 4.0d0 :value 3)
                                  (:category 5.0d0 :value 7)
                                  (:category 6.0d0 :value 8)))
                 :mark :arc
                 :encoding (:theta (:field :value :type :quantitative)
                            :color (:field :category :type :nominal))))
         (plot (make-plot :base spec :name 'test-pie))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "pie-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-donut-chart (write-spec-suite)
  "write-spec produces correct JSON for a donut chart."
  (let* ((spec '(:data (:values #((:category 1.0d0 :value 4)
                                  (:category 2.0d0 :value 6)
                                  (:category 3.0d0 :value 10)
                                  (:category 4.0d0 :value 3)
                                  (:category 5.0d0 :value 7)
                                  (:category 6.0d0 :value 8)))
                 :mark (:type :arc :inner-radius 50)
                 :encoding (:theta (:field :value :type :quantitative)
                            :color (:field :category :type :nominal))))
         (plot (make-plot :base spec :name 'test-donut))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "donut-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-url-data (write-spec-suite)
  "write-spec produces correct JSON for a spec with URL data."
  (let* ((spec `(:data (:url ,(quri:uri "https://raw.githubusercontent.com/vega/vega-datasets/master/data/cars.json"))
                 :mark :point
                 :encoding (:x (:field "horsepower" :type :quantitative)
                            :y (:field "miles-per-gallon" :type :quantitative))))
         (plot (make-plot :base spec :name 'test-url))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "url-data-spec.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-returns-string (write-spec-suite)
  "write-spec with no keyword args returns a string."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (make-plot :base spec :name 'test-string))
         (result (vega::write-spec plot)))
    (assert-true (stringp result))))

(deftest write-spec-has-schema (write-spec-suite)
  "write-spec output includes $schema with v6 URL."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (make-plot :base spec :name 'test-schema))
         (parsed (parse-json (vega::write-spec plot))))
    (assert-equal "https://vega.github.io/schema/vega-lite/v6.json"
                         (values (gethash "$schema" parsed)))))

(deftest representation-text-matches-print-object (representation-suite)
  "plot:representation with :text returns the printed plot representation."
  (let* ((spec '(:mark :bar
                 :description "Simple bar chart"
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (make-plot :base spec :name 'test-text))
         (expected (with-output-to-string (stream)
                     (let ((*print-escape* t))
                       (write plot :stream stream))))
         (actual (representation plot :text)))
    (assert-equal expected actual)))

(deftest representation-vega-lite-reuses-write-spec (representation-suite)
  "plot:representation with :vega-lite returns the same JSON as write-spec."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (make-plot :base spec :name 'test-vega-lite)))
    (assert-equalp (parse-json (vega::write-spec plot))
                   (parse-json (representation plot :vega-lite)))))

(deftest mime-representation-includes-text-and-vega-lite (representation-suite)
  "plot:mime-representation returns text/plain and Vega-Lite MIME payloads."
  (let* ((spec '(:mark :bar
                 :description "Simple bar chart"
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (make-plot :base spec :name 'test-mime))
         (bundle (mime-representation plot))
         (data (cdr bundle))
         (expected-text (representation plot :text))
         (expected-spec (parse-json (representation plot :vega-lite))))
    (assert-equal expected-text (getf-string data "text/plain"))
    (assert-equalp expected-spec
                   (getf-string data "application/vnd.vegalite.v6+json"))
    (assert-equalp expected-spec
                   (getf-string data "application/vnd.vegalite.v4+json"))))


;;;
;;; editor-url tests
;;;

(deftest editor-url-with-history (commands-suite)
  "editor-url constructs correct URL with history version."
  (let ((gist (make-test-gist :id "abc123" :filename "plot.json" :version "v1")))
    (assert-equal "https://vega.github.io/editor/#/gist/abc123/v1/plot.json"
                         (vega::editor-url gist))))

(deftest editor-url-without-history (commands-suite)
  "editor-url constructs correct URL without history."
  (let ((gist (make-test-gist :id "def456" :filename "chart.json")))
    (assert-equal "https://vega.github.io/editor/#/gist/def456/chart.json"
                         (vega::editor-url gist))))


;;;
;;; encoding-suite tests — encode-symbol-as-metadata
;;;

(deftest encode-normal-keyword (encoding-suite)
  "Normal keywords encode as lowercase strings."
  (let ((json (encode-object-via-vega-helper (list :test :bar))))
    (assert-true (search "\"bar\"" json))))

(deftest encode-camel-case (encoding-suite)
  "Hyphenated keywords encode as camelCase."
  (let ((json (encode-object-via-vega-helper (list :test :inner-radius))))
    (assert-true (search "\"innerRadius\"" json))))

(deftest encode-camel-case-x-offset (encoding-suite)
  "x-offset encodes as xOffset via camelCase."
  (let ((json (encode-object-via-vega-helper (list :test :x-offset))))
    (assert-true (search "\"xOffset\"" json))))

(deftest encode-na-as-null (encoding-suite)
  "The symbol NA encodes as JSON null."
  (let ((json (encode-object-via-vega-helper (list :test 'na))))
    (assert-true (search "null" json))))

(deftest encode-false-as-false (encoding-suite)
  "The symbol FALSE encodes as JSON false."
  (let ((json (encode-object-via-vega-helper (list :test 'false))))
    (assert-true (search "false" json))))

(deftest encode-symbol-with-type-metadata (encoding-suite)
  "A symbol with :type property emits type metadata in JSON."
  (let ((sym (make-symbol "TEMPERATURE")))
    (setf (get sym :type) :double-float)
    (unwind-protect
         (let ((json (encode-object-via-vega-helper (list :test sym))))
           (assert-true (search "\"quantitative\"" json))
           (assert-true (search "\"temperature\"" json)))
      (remprop sym :type))))

(deftest encode-symbol-with-label-metadata (encoding-suite)
  "A symbol with :type and :label properties emits both in JSON."
  (let ((sym (make-symbol "SPEED")))
    (setf (get sym :type) :double-float)
    (setf (get sym :label) "Speed (km/h)")
    (unwind-protect
         (let ((json (encode-object-via-vega-helper (list :test sym))))
           (assert-true (search "\"quantitative\"" json))
           (assert-true (search "\"Speed (km/h)\"" json)))
      (remprop sym :type)
      (remprop sym :label))))

(deftest encode-symbol-with-unit-no-label (encoding-suite)
  "A symbol with :unit but no :label uses unit as title."
  (let ((sym (make-symbol "WEIGHT")))
    (setf (get sym :unit) "kg")
    (unwind-protect
         (let ((json (encode-object-via-vega-helper (list :test sym))))
           (assert-true (search "\"kg\"" json)))
      (remprop sym :unit))))

(deftest encode-object-for-vega-na-data-frame (encoding-suite)
  "encode-object-for-vega preserves :NA as JSON null for data-frame output."
  (let* ((df (df:make-df '(:name :value)
                         (list #("A" "B")
                               (make-array 2 :initial-contents '(1 :na)))))
         (json (encode-object-via-vega-helper df)))
    (assert-true (search "null" json))))


;;;
;;; data-conversion-suite tests — df-to-vl-plist
;;;

(deftest df-to-vl-plist-basic (data-conversion-suite)
  "df-to-vl-plist converts a data-frame to a vector of row plists."
  (let* ((df (df:make-df '(:a :b) (list #("X" "Y") #(1 2))))
         (result (df-to-vl-plist df)))
    (assert-true (vectorp result))
    (assert-eql 2 (length result))))

(deftest df-to-vl-plist-row-content (data-conversion-suite)
  "df-to-vl-plist row plists contain correct key-value pairs."
  (let* ((df (df:make-df '(:col-a :col-b) (list #("hello") #(42))))
         (result (df-to-vl-plist df))
         (row (aref result 0)))
    ;; ps:symbol-to-js-string converts :col-a → "colA", :col-b → "colB"
    ;; Row plists have string keys, so use getf-string
    (assert-equal "hello" (getf-string row "colA"))
    (assert-eql 42 (getf-string row "colB"))))

(deftest df-to-vl-plist-preserves-row-count (data-conversion-suite)
  "df-to-vl-plist produces one plist per data-frame row."
  (let* ((df (df:make-df '(:x) (list #(10 20 30 40 50))))
         (result (df-to-vl-plist df)))
    (assert-eql 5 (length result))))


;;;
;;; commands-suite — make-plot constructor
;;;

(deftest make-plot-name-only-remains-legacy-compatible (commands-suite)
  "Legacy positional make-plot with NAME only remains a compatibility path and does not auto-register."
  (clear-plot-if-present "TEST-DEFAULT")
  (let ((p (make-plot "test-default")))
    (unwind-protect
         (progn
           (assert-equal "test-default" (plot-name p))
           (assert-false (plot-data p))
           ;; Default spec has string key "$schema"; use getf-string
           (assert-equal "https://vega.github.io/schema/vega-lite/v6.json"
                         (getf-string (plot-spec p) "$schema"))
           (assert-false (find-plot "TEST-DEFAULT")))
      (clear-plot-if-present "TEST-DEFAULT"))))

(deftest make-plot-with-data-and-spec-remains-legacy-compatible (commands-suite)
  "Legacy positional make-plot with NAME, DATA, and SPEC preserves the old separate slot shape and does not auto-register."
  (clear-plot-if-present "TEST-FULL")
  (let* ((data '(:values #((:a 1))))
         (spec '(:mark :bar))
         (p (make-plot "test-full" data spec)))
    (unwind-protect
         (progn
           (assert-equal "test-full" (plot-name p))
           (assert-equalp data (plot-data p))
           (assert-equalp spec (plot-spec p))
           (assert-false (find-plot "TEST-FULL")))
      (clear-plot-if-present "TEST-FULL"))))

(deftest make-plot-with-data-and-default-spec-remains-legacy-compatible (commands-suite)
  "Legacy positional make-plot with NAME and DATA still preserves separate data/spec slots and does not auto-register."
  (clear-plot-if-present "TEST-WITH-DATA")
  (let* ((data '(:values #((:a 1))))
         (p (make-plot "test-with-data" data)))
    (unwind-protect
         (progn
           (assert-equal "test-with-data" (plot-name p))
           (assert-equalp data (plot-data p))
           (assert-equal "https://vega.github.io/schema/vega-lite/v6.json"
                         (getf-string (plot-spec p) "$schema"))
           (assert-false (find-plot "TEST-WITH-DATA")))
      (clear-plot-if-present "TEST-WITH-DATA"))))

(deftest make-plot-legacy-positional-forms-do-not-display-implicitly (commands-suite)
  "Legacy positional make-plot compatibility forms remain construction-only and do not invoke PLOT:PLOT."
  (let ((display-called nil)
        (original-plot-function (symbol-function 'plot:plot)))
    (unwind-protect
         (progn
           (setf (symbol-function 'plot:plot)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf display-called t)
                   :display-called))
           (make-plot "legacy-name-only")
           (make-plot "legacy-with-data" '(:values #((:a 1))))
           (make-plot "legacy-with-data-and-spec"
                      '(:values #((:a 1)))
                      '(:mark :bar))
           (assert-false display-called))
      (setf (symbol-function 'plot:plot) original-plot-function))))

(deftest make-plot-legacy-positional-forms-signal-deprecation-warning (commands-suite)
  "Legacy positional make-plot compatibility forms signal one deprecation warning directing callers to explicit MAKE-PLOT :BASE."
  (let ((warning nil)
        (warning-count 0))
    (handler-bind ((vega::make-plot-legacy-positional-deprecated-warning
                     (lambda (condition)
                       (incf warning-count)
                       (setf warning condition)
                       (muffle-warning))))
      (let ((vega::*make-plot-legacy-positional-deprecation-warning-issued-p* nil))
        (make-plot "legacy-warning-name-only")
        (make-plot "legacy-warning-with-data" '(:values #((:a 1))))
        (make-plot "legacy-warning-with-data-and-spec"
                   '(:values #((:a 1)))
                   '(:mark :bar))))
    (assert-eql 1 warning-count)
    (assert-true warning)
    (assert-true (search "MAKE-PLOT" (string-upcase (princ-to-string warning))))
    (assert-true (search ":BASE" (string-upcase (princ-to-string warning))))))

(deftest make-plot-base-contract-constructs-plot (commands-suite)
  "make-plot accepts an explicit :base contract for advanced lower-level construction."
  (let* ((base '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (p (make-plot :base base)))
    (assert-false (plot-name p))
    (assert-equalp '(:values #((:a "A" :b 1))) (plot-data p))
    (assert-equal :bar (getf (plot-spec p) :mark))))

(deftest make-plot-base-contract-name-option-normalizes-name (commands-suite)
  "make-plot accepts :name on the explicit :base path and still does not register the plot."
  (clear-plot-if-present "BASE-CONTRACT-NAMED")
  (let* ((base '(:mark :line
                 :data (:values #((:x 1 :y 2) (:x 2 :y 3)))
                 :encoding (:x (:field :x) :y (:field :y))))
         (p (make-plot :base base :name "base-contract-named")))
    (unwind-protect
         (progn
           (assert-equal "BASE-CONTRACT-NAMED" (plot-name p))
           (assert-equalp '(:values #((:x 1 :y 2) (:x 2 :y 3))) (plot-data p))
           (assert-equal :line (getf (plot-spec p) :mark))
           (assert-false (find-plot "BASE-CONTRACT-NAMED")))
      (clear-plot-if-present "BASE-CONTRACT-NAMED"))))

(deftest make-plot-base-overlay-contract-merges-overlay-fragments (commands-suite)
  "make-plot accepts explicit :overlay fragments only on top of :base and merges them into the resulting plot."
  (let* ((base '(:title "Base Title"
                 :mark :bar
                 :data (:values #((:x 1 :y 2 :group "A")
                                  (:x 2 :y 3 :group "B")))
                 :encoding (:x (:field :x)
                            :y (:field :y))))
         (overlay '((:title "Overlay Title")
                    (:mark :point)
                    (:encoding (:color (:field :group)))))
         (p (make-plot :base base :overlay overlay))
         (encoding (getf (plot-spec p) :encoding)))
    (assert-false (plot-name p))
    (assert-equalp '(:values #((:x 1 :y 2 :group "A")
                               (:x 2 :y 3 :group "B")))
                   (plot-data p))
    (assert-equal "Overlay Title" (getf (plot-spec p) :title))
    (assert-eql :point (getf (plot-spec p) :mark))
    (assert-true (getf encoding :x))
    (assert-true (getf encoding :y))
    (assert-true (getf encoding :color))))

(deftest make-plot-base-name-overlay-contract-normalizes-name (commands-suite)
  "make-plot accepts :name and :overlay together on the explicit :base path."
  (clear-plot-if-present "BASE-OVERLAY-NAMED")
  (let* ((base '(:title "Base"
                 :mark :line
                 :data (:values #((:x 1 :y 2) (:x 2 :y 4)))
                 :encoding (:x (:field :x)
                            :y (:field :y))))
         (overlay '((:title "Named Overlay")
                    (:encoding (:tooltip (:field :y)))))
         (p (make-plot :base base :name "base-overlay-named" :overlay overlay))
         (encoding (getf (plot-spec p) :encoding)))
    (unwind-protect
         (progn
           (assert-equal "BASE-OVERLAY-NAMED" (plot-name p))
           (assert-equal "Named Overlay" (getf (plot-spec p) :title))
           (assert-true (getf encoding :tooltip))
           (assert-false (find-plot "BASE-OVERLAY-NAMED")))
      (clear-plot-if-present "BASE-OVERLAY-NAMED"))))

(deftest make-plot-high-level-fragments-construct-plot (commands-suite)
  "make-plot accepts data plus high-level fragments on the new recommended path."
  (let* ((data #((:x 1 :y 2) (:x 2 :y 3)))
         (p (make-plot data
                       '(:title "High-Level Constructor")
                       '(:mark :point)
                       '(:encoding (:x (:field :x)
                                    :y (:field :y))))))
    (assert-false (plot-name p))
    (assert-equalp `(:values ,data) (plot-data p))
    (assert-equal "High-Level Constructor" (getf (plot-spec p) :title))
    (assert-eql :point (getf (plot-spec p) :mark))
    (assert-equalp `(:values ,data) (getf (plot-spec p) :data))))

(deftest make-plot-high-level-name-option-normalizes-name (commands-suite)
  "make-plot accepts :name on the high-level path and still does not register the plot."
  (clear-plot-if-present "HIGH-LEVEL-NAMED")
  (let* ((data #((:x 1 :y 2)))
         (p (make-plot data
                       :name "high-level-named"
                       '(:title "Named High-Level Constructor")
                       '(:mark :point)
                       '(:encoding (:x (:field :x)
                                    :y (:field :y))))))
    (unwind-protect
         (progn
           (assert-equal "HIGH-LEVEL-NAMED" (plot-name p))
           (assert-equalp `(:values ,data) (plot-data p))
           (assert-false (find-plot "HIGH-LEVEL-NAMED")))
      (clear-plot-if-present "HIGH-LEVEL-NAMED"))))

(deftest authoring-package-surfaces-match-plot-owned-entry-points (authoring-suite)
  "plot/vega loads GG and GEOM with the exact intended public authoring surfaces."
  (flet ((assert-package-surface (package-name expected-symbol-names)
           (let ((package (find-package package-name)))
             (assert-true package)
             (assert-equalp (sort (copy-list expected-symbol-names) #'string<)
                            (package-external-symbol-names package))
             (dolist (name expected-symbol-names)
               (multiple-value-bind (symbol status)
                   (find-symbol name package)
                 (assert-true symbol)
                 (assert-eql :external status)
                 (assert-true (fboundp symbol)))))))
    (assert-package-surface :gg
                            '("LABEL" "AXES" "COORD" "THEME" "TOOLTIP" "LAYER"))
    (assert-package-surface :geom
                            '("HISTOGRAM" "BAR" "POINT" "LINE" "BOX-PLOT"
                              "ERROR-BAR" "FUNC" "LOESS"))))

(deftest gg-fragments-compose-through-make-plot (authoring-suite)
  "The migrated GG fragment layer composes through plot-owned MAKE-PLOT on the high-level path."
  (let* ((data #((:x 1 :y 2 :group "A")
                 (:x 2 :y 3 :group "B")))
         (plot (make-plot data
                          '(:title "GG Composition")
                          '(:mark (:type :point :filled t))
                          '(:encoding (:x (:field :x :type :quantitative)
                                       :y (:field :y :type :quantitative)))
                          (gg:label :x "X Axis" :y "Y Axis")
                          (gg:axes :x-domain #(0 10)
                                   :y-domain #(0 10)
                                   :color-scheme :viridis)
                          (gg:tooltip :x '(:y :quantitative))
                          (gg:coord :x-domain #(0 5))
                          (gg:theme :width 420
                                    :height 240
                                    :font "IBM Plex Sans"
                                    :background "#f8f4ec"))))
    (let* ((spec (plot-spec plot))
           (mark (getf spec :mark))
           (encoding (getf spec :encoding))
           (x-encoding (getf encoding :x))
           (y-encoding (getf encoding :y))
           (color-encoding (getf encoding :color))
           (tooltip-encoding (getf encoding :tooltip))
           (config (getf spec :config)))
      (assert-equalp `(:values ,data) (plot-data plot))
      (assert-equal "GG Composition" (getf spec :title))
      (assert-eql :point (getf mark :type))
      (assert-true (getf mark :filled))
      (assert-true (getf mark :clip))
      (assert-equal "X Axis" (getf x-encoding :title))
      (assert-equal "Y Axis" (getf y-encoding :title))
      (assert-equalp #(0 5) (getf (getf x-encoding :scale) :domain))
      (assert-equalp #(0 10) (getf (getf y-encoding :scale) :domain))
      (assert-equal :viridis (getf (getf color-encoding :scale) :scheme))
      (assert-true (vectorp tooltip-encoding))
      (assert-eql 2 (length tooltip-encoding))
      (assert-equal 420 (getf spec :width))
      (assert-equal 240 (getf spec :height))
      (assert-equal "#f8f4ec" (getf spec :background))
      (assert-equal "IBM Plex Sans" (getf config :font)))))

(deftest gg-layer-composes-through-make-plot-without-display (authoring-suite)
  "The migrated GG layer helper stays construction-only and does not invoke PLOT:PLOT."
  (let ((display-called nil)
        (original-plot-function (symbol-function 'plot:plot))
        (data #((:x 1 :y 2) (:x 2 :y 4))))
    (unwind-protect
         (progn
           (setf (symbol-function 'plot:plot)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf display-called t)
                   :display-called))
           (let* ((plot (make-plot data
                                   '(:title "Layered GG")
                                   (gg:layer
                                    '(:mark (:type :point :filled t)
                                      :encoding (:x (:field :x :type :quantitative)
                                                 :y (:field :y :type :quantitative)))
                                    '(:mark :line
                                      :encoding (:x (:field :x :type :quantitative)
                                                 :y (:field :y :type :quantitative))))))
                  (spec (plot-spec plot))
                  (layers (getf spec :layer)))
             (assert-false display-called)
             (assert-equalp `(:values ,data) (plot-data plot))
             (assert-equalp `(:values ,data) (getf spec :data))
             (assert-true (vectorp layers))
             (assert-eql 2 (length layers))
             (assert-eql :point (getf (getf (aref layers 0) :mark) :type))
             (assert-eql :line (getf (aref layers 1) :mark))))
      (setf (symbol-function 'plot:plot) original-plot-function))))

(deftest geom-point-fragment-composes-through-make-plot (authoring-suite)
  "The migrated GEOM point helper composes through plot-owned MAKE-PLOT."
  (let* ((data #((:x 1 :y 2 :origin "A" :size 10)
                 (:x 2 :y 3 :origin "B" :size 20)))
         (plot (make-plot data
                          '(:title "Geom Point")
                          (geom:point :x :y
                                      :color :origin
                                      :shape "diamond"
                                      :size :size
                                      :opacity 0.7
                                      :filled t
                                      :zero-scale nil)))
         (spec (plot-spec plot))
         (mark (getf spec :mark))
         (encoding (getf spec :encoding)))
    (assert-equalp `(:values ,data) (plot-data plot))
    (assert-equal "Geom Point" (getf spec :title))
    (assert-eql :point (getf mark :type))
    (assert-true (getf mark :filled))
    (assert-equal "diamond" (getf mark :shape))
    (assert-equal :origin (getf (getf encoding :color) :field))
    (assert-equal :size (getf (getf encoding :size) :field))
    (assert-equal 0.7 (getf (getf encoding :opacity) :value))
    (assert-eql :false (getf (getf (getf encoding :x) :scale) :zero))
    (assert-eql :false (getf (getf (getf encoding :y) :scale) :zero))))

(deftest geom-line-fragment-composes-through-make-plot (authoring-suite)
  "The migrated GEOM line helper preserves the public line fragment contract."
  (let* ((data #((:date "2024-01-01" :price 10 :series "A" :weight 1)
                 (:date "2024-01-02" :price 12 :series "B" :weight 2)))
         (plot (make-plot data
                          '(:title "Geom Line")
                          (geom:line :date :price
                                     :x-type :temporal
                                     :color :series
                                     :size :weight
                                     :opacity 0.5
                                     :interpolate :monotone
                                     :point t
                                     :aggregate :mean
                                     :order :date)))
         (spec (plot-spec plot))
         (mark (getf spec :mark))
         (encoding (getf spec :encoding)))
    (assert-eql :line (getf mark :type))
    (assert-true (getf mark :point))
    (assert-eql :monotone (getf mark :interpolate))
    (assert-eql :temporal (getf (getf encoding :x) :type))
    (assert-eql :mean (getf (getf encoding :y) :aggregate))
    (assert-equal :series (getf (getf encoding :color) :field))
    (assert-equal :weight (getf (getf encoding :size) :field))
    (assert-equal 0.5 (getf (getf encoding :opacity) :value))
    (assert-equal :date (getf (getf encoding :order) :field))))

(deftest geom-bar-and-histogram-fragments-compose-through-make-plot (authoring-suite)
  "The migrated GEOM bar and histogram helpers preserve orientation and grouping behavior."
  (let* ((bar-data #((:category "A" :value 10 :series "S1")
                     (:category "B" :value 12 :series "S2")))
         (bar-plot (make-plot bar-data
                              (geom:bar :category :value
                                        :orient :horizontal
                                        :group :series
                                        :stack :normalize
                                        :opacity 0.4
                                        :corner-radius-end 3)))
         (bar-spec (plot-spec bar-plot))
         (bar-mark (getf bar-spec :mark))
         (bar-encoding (getf bar-spec :encoding))
         (hist-data #((:value 1 :series "S1")
                      (:value 2 :series "S2")))
         (hist-plot (make-plot hist-data
                               (geom:histogram :value
                                               :group :series
                                               :stack :null
                                               :opacity 0.25
                                               :bin-spacing 0
                                               :corner-radius-end 5)))
         (hist-spec (plot-spec hist-plot))
         (hist-mark (getf hist-spec :mark))
         (hist-encoding (getf hist-spec :encoding)))
    (assert-eql :bar (getf bar-mark :type))
    (assert-eql 3 (getf bar-mark :corner-radius-end))
    (assert-eql :value (getf (getf bar-encoding :x) :field))
    (assert-eql :normalize (getf (getf bar-encoding :x) :stack))
    (assert-eql :category (getf (getf bar-encoding :y) :field))
    (assert-eql :series (getf (getf bar-encoding :color) :field))
    (assert-equal 0.4 (getf (getf bar-encoding :opacity) :value))
    (assert-eql :bar (getf hist-mark :type))
    (assert-eql 0 (getf hist-mark :bin-spacing))
    (assert-eql 5 (getf hist-mark :corner-radius-end))
    (assert-eql :value (getf (getf hist-encoding :x) :field))
    (assert-eql :count (getf (getf hist-encoding :y) :aggregate))
    (assert-eql :null (getf (getf hist-encoding :y) :stack))
    (assert-eql :series (getf (getf hist-encoding :color) :field))
    (assert-equal 0.25 (getf (getf hist-encoding :opacity) :value))))

(deftest geom-box-plot-and-error-bar-fragments-compose-through-make-plot (authoring-suite)
  "The migrated GEOM box-plot and error-bar helpers preserve category and legend behavior."
  (let* ((box-data #((:value 10 :group "A")
                     (:value 12 :group "B")))
         (box-plot (make-plot box-data
                              (geom:box-plot :value
                                             :category :group
                                             :opacity 0.6)))
         (box-spec (plot-spec box-plot))
         (box-mark (getf box-spec :mark))
         (box-encoding (getf box-spec :encoding))
         (error-data #((:value 10 :group "A")
                       (:value 12 :group "B")))
         (error-plot (make-plot error-data
                                (geom:error-bar :value
                                                :orient :horizontal
                                                :category :group
                                                :opacity 0.3
                                                :thickness 2
                                                :ticks t)))
         (error-spec (plot-spec error-plot))
         (error-mark (getf error-spec :mark))
         (error-encoding (getf error-spec :encoding)))
    (assert-eql :boxplot (getf box-mark :type))
    (assert-eql 1.5 (getf box-mark :extent))
    (assert-eql :value (getf (getf box-encoding :x) :field))
    (assert-eql :false (getf (getf (getf box-encoding :x) :scale) :zero))
    (assert-eql :group (getf (getf box-encoding :color) :field))
    (assert-eql :null (getf (getf box-encoding :color) :legend))
    (assert-equal 0.6 (getf (getf box-encoding :opacity) :value))
    (assert-eql :errorbar (getf error-mark :type))
    (assert-eql :stdev (getf error-mark :extent))
    (assert-true (getf error-mark :ticks))
    (assert-eql 2 (getf error-mark :thickness))
    (assert-eql :value (getf (getf error-encoding :x) :field))
    (assert-eql :group (getf (getf error-encoding :y) :field))
    (assert-eql :group (getf (getf error-encoding :color) :field))
    (assert-eql :null (getf (getf error-encoding :color) :legend))
    (assert-equal 0.3 (getf (getf error-encoding :opacity) :value))))

(deftest geom-and-gg-compose-through-make-plot (authoring-suite)
  "The migrated GEOM and plot-owned GG layers compose together through MAKE-PLOT."
  (let* ((data #((:x 1 :y 2 :group "A")
                 (:x 2 :y 3 :group "B")))
         (plot (make-plot data
                          '(:title "Geom + GG Composition")
                          (geom:point :x :y :color :group :filled t)
                          (gg:label :x "X" :y "Y")
                          (gg:theme :width 360 :height 220)))
         (spec (plot-spec plot))
         (mark (getf spec :mark))
         (encoding (getf spec :encoding)))
    (assert-equal "Geom + GG Composition" (getf spec :title))
    (assert-eql :point (getf mark :type))
    (assert-true (getf mark :filled))
    (assert-eql :group (getf (getf encoding :color) :field))
    (assert-equal "X" (getf (getf encoding :x) :title))
    (assert-equal "Y" (getf (getf encoding :y) :title))
    (assert-equal 360 (getf spec :width))
    (assert-equal 220 (getf spec :height))))

(deftest geom-fragments-do-not-display-implicitly (authoring-suite)
  "The migrated GEOM fragment path remains construction-only and does not invoke PLOT:PLOT."
  (let ((display-called nil)
        (original-plot-function (symbol-function 'plot:plot)))
    (unwind-protect
         (progn
           (setf (symbol-function 'plot:plot)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf display-called t)
                   :display-called))
           (make-plot #((:x 1 :y 2))
                      '(:title "No Display Geom")
                      (geom:line :x :y))
           (assert-false display-called))
      (setf (symbol-function 'plot:plot) original-plot-function))))

(deftest geom-func-generates-inline-data-and-composes-with-gg (authoring-suite)
  "GEOM:FUNC remains a fragment helper that generates inline data and composes with plot-owned GG helpers."
  (let* ((plot (make-plot :base
                          (vega::merge-plists
                           '(:title "Geom Func")
                           (geom:func #'sin
                                      :xlim #(-1d0 1d0)
                                      :n 5
                                      :color "steelblue"
                                      :stroke-width 2
                                      :opacity 0.5)
                           (gg:label :x "x" :y "sin(x)")
                           (gg:theme :width 300 :height 180))))
         (spec (plot-spec plot))
         (mark (getf spec :mark))
         (encoding (getf spec :encoding))
         (data (getf (plot-data plot) :values)))
    (assert-equal "Geom Func" (getf spec :title))
    (assert-true (vectorp data))
    (assert-eql 5 (length data))
    (assert-eql :line (getf mark :type))
    (assert-eql :monotone (getf mark :interpolate))
    (assert-equal "steelblue" (getf mark :color))
    (assert-eql 2 (getf mark :stroke-width))
    (assert-equal 0.5 (getf mark :opacity))
    (assert-eql :x (getf (getf encoding :x) :field))
    (assert-eql :y (getf (getf encoding :y) :field))
    (assert-equal "x" (getf (getf encoding :x) :title))
    (assert-equal "sin(x)" (getf (getf encoding :y) :title))
    (assert-eql -1.0d0 (getf (aref data 0) :x))
    (assert-eql 1.0d0 (getf (aref data 4) :x))
    (assert-equal 300 (getf spec :width))
    (assert-equal 180 (getf spec :height))))

(deftest geom-func-drops-invalid-evaluation-points (authoring-suite)
  "GEOM:FUNC drops points whose evaluation signals an error."
  (let* ((plot (make-plot :base
                          (vega::merge-plists
                           '(:title "Geom Func Filtering")
                           (geom:func (lambda (x)
                                        (if (zerop x)
                                            (error "boom")
                                            x))
                                      :xlim #(-1d0 1d0)
                                      :n 5))))
         (data (getf (plot-data plot) :values)))
    (assert-true (vectorp data))
    (assert-eql 4 (length data))
    (assert-false (find 0.0d0 data :key (lambda (row) (getf row :x)) :test #'=))))

(deftest geom-finite-real-helper-rejects-infinity (authoring-suite)
  "The internal finite-real helper accepts ordinary finite numbers and rejects infinity."
  (assert-true (geom::%finite-real-p 1d0))
  #+sbcl
  (progn
    (assert-false (geom::%finite-real-p sb-ext:double-float-positive-infinity))
    (assert-false (geom::%finite-real-p sb-ext:double-float-negative-infinity))))

(deftest geom-loess-fragment-composes-through-make-plot (authoring-suite)
  "GEOM:LOESS remains a transform-oriented fragment helper that composes with plot-owned GG helpers."
  (let* ((data #((:x 1 :y 2 :group "A")
                 (:x 2 :y 4 :group "A")
                 (:x 3 :y 3 :group "B")))
         (plot (make-plot data
                          '(:title "Geom Loess")
                          (geom:loess :x :y
                                      :group :group
                                      :opacity 0.4
                                      :stroke-width 2
                                      :stroke-dash #(4 2))
                          (gg:label :x "X" :y "Y")
                          (gg:theme :width 420 :height 240)))
         (spec (plot-spec plot))
         (transform (getf spec :transform))
         (transform-entry (aref transform 0))
         (mark (getf spec :mark))
         (encoding (getf spec :encoding)))
    (assert-equalp `(:values ,data) (plot-data plot))
    (assert-true (vectorp transform))
    (assert-eql 1 (length transform))
    (assert-eql :y (getf transform-entry :loess))
    (assert-eql :x (getf transform-entry :on))
    (assert-equal 0.3 (getf transform-entry :bandwidth))
    (assert-equalp #(:group) (getf transform-entry :groupby))
    (assert-eql :line (getf mark :type))
    (assert-eql 2 (getf mark :stroke-width))
    (assert-equalp #(4 2) (getf mark :stroke-dash))
    (assert-equal 0.4 (getf mark :opacity))
    (assert-eql :quantitative (getf (getf encoding :x) :type))
    (assert-eql :quantitative (getf (getf encoding :y) :type))
    (assert-eql :group (getf (getf encoding :color) :field))
    (assert-equal "X" (getf (getf encoding :x) :title))
    (assert-equal "Y" (getf (getf encoding :y) :title))
    (assert-equal 420 (getf spec :width))
    (assert-equal 240 (getf spec :height))))

(deftest geom-advanced-helpers-do-not-display-implicitly (authoring-suite)
  "GEOM:FUNC and GEOM:LOESS remain construction-only helpers and do not invoke PLOT:PLOT."
  (let ((display-called nil)
        (original-plot-function (symbol-function 'plot:plot)))
    (unwind-protect
         (progn
           (setf (symbol-function 'plot:plot)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf display-called t)
                   :display-called))
           (make-plot :base
                      (vega::merge-plists
                       '(:title "No Display Func")
                       (geom:func #'sin :xlim #(-1d0 1d0) :n 5)))
           (make-plot #((:x 1 :y 2) (:x 2 :y 3))
                      '(:title "No Display Loess")
                      (geom:loess :x :y))
           (assert-false display-called))
      (setf (symbol-function 'plot:plot) original-plot-function))))

(deftest make-plot-unsupported-keyword-contract-fails-explicitly (commands-suite)
  "Unsupported top-level keyword contracts still fail explicitly."
  (assert-true
   (handler-case
       (progn
         (make-plot :overlay '((:title "Not Supported Here")))
         nil)
     (error () t))))

(deftest make-plot-from-spec-signals-deprecation-warning (commands-suite)
  "make-plot-from-spec signals a deprecation warning that directs callers to explicit MAKE-PLOT :BASE."
  (let ((warning nil))
    (handler-bind ((vega::make-plot-from-spec-deprecated-warning
                     (lambda (condition)
                       (setf warning condition)
                       (muffle-warning))))
      (let ((vega::*make-plot-from-spec-deprecation-warning-issued-p* nil))
        (make-plot-from-spec '(:mark :bar
                               :data (:values #((:a "A" :b 1)))
                               :encoding (:x (:field :a) :y (:field :b))))))
    (assert-true warning)
    (assert-true (search "MAKE-PLOT" (string-upcase (princ-to-string warning))))
    (assert-true (search ":BASE" (string-upcase (princ-to-string warning))))))

(deftest make-plot-from-spec-unnamed-construction (commands-suite)
  "make-plot-from-spec remains an unnamed compatibility wrapper over explicit :base construction."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (p (make-plot-from-spec spec)))
    (assert-false (plot-name p))
    (assert-equalp '(:values #((:a "A" :b 1))) (plot-data p))
    (assert-equal :bar (getf (plot-spec p) :mark))))

(deftest make-plot-from-spec-inserts-default-schema (commands-suite)
  "make-plot-from-spec preserves its schema-oriented compatibility behavior when absent from the base spec."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (p (make-plot-from-spec spec)))
    (assert-equal "https://vega.github.io/schema/vega-lite/v6.json"
                  (getf-string (plot-spec p) "$schema"))))

(deftest make-plot-from-spec-preserves-explicit-schema (commands-suite)
  "make-plot-from-spec preserves an explicit schema while remaining subordinate to MAKE-PLOT."
  (let* ((schema "https://example.com/custom-schema.json")
         (spec `("$schema" ,schema
                 :mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (p (make-plot-from-spec spec)))
    (assert-equal schema
                  (getf-string (plot-spec p) "$schema"))))

(deftest make-plot-from-spec-named-construction-remains-unregistered (commands-suite)
  "make-plot-from-spec remains a public compatibility constructor and does not auto-register named plots."
  (clear-plot-if-present "SPEC-COMPAT")
  (let* ((spec '(:mark :point
                 :data (:values #((:x 1 :y 2)))
                 :encoding (:x (:field :x) :y (:field :y))))
         (p (make-plot-from-spec spec :name "spec-compat")))
    (unwind-protect
         (progn
           (assert-equal "SPEC-COMPAT" (plot-name p))
           (assert-equalp '(:values #((:x 1 :y 2))) (plot-data p))
           (assert-false (find-plot "SPEC-COMPAT")))
      (clear-plot-if-present "SPEC-COMPAT"))))

(deftest make-plot-from-spec-matches-explicit-base-construction (commands-suite)
  "make-plot-from-spec remains a compatibility wrapper over explicit MAKE-PLOT :BASE construction."
  (clear-plot-if-present "SPEC-COMPAT-EQUIV")
  (let* ((template '(:mark :line
                     :data (:values #((:x 1 :y 2) (:x 2 :y 3)))
                     :encoding (:x (:field :x) :y (:field :y))))
         (compat-spec (copy-tree template))
         (base-spec (copy-tree template))
         (compat (make-plot-from-spec compat-spec :name "spec-compat-equiv"))
         (explicit (make-plot :base base-spec :name "spec-compat-equiv")))
    (unwind-protect
         (progn
           (assert-equal (plot-name explicit) (plot-name compat))
           (assert-equalp (plot-data explicit) (plot-data compat))
           (assert-equalp (plot-spec explicit) (plot-spec compat))
           (assert-false (find-plot "SPEC-COMPAT-EQUIV")))
      (clear-plot-if-present "SPEC-COMPAT-EQUIV"))))

(deftest make-plot-from-spec-schema-argument-remains-compatible (commands-suite)
  "make-plot-from-spec still honors its explicit schema argument while remaining an unregistered compatibility wrapper."
  (clear-plot-if-present "SPEC-COMPAT-SCHEMA")
  (let* ((schema "https://example.com/wrapper-schema.json")
         (spec '(:mark :point
                 :data (:values #((:x 1 :y 2)))
                 :encoding (:x (:field :x) :y (:field :y))))
         (p (make-plot-from-spec spec :name "spec-compat-schema" :schema schema)))
    (unwind-protect
         (progn
           (assert-equal "SPEC-COMPAT-SCHEMA" (plot-name p))
           (assert-equal schema (getf-string (plot-spec p) "$schema"))
           (assert-false (find-plot "SPEC-COMPAT-SCHEMA")))
      (clear-plot-if-present "SPEC-COMPAT-SCHEMA"))))

(deftest make-plot-from-spec-does-not-display-implicitly (commands-suite)
  "make-plot-from-spec remains construction-only and does not invoke PLOT:PLOT implicitly."
  (let ((display-called nil)
        (original-plot-function (symbol-function 'plot:plot)))
    (unwind-protect
         (progn
           (setf (symbol-function 'plot:plot)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf display-called t)
                   :display-called))
           (make-plot-from-spec '(:mark :bar
                                  :data (:values #((:a "A" :b 1)))
                                  :encoding (:x (:field :a) :y (:field :b))))
           (assert-false display-called))
      (setf (symbol-function 'plot:plot) original-plot-function))))

(deftest register-plot-adds-normalized-name (registry-suite)
  "register-plot stores a plot by normalized name and updates plot-name."
  (clear-plot-if-present "REGISTRY-TEST")
  (let* ((plot (make-plot-from-spec '(:mark :bar) :name "registry-test")))
    (unwind-protect
         (progn
           (assert-true (eq plot (register-plot plot)))
           (assert-equal "REGISTRY-TEST" (plot-name plot))
           (assert-true (eq plot (find-plot "registry-test")))
           (assert-true (eq plot (find-plot 'registry-test))))
      (clear-plot-if-present "REGISTRY-TEST"))))

(deftest list-plots-returns-sorted-registered-names (registry-suite)
  "list-plots returns sorted names from the registry."
  (clear-plot-if-present "ALPHA-PLOT")
  (clear-plot-if-present "BETA-PLOT")
  (let ((alpha (make-plot-from-spec '(:mark :bar) :name "alpha-plot"))
        (beta (make-plot-from-spec '(:mark :bar) :name "beta-plot")))
    (unwind-protect
         (progn
           (register-plot beta)
           (register-plot alpha)
           (assert-true (member "ALPHA-PLOT" (list-plots) :test #'string=))
           (assert-true (member "BETA-PLOT" (list-plots) :test #'string=))
           (assert-equal '("ALPHA-PLOT" "BETA-PLOT")
                         (remove-if-not (lambda (name)
                                          (member name '("ALPHA-PLOT" "BETA-PLOT")
                                                  :test #'string=))
                                        (list-plots))))
      (clear-plot-if-present "ALPHA-PLOT")
      (clear-plot-if-present "BETA-PLOT"))))

(deftest unregister-plot-removes-and-returns-plot (registry-suite)
  "unregister-plot removes the plot and returns it."
  (clear-plot-if-present "DELETE-PLOT")
  (let ((plot (register-plot (make-plot-from-spec '(:mark :bar) :name "delete-plot"))))
    (assert-true (eq plot (unregister-plot "delete-plot")))
    (assert-false (find-plot "delete-plot"))))

(deftest defplot-registers-via-runtime-api (registry-suite)
  "defplot defines a variable and leaves a discoverable registered plot."
  (let ((sym (intern "RUNTIME-DEFPLOT-TEST" (find-package :vega-tests))))
    (clear-plot-if-present sym)
    (when (boundp sym)
      (makunbound sym))
    (unwind-protect
         (progn
           (eval `(defplot ,sym
                    (:mark :bar
                     :data (:values #((:a "A" :b 1)))
                     :encoding (:x (:field :a) :y (:field :b)))))
           (assert-true (boundp sym))
           (assert-true (eq (symbol-value sym) (find-plot sym)))
           (assert-equal "RUNTIME-DEFPLOT-TEST"
                         (plot-name (symbol-value sym))))
      (clear-plot-if-present sym)
      (when (boundp sym)
        (makunbound sym)))))


;;;
;;; write-spec-suite — additional plot types (Wave 2)
;;;

(deftest write-spec-line-chart (write-spec-suite)
  "write-spec produces correct JSON for a line chart."
  (let* ((spec '(:mark :line
                 :data (:values #((:x 1 :y 10)
                                  (:x 2 :y 20)
                                  (:x 3 :y 15)))
                 :encoding (:x (:field :x :type :quantitative)
                            :y (:field :y :type :quantitative))))
         (plot (make-plot :base spec :name 'test-line))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "line-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-scatter-chart (write-spec-suite)
  "write-spec produces correct JSON for a scatter plot with inline data."
  (let* ((spec '(:mark :point
                 :data (:values #((:x 1 :y 5)
                                  (:x 2 :y 3)
                                  (:x 3 :y 7)))
                 :encoding (:x (:field :x :type :quantitative)
                            :y (:field :y :type :quantitative))))
         (plot (make-plot :base spec :name 'test-scatter))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "scatter-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-area-chart (write-spec-suite)
  "write-spec produces correct JSON for an area chart."
  (let* ((spec '(:mark :area
                 :data (:values #((:x 1 :y 10)
                                  (:x 2 :y 20)
                                  (:x 3 :y 15)))
                 :encoding (:x (:field :x :type :quantitative)
                            :y (:field :y :type :quantitative))))
         (plot (make-plot :base spec :name 'test-area))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "area-chart.json"))))
    (assert-equalp expected actual)))
